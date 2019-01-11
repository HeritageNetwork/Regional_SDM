# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

## start with a fresh workspace with no objects loaded
library(sf)
library(RSQLite)
library(stringr)

# load data, QC ----
# Set working directory to the prepped reaches location
setwd(paste0(loc_model, "/", model_species, "/inputs"))

# load files
fileName <- paste0("presence/", baseName, "_prepped.shp")
reaches <- st_read(fileName, quiet = T)

fileName <- paste0("model_input/", baseName, "_bkgd_clean.shp")
bkgd.reaches <- st_read(fileName, quiet = T)

# subset input env. vars by model type (terrestrial, shore, etc)
db <- dbConnect(SQLite(),dbname=nm_db_file)
# get MODTYPE
SQLQuery <- paste0("SELECT MODTYPE m FROM lkpSpecies WHERE sp_code = '", model_species, "';")
modType <- dbGetQuery(db, SQLQuery)$m

SQLQuery <- paste0("SELECT gridName g FROM lkpEnvVarsAqua WHERE use_",modType," = 1;")
gridlistSub <- tolower(dbGetQuery(db, SQLQuery)$g)
gridlistSub <- gridlistSub[-c(1:2)]
dbDisconnect(db)

## account for add/remove vars
if (!is.null(add_vars)) {
  add_vars1 <- add_vars
  add_vars <- tolower(add_vars)

  # get all aquatic vars (including ones marked use_A = 0)
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  SQLQuery <- paste0("SELECT gridName g FROM lkpEnvVarsAqua;")
  gridlistAll <- tolower(dbGetQuery(db, SQLQuery)$g)
  dbDisconnect(db)
  
  if (!all(add_vars %in% gridlistAll)) {
    stop("Some environmental variables listed in `add_vars` were not found in `nm_bkg` dataset: ",
         paste(add_vars1[!add_vars %in% gridlistSub], collapse = ", "), ".")
  }
  gridlistSub <- c(gridlistSub, add_vars)
}
if (!is.null(remove_vars)) {
  remove_vars1 <- remove_vars
  remove_vars <- tolower(remove_vars)
  if (!all(remove_vars %in% gridlistSub)) {
    message("Some environmental variables listed in `remove_vars` were not found in the `nm_bkg` dataset: ",
            paste(remove_vars1[!remove_vars %in% gridlistSub], collapse = ", "), ".")
  } 
  gridlistSub <- gridlistSub[!gridlistSub %in% remove_vars]
}

# get desired env. var. columns + comid for presence
# SQLite database integration for Env Vars
dbEV <- dbConnect(SQLite(),dbname=nm_bkg[1])
SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2],"_att WHERE COMID IN ('", paste(reaches$comid, collapse = "','"),"')") 
EnvVars <- dbGetQuery(dbEV, SQLQuery)
names(EnvVars) <- tolower(names(EnvVars))
EnvVars <- EnvVars[c("comid",gridlistSub)]
dbDisconnect(dbEV)

# merge two data frames by COMID
reaches_attributed <- merge(reaches,EnvVars,by="comid")
reaches_attributed$geometry <- NULL
write.csv(reaches_attributed, paste0("model_input/",baseName,"_att.csv"), row.names = FALSE)

# get desired env. var. columns + comid for bkgd
# SQLite database integration for Env Vars
dbEV <- dbConnect(SQLite(),dbname=nm_bkg[1])
SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2],"_att WHERE COMID IN ('", paste(bkgd.reaches$comid, collapse = "','"),"')") 
EnvVars <- dbGetQuery(dbEV, SQLQuery)
names(EnvVars) <- tolower(names(EnvVars))
EnvVars <- EnvVars[c("comid",gridlistSub)]
dbDisconnect(dbEV)

bkgd.reaches_attributed <- merge(bkgd.reaches,EnvVars,by="comid")
bkgd.reaches_attributed$geometry <- NULL
write.csv(bkgd.reaches_attributed, paste0("model_input/",baseName,"_bkgd_att.csv"), row.names = FALSE)

# clean up
rm(gridlistSub, modType)

