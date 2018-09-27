# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

## start with a fresh workspace with no objects loaded
library(rgdal)
library(RSQLite)
library(maptools)
library(stringr)

# load data, QC ----
###
EnvVars <- read.csv(nm_envVars, colClasses=c("huc12"="character")) 
names(EnvVars) <- tolower(names(EnvVars))

# join ev to reaches
# Set working directory to the prepped reaches location
setwd(paste0(loc_model, "/", model_species, "/inputs"))

fileName <- paste0("model_input/", baseName, "_prepped.csv")
reaches <- read.csv(fileName)

# subset input env. vars by model type (terrestrial, shore, etc)
db <- dbConnect(SQLite(),dbname=nm_db_file)
# get MODTYPE
SQLQuery <- paste0("SELECT MODTYPE m FROM lkpSpecies WHERE CODE = '", model_species, "';")
modType <- dbGetQuery(db, SQLQuery)$m

SQLQuery <- paste0("SELECT gridName g FROM lkpEnvVarsAqua WHERE use_",modType," = 1;")
gridlistSub <- tolower(dbGetQuery(db, SQLQuery)$g)
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
    stop("Some environmental variables listed in `add_vars` were not found in `nm_EnvVars` dataset: ",
         paste(add_vars1[!add_vars %in% gridlistSub], collapse = ", "), ".")
  }
  gridlistSub <- c(gridlistSub, add_vars)
}
if (!is.null(remove_vars)) {
  remove_vars1 <- remove_vars
  remove_vars <- tolower(remove_vars)
  if (!all(remove_vars %in% gridlistSub)) {
    message("Some environmental variables listed in `remove_vars` were not found in the `nm_EnvVars` dataset: ",
            paste(remove_vars1[!remove_vars %in% gridlistSub], collapse = ", "), ".")
  } 
  gridlistSub <- gridlistSub[!gridlistSub %in% remove_vars]
}
# get desired env. var. columns + comid
EnvVars <- EnvVars[c("comid",gridlistSub)]
rm(gridlistSub, modType)

# merge two data frames by COMID
reaches_attributed <- merge(reaches,EnvVars,by="comid")

# write it out ----
write.csv(reaches_attributed, paste0("model_input/",baseName,"_att.csv"), row.names = FALSE)
