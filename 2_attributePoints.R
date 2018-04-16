# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

## start with a fresh workspace with no objects loaded
library(rgdal)
library(RSQLite)
library(maptools)

# load data, QC ----

###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 43 where you choose which random points file to use
setwd(loc_envVars)
EnvVars <- read.csv("EnvVars.csv", colClasses=c("huc12"="character")) 
names(EnvVars) <- tolower(names(EnvVars))

# join ev to reaches
# Set working directory to the random points location
setwd(loc_spReaches)
#get a list of what's in the directory
fileList <- dir( pattern = "_prepped.csv$")
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

fileName <- fileList[[n]]
shpName <- strsplit(fileName,"\\_prepped.")[[1]][[1]]
sppCode <- shpName
reaches <- read.csv(fileName)

# subset input env. vars by model type (terrestrial, shore, etc)
db <- dbConnect(SQLite(),dbname=nm_db_file)
# get MODTYPE
SQLQuery <- paste0("SELECT MODTYPE m FROM lkpSpecies WHERE CODE = '", sppCode, "';")
modType <- dbGetQuery(db, SQLQuery)$m

SQLQuery <- paste0("SELECT gridName g FROM lkpEnvVarsAqua WHERE use_",modType," = 1;")
gridlistSub <- dbGetQuery(db, SQLQuery)$g
dbDisconnect(db)

## account for add/remove vars
if (!is.null(add_vars)) {
  # get all aquatic vars (including ones marked use_A = 0)
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  SQLQuery <- paste0("SELECT gridName g FROM lkpEnvVarsAqua;")
  gridlistAll <- dbGetQuery(db, SQLQuery)$g
  dbDisconnect(db)
  
  if (!all(tolower(add_vars) %in% gridlistAll)) {
    stop("Some environmental variables listed in `add_vars` were not found in `loc_EnvVars` dataset: ",
         paste(add_vars[!tolower(add_vars) %in% gridlistSub], collapse = ", "), ".")
  }
  gridlistSub <- c(gridlistSub, add_vars)
}
if (!is.null(remove_vars)) {
  if (!all(tolower(remove_vars) %in% gridlistSub)) {
    warning("Some environmental variables listed in `remove_vars` were not found in the `loc_EnvVars` dataset: ",
            paste(remove_vars[!tolower(remove_vars) %in% gridlistSub], collapse = ", "), ".")
  } 
  gridlistSub <- gridlistSub[!tolower(gridlistSub) %in% tolower(remove_vars)]
}
# get desired env. var. columns + comid
EnvVars <- EnvVars[c("comid",gridlistSub)]
rm(gridlistSub, modType)

# merge two data frames by COMID
reaches_attributed <- merge(reaches,EnvVars,by="comid")

# write it out ----
write.csv(reaches_attributed, paste(sppCode,"_att.csv",sep=""), row.names = FALSE)