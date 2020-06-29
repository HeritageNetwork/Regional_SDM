# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(here)
library(raster)
library(randomForest)
library(RSQLite)
library(sf)
# library(snow)
# library(parallel)

removeTmpFiles(48) # clean old (>2days) Raster temporary files

# load data ----
# get the rdata file
setwd(loc_model)
dir.create(paste0(model_species,"/outputs/model_predictions"), recursive = TRUE, showWarnings = FALSE)
setwd(paste0(model_species,"/outputs"))

# load rdata
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

# each algo in the ensemble uses a different set of env vars
#
# get the vars used for each
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelResultsVarsUsed where model_run_name = '", 
              model_run_name, "';")
varsImp <- dbGetQuery(db, statement = sql)
# get full file names
sql <- "SELECT gridName, fileName FROM lkpEnvVars"
evs <- dbGetQuery(db, statement = sql)
evs$gridName <- tolower(evs$gridName)
dbDisconnect(db)
rm(db)

# remove vars not used by any algo
varsImp <- varsImp[varsImp$inFinalModel == 1,]

# merge in full name, reduce cols and dups
rasFiles <- merge(varsImp, evs)
rasFiles <- unique(rasFiles[,c("gridName","fileName")])

##Make the raster stack
setwd(loc_envVars)

# find matching var rasters (with folder for temporal vars)
raslist <- list.files(pattern = ".tif$", recursive = TRUE)


fullL <- as.list(rasFiles$fileName)
names(fullL) <- rasFiles$gridName

# this loop handles temporal rasters but otherwise is not necessary. 
# keep it simple (as above two-liner does) till we need this (or improve it)
# attach file names to env var names
# fullL <- list()
# for (i in 1:nrow(rasFiles)) {
#   rs <- raslist[grep(rasFiles$fileName[i], raslist, ignore.case = TRUE)]
#   if (length(rs) > 1) {
#     # always take most recent temporal raster
#     rs1 <- do.call(rbind.data.frame, strsplit(rs, "_|/"))
#     rs1$nm <- rs
#     rs <- rs1$nm[which.max(as.numeric(rs1[,2]))]
#     rm(rs1)
#   }
#   fullL[[i]] <- rs
# }
# names(fullL) <- rasFiles$gridName
# rm(rs)


source(paste0(loc_scripts, "/helper/crop_mask_rast.R"), local = FALSE)
envStack <- stack(newL)

#envStack <- stack(fullL) # if not using helper/crop_mask_rast.R
rm(fullL)

#### predict! ###

for(algo in ensemble_algos){
  message(paste0("building out a prediction for the ", algo, " model."))
  scriptToCall <- paste0(algo, "_4_predictModelToStudyArea.R")
  source(here("ensemble", scriptToCall))
}


# delete temp rasts folder
if (dir.exists(paste0(options("rasterTmpDir")[1], "/", modelrun_meta_data$model_run_name))) {
  unlink(x = paste0(options("rasterTmpDir")[1], "/", modelrun_meta_data$model_run_name), recursive = TRUE, force = TRUE)
}
