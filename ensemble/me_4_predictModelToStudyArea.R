# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(here)
library(raster)
library(RSQLite)
library(sf)
library(snow)
library(parallel)

library(dismo)

removeTmpFiles(48) # clean old (>2days) Raster temporary files

# load data ----
# get the rdata file
setwd(loc_model)
dir.create(paste0(model_species,"/outputs/model_predictions"), recursive = TRUE, showWarnings = FALSE)
setwd(paste0(model_species,"/outputs"))

# load rdata
load(paste0("rdata/mev2_",modelrun_meta_data$model_run_name,".Rdata"))

##Make the raster stack
stackOrder <- names(df.full)[indVarCols]
# set wd
#temprast <- paste0(loc_model, "/", model_species, "/inputs/temp_rasts")
#if (dir.exists(temprast)) setwd(temprast) else 
setwd(loc_envVars)

# get raster full names
db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLQuery <- "select gridName, fileName from lkpEnvVars;"
evs <- dbGetQuery(db, SQLQuery)
evs$gridName <- tolower(evs$gridName)
rasFiles <- merge(data.frame(gridName = stackOrder), evs)
#sort it back to stackOrder's order
rasFiles <- rasFiles[match(stackOrder, rasFiles$gridName),]
dbDisconnect(db)
rm(db)

# find matching var rasters (with folder for temporal vars)
raslist <- list.files(pattern = ".tif$", recursive = TRUE)

fullL <- list()

# attach file names to env var names
for (i in 1:length(stackOrder)) {
  rs <- raslist[grep(rasFiles$fileName[i], raslist, ignore.case = TRUE)]
  if (length(rs) > 1) {
    # always take most recent temporal raster
    rs1 <- do.call(rbind.data.frame, strsplit(rs, "_|/"))
    rs1$nm <- rs
    rs <- rs1$nm[which.max(as.numeric(rs1[,2]))]
    rm(rs1)
  }
  fullL[[i]] <- rs
}
names(fullL) <- stackOrder
rm(rs)

source(paste0(loc_scripts, "/helper/crop_mask_rast.R"), local = FALSE)
envStack <- stack(newL)

#envStack <- stack(fullL) # if not using helper/crop_mask_rast.R
rm(fullL)

# run prediction ----
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/mev2_", model_run_name,".tif")


cat("... predicting throughout study area \n")

pd <- predict(me.out.fin, envStack, filename = fileNm)

#errored out after writing raster, add pd object, will that help?

# # use parallel processing if packages installed
# if (all(c("snow","parallel") %in% installed.packages())) {
#   try({
#     beginCluster(type = "SOCK", n=parallel:::detectCores()-10)
#     outRas <- clusterR(envStack, predict, args = list(model=rf.full, type = "prob", index = 2), verbose = TRUE)
#     writeRaster(outRas, filename = fileNm, format = "GTiff", overwrite = TRUE)
#   })
#   try(endCluster())
#   if (!exists("outRas")) {
#     cat("Cluster processing failed. Falling back to single-core processing...\n")
#     outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2,
#                       filename=fileNm, format = "GTiff", overwrite=TRUE)
#   }
# # otherwise regular processing
# } else {
#   cat("Using single-core processing for prediction.\nInstall package 'snow' for faster cluster (multi-core) processing.\n")
#   outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2,
#                     filename=fileNm, format = "GTiff", overwrite=TRUE)
# 
# }
# 
# # delete temp rasts folder
# if (dir.exists(paste0(options("rasterTmpDir")[1], "/", modelrun_meta_data$model_run_name))) {
#   unlink(x = paste0(options("rasterTmpDir")[1], "/", modelrun_meta_data$model_run_name), recursive = TRUE, force = TRUE)
# }
