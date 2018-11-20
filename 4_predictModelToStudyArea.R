# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(randomForest)
library(RSQLite)
library(sf)
removeTmpFiles(48) # clean old (>2days) Raster temporary files

####
## two lines need your attention. The one directly below (loc_scripts)
## and about line 26 where you choose which Rdata file to use,

# get paths, other settings
# get the customized version of the predict function
# source(paste(loc_scripts, "helper/RasterPredictMod.R", sep = "/"), local = TRUE)

# load data ----
# get the rdata file
setwd(loc_model)
dir.create(paste0(model_species,"/outputs/model_predictions"), recursive = T, showWarnings = F)
setwd(paste0(model_species,"/outputs"))

# load rdata
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

##Make the raster stack
stackOrder <- names(df.full)[indVarCols]
# set wd
#temprast <- paste0(loc_model, "/", model_species, "/inputs/temp_rasts")
#if (dir.exists(temprast)) setwd(temprast) else 
setwd(loc_envVars)

# find matching var rasters (with folder for temporal vars)
raslist <- list.files(pattern = ".tif$", recursive = TRUE)

fullL <- list()

# attach file names to env var names
for (i in 1:length(stackOrder)) {
  rs <- raslist[grep(paste0(stackOrder[i],".tif"), raslist, ignore.case = TRUE)]
  if (length(rs) > 1) {
    # always take most recent temporal raster
    rs1 <- do.call(rbind.data.frame, strsplit(rs, "_|/"))
    rs1$nm <- rs
    rs <- rs1$nm[which.max(as.numeric(rs1[,2]))]
  }
  fullL[[i]] <- rs
}
names(fullL) <- stackOrder
rm(rs,rs1)

source(paste0(loc_scripts, "/helper/crop_mask_rast.R"), local = TRUE)
envStack <- stack(newL)

#envStack <- stack(fullL) # if not using helper/crop_mask_rast.R
rm(fullL)

# run prediction ----
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/", model_run_name,".tif")

# use parallel processing if packages installed
if (all(c("snow","parallel") %in% installed.packages())) {
  try({
    beginCluster(type = "SOCK")
    outRas <- clusterR(envStack, predict, args = list(model=rf.full, type = "prob", index = 2), verbose = T)
    writeRaster(outRas, filename = fileNm, format = "GTiff", overwrite = TRUE)
  })
  try(endCluster())
  if (!exists("outRas")) {
    cat("Cluster processing failed. Falling back to single-core processing...\n")
    outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2,
                      filename=fileNm, format = "GTiff", overwrite=TRUE)
  }
# otherwise regular processing
} else {
  cat("Using single-core processing for prediction.\nInstall package 'snow' for faster cluster (multi-core) processing.\n")
  outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2,
                    filename=fileNm, format = "GTiff", overwrite=TRUE)

}

# delete temp rasts folder
if (dir.exists(paste(loc_model, model_species, "inputs", "temp_rasts", sep = "/"))) {
  unlink(x = paste(loc_model, model_species, "inputs", "temp_rasts", sep = "/"), recursive = T, force = T)
}