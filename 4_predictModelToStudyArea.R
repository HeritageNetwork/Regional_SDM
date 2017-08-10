# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(randomForest)

####
## two lines need your attention. The one directly below (loc_scripts)
## and about line 26 where you choose which Rdata file to use,

#loc_scripts <- "K:/Reg5Modeling_Project/scripts/Regional_SDM"

# get paths, other settings
#source(paste(loc_scripts,"0_pathsAndSettings.R", sep="/"))
# get the customized version of the predict function
source(paste(loc_scripts, "RasterPredictMod.R", sep = "/"))

# load data ----
# get the rdata file
setwd(loc_RDataOut)
# fileList <- dir(pattern = ".Rdata$",full.names=FALSE)
# fileList
# # choose one to run, load it #### requires editing ####
# n <- 1
# load(fileList[[n]])
load(paste(modelrun_meta_data$model_run_name,".Rdata", sep=""))


##Make the raster stack
stackOrder <- names(df.full)[indVarCols]
setwd(loc_envVars)

# find matching var rasters (with folder for temporal vars)
raslist <- list.files(pattern = ".tif$", recursive = TRUE)
raslist <- raslist[-grep("OBSOLETE",raslist, fixed = TRUE)]

fullL <- list()

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

envStack <- stack(fullL)

# run prediction ----
fileNm <- paste(loc_outRas, "/", model_run_name ,".tif", sep = "")
outRas <- predictRF(envStack, rf.full, progress="text", index=2, na.rm=TRUE, type="prob", filename=fileNm, format = "GTiff", overwrite=TRUE)

#writeRaster(outRas, filename=paste(fileNm, "_2",sep=""), format = "GTiff", overwrite = TRUE)

