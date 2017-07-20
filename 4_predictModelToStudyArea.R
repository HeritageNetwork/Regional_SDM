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
load(rdat_nm)


##Make the raster stack
#browser()
stackOrder <- names(df.full)[indVarCols]
setwd(loc_envVars)
rasL <- paste(stackOrder,".tif", sep="")
fullL <- as.list(paste(loc_envVars, rasL, sep="/"))
names(fullL) <- stackOrder
envStack <- stack(fullL)

# run prediction ----
fileNm <- paste(loc_outRas, "/", ElementNames$Code, "_",Sys.Date(),".tif", sep = "")
outRas <- predictRF(envStack, rf.full, progress="text", index=2, na.rm=TRUE, type="prob", filename=fileNm, format = "GTiff", overwrite=TRUE)

#writeRaster(outRas, filename=paste(fileNm, "_2",sep=""), format = "GTiff", overwrite = TRUE)

