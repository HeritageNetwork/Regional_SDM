# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

library(randomForest)

#####
#  Lines that require editing

# set up paths ----
# directory for the RData files (analysis data)
rdataLoc <- "K:/Reg5Modeling_Project/outputs"

# directory for the environmental rasters
pathToRas <- "K:/Reg5Modeling_Project/inputs/env_vars/nativeR"

# output path (best if different from rdataloc)
outRasPath <- "K:/Reg5Modeling_Project/outputs/grids"


# get the customized version of the predict function
source('K:/Reg5Modeling_Project/scripts/Regional_SDM/RasterPredictMod.R')

#  End, lines that require editing
#
#####

# load data ----
# get the rdata file
setwd(rdataLoc)
fileList <- dir(pattern = ".Rdata$",full.names=FALSE)
fileList
# choose one to run, load it #### requires editing ####
n <- 2
load(fileList[[n]])

##Make the raster stack
stackOrder <- names(df.full)[indVarCols]
setwd(pathToRas)
rasL <- paste(stackOrder,".grd", sep="")
fullL <- as.list(paste(pathToRas, rasL, sep="/"))
names(fullL) <- stackOrder
envStack <- stack(fullL)

# run prediction ----
fileNm <- paste(outRasPath, ElementNames$Code, sep = "/")
outRas <- predictRF(envStack, rf.full, progress="text", index=2, na.rm=TRUE, type="prob", filename=fileNm, format = "GTiff", overwrite=TRUE)

#writeRaster(outRas, filename=paste(fileNm, "_2",sep=""), format = "GTiff", overwrite = TRUE)

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())

