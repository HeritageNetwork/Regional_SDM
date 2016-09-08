# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

library(randomForest)

#####
#  Lines that require editing
#

# directory for the RData files (analysis data)
rdataLoc <- "D:/RegionalSDM/zz_testArea/outputs"

# directory for the environmental rasters
pathToRas <- "D:/RegionalSDM/zz_testArea/env_vars/nativeR"

# get the customized version of the predict function
source('D:/RegionalSDM/scripts/Regional_SDM/RasterPredictMod.R')

#  End, lines that require editing
#
#####

## get the rdata file
setwd(rdataLoc)
fileList <- dir(pattern = ".Rdata$",full.names=FALSE)
fileList
# choose one to run, load it #### requires editing ####
n <- 1
load(fileList[[n]])

##Make the raster stack
stackOrder <- names(df.in)[indVarCols]
setwd(pathToRas)
rasL <- paste(stackOrder,".grd", sep="")
fullL <- as.list(paste(pathToRas, rasL, sep="/"))
names(fullL) <- stackOrder
envStack <- stack(fullL)


fileNm <- paste(rdataLoc, ElementNames$Code, sep = "/")


outRas <- predictRF(envStack, rf.full, progress='text', index=2, na.rm=TRUE, type="prob", filename=fileNm, format = "GTiff", overwrite=TRUE)

#writeRaster(outRas, filename=fileNm, format = "GTiff", overwrite = TRUE)



