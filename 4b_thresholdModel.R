# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

inPath <- "X:/RegionalSDM/yy_testArea/outputs"
gridpath <- "X:/RegionalSDM/yy_testArea/outputs/grids"
#out path
outRas <- "X:/RegionalSDM/yy_testArea/outputs/grids" 

## find and load model data ----
# get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
# which one do we want to run?
n <- 1
fileName <- d[[n]]
load(paste(inPath,fileName, sep="/"))
# load the prediction grid
ras <- raster(paste(gridpath,"/",ElementNames$Code, ".tif", sep = ""))
#set a threshhold. This is from...
threshold <- round(rf.full.ctoff[2],3)
# reclassify the raster based on the threshold
rasrc <- reclassify(ras, c(-Inf,threshold,0,  threshold,Inf,1))

#plot(rasrc)
outfile <- paste(outRas,"/",ElementNames$Code,"_threshold.tif", sep = "")
writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)

