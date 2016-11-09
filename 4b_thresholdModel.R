# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

inPath <- "G:/RegionalSDM/outputs"
gridpath <- "G:/RegionalSDM/outputs/grids"
#out path
outRas <- "G:/RegionalSDM/outputs/grids" 

## find and load model data ----
# get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
# which one do we want to run?
n <- 1
fileName <- d[[n]]
load(paste(inPath,fileName, sep="/"))

#get minimum training presence
x <- data.frame(rf.full$y, rf.full$votes)
y <- x[x$rf.full.y ==1,]
MTP <- min(y$X1)

#get 10 percentile training presence
TenPctile <- quantile(y$X1, prob = c(0.1))

#equal sensitivity and specificity

#max sensitivity plus specificity


# load the prediction grid
ras <- raster(paste(gridpath,"/",ElementNames$Code, ".tif", sep = ""))
#set a threshhold. This is from...
threshold <- round(rf.full.ctoff[2],3)
# reclassify the raster based on the threshold
rasrc <- reclassify(ras, c(-Inf,threshold,0,  threshold,Inf,1))

#plot(rasrc)
outfile <- paste(outRas,"/",ElementNames$Code,"_threshold.tif", sep = "")
writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)

