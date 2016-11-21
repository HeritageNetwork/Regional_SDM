# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(ROCR)

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

#max sensitivity plus specificity (maxSSS per Liu et al 2016)
rf.full.sens <- performance(rf.full.pred,"sens")
rf.full.spec <- performance(rf.full.pred,"spec")
rf.full.sss <- data.frame(cutSens = unlist(rf.full.sens@x.values),sens = unlist(rf.full.sens@y.values),
                          cutSpec = unlist(rf.full.spec@x.values), spec = unlist(rf.full.spec@y.values))

rf.full.sss$sss <- with(rf.full.sss, sens + spec)
maxSSS <- rf.full.sss[which.max(rf.full.sss$sss),"cutSens"]

#equal sensitivity and specificity
rf.full.sss$diff <- abs(rf.full.sss$sens - rf.full.sss$spec)
eqss <- rf.full.sss[which.min(rf.full.sss$diff),"cutSens"]


# load the prediction grid
ras <- raster(paste(gridpath,"/",ElementNames$Code, ".tif", sep = ""))
#set a threshhold. This is from...
threshold <- round(rf.full.ctoff[2],3)
threshold <- round(maxSSS, 4)

# reclassify the raster based on the threshold
rasrc <- reclassify(ras, c(-Inf,threshold,0,  threshold,Inf,1))

#plot(rasrc)
outfile <- paste(outRas,"/",ElementNames$Code,"_threshold.tif", sep = "")
writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)

