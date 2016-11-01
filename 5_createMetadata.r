# File: 5_createMetadata.r
# Purpose: to summarize validation data and other information about the 
# model and write it to a pdf. This pdf should accompany ALL sharing/showing
# of the SDM map.

# For knitr to work, you need MikTex installed. See http://miktex.org/

# load libraries and set paths ----
library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(randomForest)
library(knitr)
library(raster)
library(maptools)
library(sp)
library(rgdal)
library(RColorBrewer)
library(rgdal)
library(rasterVis)


inPath <- "D:/RegionalSDM/outputs"
rnwPath <- "D:/RegionalSDM/scripts/Regional_SDM"
outPath <- "D:/RegionalSDM/outputs/metadata"
gridpath <- "D:/RegionalSDM/outputs/grids"
stateBoundPath <- "D:/RegionalSDM/other_spatial"

extentMapName <- "StatesNE"
testareapath <- "X:/RegionalSDM/zz_testArea/inputs/background"
testAreaName <- "clpBnd_SDM"


## find and load model data ----
#get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
### which one do we want to run?
n <- 1
fileName <- d[[n]]
load(paste(inPath,fileName, sep="/"))

## Get Program and Sources information ----



## Run knitr and create metadata ----

# writing to the same folder as a grid might cause problems.
# if errors check that first
#   more explanation: tex looks for and uses aux files, which are also used
#   by esri. If there's a non-tex aux file, knitr bails. 

# Also, might need to run this twice. First time through tex builds the reference
# list, second time through it can then number the refs in the doc.

setwd(outPath)

knit2pdf(paste(rnwPath,"MetadataEval_knitr.rnw",sep="/"), output=paste(ElementNames$Code, ".tex",sep=""))



