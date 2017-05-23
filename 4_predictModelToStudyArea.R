# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
#library(raster) # do we stil need this - CT
library(rgdal)
library(randomForest)

####
## two lines need your attention. The one directly below (loc_scripts)
## and about line 26 where you choose which Rdata file to use,

loc_scripts <- "E:/SDM/Aquatic/scripts/Regional_SDM"

# get paths, other settings
source(paste(loc_scripts,"0_pathsAndSettings.R", sep="/"))
# get the customized version of the predict function
source(paste(loc_scripts, "RasterPredictMod.R", sep = "/"))

# load data ----
# get the rdata file
setwd(loc_RDataOut)
fileList <- dir(pattern = ".Rdata$",full.names=FALSE)
fileList
# choose one to run, load it #### requires editing ####
n <- 1
load(fileList[[n]])

# load the environmental variables -- analogous to the development of the raster stack in the terr models
setwd(loc_envVars)
EnvVars <- read.csv("EnvVars.csv", colClasses=c("HUC12"="character"))
names(EnvVars) <- tolower(names(EnvVars))

#Make the raster stack
##stackOrder <- names(df.full)[indVarCols]
##setwd(loc_envVars)
##rasL <- paste(stackOrder,".tif", sep="")
##fullL <- as.list(paste(loc_envVars, rasL, sep="/"))
##names(fullL) <- stackOrder
##envStack <- stack(fullL)

## load the reach shapefile for the study area
setwd(loc_otherSpatial)
StudyAreaReaches <- "flowlines.shp" # the name of the study area flowlines
layer <- strsplit(StudyAreaReaches,"\\.")[[1]][[1]]
shapef <- readOGR(StudyAreaReaches, layer = layer)

# need to understand what's going on here a little more - CT
# run prediction ----
fileNm <- paste(loc_outRas, "/", ElementNames$Code, "_",Sys.Date(),".tif", sep = "")
outRas <- predictRF(EnvVars, rf.full, progress="text", index=2, na.rm=TRUE, type="prob", filename=fileNm, format = "GTiff", overwrite=TRUE)

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())
