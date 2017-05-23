# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(RSQLite)
library(maptools)

# load data, QC ----

###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 43 where you choose which random points file to use
loc_scripts <- "E:/SDM/Aquatic/scripts/Regional_SDM"

source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))
setwd(loc_envVars)

EnvVars <- read.csv("EnvVars.csv") #may need additional code for field types

nm <- names(EnvVars)
# check to make sure there are no names greater than 10 chars
nmLen <- unlist(lapply(nm, nchar))
max(nmLen) # if this result is greater than 10, you've got a renegade

names(EnvVars) <- tolower(names(EnvVars))

# join ev to reaches
# Set working directory to the random points location
setwd(loc_spReaches)
#get a list of what's in the directory
fileList <- dir( pattern = "_prepped.csv$")
fileList
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

fileName <- fileList[[n]]
shpName <- strsplit(fileName,"\\_prepped.")[[1]][[1]]
sppCode <- shpName
reaches <- read.csv(fileName)

# merge two data frames by COMID
reaches_attributed <- merge(reaches,EnvVars,by="comid")

# write it out ----
write.csv(reaches_attributed, paste(sppCode,"_att.csv",sep=""))

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())
