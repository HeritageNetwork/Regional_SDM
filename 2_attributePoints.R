# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(RSQLite)
library(maptools)

# Set paths ----
pathToRas <- "K:/Reg5Modeling_Project/inputs/env_vars/nativeR"
pathToRanPts <- "K:/Reg5Modeling_Project/inputs/species/glypmuhl/point_data"

setwd(pathToRas)

# load data, QC ----
# create a stack
# if using TIFFs, use this line

#raslist <- list.files(pattern = ".tif$")
# if using native R rasters, use this line
raslist <- list.files(pattern = ".grd$")

gridlist <- as.list(paste(pathToRas,raslist,sep = "/"))
nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm

# check to make sure there are no names greater than 10 chars
nmLen <- unlist(lapply(nm, nchar))
max(nmLen) # if this result is greater than 10, you've got a renegade

envStack <- stack(gridlist)

# Set working directory to the random points location
setwd(pathToRanPts)

ranPtsFiles <- list.files(pattern = ".RanPts.shp$")
ranPtsFiles
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

ranPtsFilesNoExt <- sub(".shp","",ranPtsFiles)
shpf <- readOGR(".", layer = ranPtsFilesNoExt[n])

#get projection info for later
projInfo <- shpf@proj4string

#Get the species code for the ranPtsFile chosen
code_name <- substr(ranPtsFiles,1,(nchar(ranPtsFiles)-11))[[n]]

# extract raster data to points ----
##  Bilinear interpolation is a *huge* memory hog. 
##  Do it all as 'simple' 

x <- extract(envStack, shpf, method="simple", sp=TRUE)
filename <- paste(code_name, "_att", sep="")

# write it out ----
# apply projection info
x@proj4string <- projInfo
writeOGR(x, ".", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())
