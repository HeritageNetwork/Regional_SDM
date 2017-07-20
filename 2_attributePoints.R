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
#loc_scripts <- "K:/Reg5Modeling_Project/scripts/Regional_SDM"

#source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))
setwd(loc_envVars)

# create a stack
# if using TIFFs, use this line
raslist <- list.files(pattern = ".tif$")
# if using native R rasters, use this line
#raslist <- list.files(pattern = ".grd$")

gridlist <- as.list(paste(loc_envVars,raslist,sep = "/"))
nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm

# check to make sure there are no names greater than 10 chars
nmLen <- unlist(lapply(nm, nchar))
max(nmLen) # if this result is greater than 10, you've got a renegade

envStack <- stack(gridlist)

# Set working directory to the random points location
setwd(loc_spPts)

ranPtsFiles <- list.files(pattern = ".RanPts.shp$")
ranPtsFiles
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

ranPtsFilesNoExt <- sub(".shp","",ranPtsFiles[n])
shpf <- readOGR(".", layer = ranPtsFilesNoExt)

#get projection info for later
projInfo <- shpf@proj4string

# Get the species code for the ranPtsFile chosen
# assume you want first part of text string, before first underscore
code_name <- strsplit(ranPtsFilesNoExt, "_")[[1]][1]

# extract raster data to points ----
##  Bilinear interpolation is a *huge* memory hog. 
##  Do it all as 'simple' 

points_attributed <- extract(envStack, shpf, method="simple", sp=TRUE)

# write it out ----
# apply projection info
points_attributed@proj4string <- projInfo
filename <- paste(code_name, "_att", sep="")
writeOGR(points_attributed, ".", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)

