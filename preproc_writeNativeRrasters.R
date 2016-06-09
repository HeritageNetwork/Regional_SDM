#####
# This script will take a folder of geoTiffs and 
# write them out in native R raster format
#
####

library(raster)

# set this path to the folder where the environmental rasters reside
# it can have other files, but only *.tifs that you want to be included
# in the brick
pathToTifs <- "G:/SDM_test/env_rasters"

setwd(pathToTifs)

# get a list of the grids
tiflist <- list.files(pattern = ".tif$")

for(tif in tiflist){

	x <- raster(tif)
	nm <- substr(tif,1,nchar(tif) - 4)
	writeRaster(x, filename = nm, format = "raster")
	
	}


