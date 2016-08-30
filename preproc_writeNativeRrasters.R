#####
# This script will take a folder of geoTiffs and 
# write them out in native R raster format
#
####

library(raster)

# set this path to the folder where the environmental rasters reside
# it can have other files, but only *.tifs that you want to write to the R format

pathToTifs <- "D:/RegionalSDM/env_vars/geotiffs"

setwd(pathToTifs)

# set the output path
outPath <- "D:/RegionalSDM/env_vars/nativeR"

# get a list of the grids
tiflist <- list.files(pattern = ".tif$")

# loop through each tiff in the list, write it out in native R format
for(tif in tiflist){
  print(paste("working on", tif, sep = " "))
	x <- raster(tif)
	nm <- substr(tif,1,nchar(tif) - 4)
	writeRaster(x, filename = paste(outPath,"/",nm,sep=""), format = "raster")
	}


