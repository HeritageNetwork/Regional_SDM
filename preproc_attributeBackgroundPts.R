# File: preproc_attributeBackgroundPts.r
# Purpose: attribute environmental data to random background points

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

pathToRas <- "D:/RegionalSDM/env_vars/nativeR"
pathToPts <- "D:/RegionalSDM/inputs/background"

setwd(pathToRas)
## create a stack. Note this is using native R rasters
raslist <- list.files(pattern = ".grd$")
gridlist <- as.list(paste(pathToRas,raslist,sep = "/"))
nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm
envStack <- stack(gridlist)

## Set working directory to the random points location
setwd(pathToPts)

ranPtsFile <- "clpBnd_SDM_RanPts_clean.shp"
ranPtsFileNoExt <- sub(".shp","",ranPtsFile)

##Read these files into a list of SpatialPoints dataframes
shpf <- readOGR(".", layer = ranPtsFileNoExt)
  
#Get a list of the codes (this assumes all the input files had '_RanPts.shp' that shall be stripped)
code_name <- substr(ranPtsFile,1,(nchar(ranPtsFile)-11))

# do it, write it
x <- extract(envStack, shpf, method="simple", sp=TRUE)
filename <- paste(code_name, "_att", sep="")
writeOGR(x, pathToPts, layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)


