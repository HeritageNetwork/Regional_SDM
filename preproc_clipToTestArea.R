# File: preproc_clipToTestArea.r
# Purpose: This script will clip rasters and point shapefiles to a test area. 
#  If we find ourselves creating models within subsets of the range, this 
#  script should be able to create our subsets.

library(raster)
library(rgdal)

## set paths ----
# set this path to the folder where the environmental rasters reside
pathToTifs <- "K:/Florida"

# the path to write out the tiffs to
pathToClipped <- "K:/Reg5Modeling_Project/inputs/env_vars/geotiffs_clip"


# path to the shape to use for clipping
pathToClipShape <- "K:/Reg5Modeling_Project/other_spatial"
clipShapeName <- "reg5_pred_20161027"

clpShp <- readOGR(pathToClipShape,clipShapeName)

# get a list of the grids
tiflist <- list.files(path = pathToTifs, pattern = ".tif$")

## already got some clipped? use the next few lines to check and 
## remove the ones already done
donetiflist <- list.files(path = pathToClipped, pattern = ".tif$")
finalTifList <- tiflist[!tiflist %in% donetiflist]
tiflist <- finalTifList


# tack on the full paths and name them
gridlist<-as.list(paste(pathToTifs,tiflist,sep = "/"))
nm <- substr(tiflist,1,nchar(tiflist) - 4)
names(gridlist)<-nm


## clip the rasters ----
for (i in 1:length(gridlist)){
  ras <- raster(gridlist[[i]])
  fn <- paste(pathToClipped, "/", names(gridlist[i]), ".tif", sep="")
  a <- crop(ras,clpShp, filename = fn, format = "GTiff", overwrite = TRUE)
}


## now clip the points ----
pathToBackgPts <- "D:/RegionalSDM/inputs/background"
backgPts <- "clpBnd_SDM_RanPts"
outPathBkg <- "D:/RegionalSDM/zz_testArea/inputs/background"

bigArea <- readOGR(pathToBackgPts, backgPts)
smallArea <- bigArea[clpShp,]
writeOGR(smallArea, outPathBkg, backgPts, driver="ESRI Shapefile")

pathToPresPts <- "D:/RegionalSDM/inputs/species/glypmuhl/point_data"
presPts <- "glypmuhl_att"
outPathPres <- "D:/RegionalSDM/zz_testArea/inputs/species/glypmuhl/point_data"

bigArea <- readOGR(pathToPresPts, presPts)
smallArea <- bigArea[clpShp,]
writeOGR(smallArea, outPathPres, presPts, driver="ESRI Shapefile")

pathToPresPolys <- "D:/RegionalSDM/inputs/species/glypmuhl/polygon_data"
presPolys <- "glypmuhl"
outPathPres <- "D:/RegionalSDM/zz_testArea/inputs/species/glypmuhl/polygon_data"

bigArea <- readOGR(pathToPresPolys, presPolys)
smallArea <- bigArea[clpShp,]
writeOGR(smallArea, outPathPres, presPolys, driver="ESRI Shapefile")

## clean up ----
# remove all objects before using another script
rm(list=ls())