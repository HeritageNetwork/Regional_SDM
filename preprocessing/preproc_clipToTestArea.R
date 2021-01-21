# File: preproc_clipToTestArea.r
# Purpose: This script will clip rasters and point shapefiles to a test area. 
#  If we find ourselves creating models within subsets of the range, this 
#  script should be able to create our subsets.

library(checkpoint)
checkpoint("2020-04-22", scanForPackages = FALSE)

library(here)
library(raster)
library(sf)
#library(rgdal)

## set paths ----
# set this path to the folder where the environmental rasters reside
pathToRas <- here("_data","env_vars","raster")

# the path to write out the tiffs to
pathToClipped <- here("_data","env_vars","rasterClipped")

# path to the shape to use for clipping
pathToClipShape <- here("_data","other_spatial","feature","HUC10_full_bkg_area.gpkg")
clipShapeName <- "HUC10_bkg"

clpShp <- st_read(pathToClipShape, clipShapeName)
# continual problems with ESRI Albers. Set it here manually
# ignore the warning
suppressWarnings(st_crs(clpShp) <- 42303)
# dissolve it
cs <- st_union(clpShp)

# get a list of the grids
tiflist <- list.files(path = pathToRas, pattern = ".tif$", recursive = TRUE)

## already got some clipped? use the next few lines to check and 
## remove the ones already done
donetiflist <- list.files(path = pathToClipped, pattern = ".tif$")

finalTifList <- tiflist[!sub("^[_[:alnum:]]+/","",tiflist) %in% donetiflist]
tiflist <- finalTifList

# tack on the full paths and name them
gridlist<-as.list(paste(pathToRas,tiflist,sep = "/"))
nm <- substr(tiflist,1,nchar(tiflist) - 4)
names(gridlist)<-nm

bbx <- st_bbox(cs)
extnt <- c(bbx$xmin, bbx$xmax, bbx$ymin, bbx$ymax)

## clip the rasters ----
for (i in 1:length(gridlist)){
  ras <- raster(gridlist[[i]])
  #strip subfolders from new name
  rasOut <- sub("^[_[:alnum:]]+/","",names(gridlist[i]))
  fn <- paste(pathToClipped, "/", rasOut, ".tif", sep="")
  a <- crop(ras,extnt)
  writeRaster(a, filename = fn, format = "GTiff", overwrite = TRUE, datatype = "INT4S")
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