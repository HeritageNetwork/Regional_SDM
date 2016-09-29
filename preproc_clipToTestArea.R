#####
# This script will clip rasters and point shapefiles to a test area. 
#
#
####

library(raster)
library(rgdal)

# set this path to the folder where the environmental rasters reside
pathToTifs <- "D:/RegionalSDM/env_vars/geotiffs"

# the path to write out the brick to
pathToClipped <- "D:/RegionalSDM/zz_testArea/env_vars/geotiffs"

# path to the shape to use for clipping
pathToClipShape <- "D:/RegionalSDM/zz_testArea"
clipShapeName <- "testAreaGlypMuhl_AlbersUSGS"

clpShp <- readOGR(pathToClipShape,clipShapeName)

# get a list of the grids
tiflist <- list.files(path = pathToTifs, pattern = ".tif$")

# tack on the full paths and name them
gridlist<-as.list(paste(pathToTifs,tiflist,sep = "/"))
nm <- substr(tiflist,1,nchar(tiflist) - 4)
names(gridlist)<-nm

for (i in 1:length(gridlist)){
  ras <- raster(gridlist[[i]])
  fn <- paste(pathToClipped, "/", names(gridlist[i]), ".tif", sep="")
  a <- crop(ras,clpShp, filename = fn, format = "GTiff", overwrite = TRUE)
}


### now clip the points to same rectangle

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





