# File: 2_attributePoints.r
# Purpose: attribute environmental data to random points

# this is set up to be able to attribute multiple species (multiple random points shapefiles)
# at once, but it will run just fine with a single shapefile of random points. 

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

## Set Working Directory to the rasters location
setwd("G:/SDM_test/env_rasters")

## Option 1: load the brick
envBrick <- brick("brick.grd")

## Option 2: create a stack
# tiflist <- list.files(pattern = ".tif$")
# gridlist<-as.list(paste(pathToTifs,tiflist,sep = "/"))
# nm <- substr(tiflist,1,nchar(tiflist) - 4)
# names(gridlist)<-nm
# envStack <- stack(gridlist)

## Set working directory to the random points location
setwd("G:/SDM_test/output")

ranPtsFiles <- list.files(pattern = ".RanPts.shp$")
ranPtsFilesNoExt <- sub(".shp","",ranPtsFiles)

##Read these files into a list of SpatialPoints dataframes
list_shpf <- lapply(ranPtsFilesNoExt,function(x)
  readOGR(".",layer=x)
  )
  
#Get a list of the codes (this assumes all the input files had '_RanPts.shp' that shall be stripped)
code_names<-substr(ranPtsFiles,1,(nchar(ranPtsFiles)-11))

##Add names to the list
names(list_shpf)<-code_names

#TODO: use the sqlite database to get interpolation information (simple or bilinear)
# in this test, all layers are continuous and should be bilinear
# hmmm, might not be able to do separate methods on a brick
#methods_list <- rep("bilinear",4)

# loop through the list, extracting to points, then writing each attributed shapefile
for(j in 1:length(list_shpf)){
	x <- extract(envBrick,list_shpf[[j]],method="bilinear", sp=TRUE)
	layername <- paste(names(list_shpf)[[j]], "_att")
	writeOGR(x, ".", layer=paste(layername), driver="ESRI Shapefile")
}

