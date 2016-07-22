# File: 2_attributePoints.r
# Purpose: attribute environmental data to random points

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(RSQLite)

## Set Working Directory to the rasters location
setwd("G:/RegionalSDM/env_vars/brick")

## Option 1: load the brick
envBrick <- brick("brick.grd")

## Option 2: create a stack
# tiflist <- list.files(pattern = ".tif$")
# gridlist<-as.list(paste(pathToTifs,tiflist,sep = "/"))
# nm <- substr(tiflist,1,nchar(tiflist) - 4)
# names(gridlist)<-nm
# envStack <- stack(gridlist)

## Set working directory to the random points location
setwd("G:/RegionalSDM/inputs/species/glypmuhl/point_data")

ranPtsFiles <- list.files(pattern = ".RanPts.shp$")
ranPtsFilesNoExt <- sub(".shp","",ranPtsFiles)

##Read these files into a list of SpatialPoints dataframes
list_shpf <- lapply(ranPtsFilesNoExt,function(x)
  readOGR(".",layer=x)
  )
  
#Get a list of the codes (this assumes all the input files had '_RanPts.shp' that shall be stripped)
code_names <- substr(ranPtsFiles,1,(nchar(ranPtsFiles)-11))

##Add names to the list
names(list_shpf) <- code_names

## If we have any categorical data sets, then we need to extract the cell values
## directly. If continuous data, then we can (should) apply bilinear interpolation. 

### get categorical/continuous info from the lookup database
db_file <- "F:/_Howard/git/Regional_SDM/SDM_lookupAndTracking.sqlite"
db <- dbConnect(SQLite(),dbname=db_file)
# get list of layers, select from the db, put into a dataframe
layerList <- paste(names(envBrick), collapse = "', '")
query <- paste("Select dataType from lkpEnvVars where code in ('", layerList, "');", sep="")
dataTypes <- dbGetQuery(db,query)
dataTypes <- cbind(dataTypes, layer = names(envBrick))
dataTypes$method <- ifelse(dataTypes$dataType == "categorical", "simple", "bilinear")
dbDisconnect(db)
rm(db)

# step through list of shapefiles (probably only one)
for(j in 1:length(list_shpf)){
  # assume mixed simple/bilinear or bilinear only
  if("simple" %in% dataTypes$method){
      # clean vector method suggested by Robert Hijmans
      k <- dataTypes$method == "simple"
      x1 <- extract(envBrick[[which(k), drop=FALSE]], list_shpf[[j]], sp=TRUE)
      x2 <- extract(envBrick[[which(!k), drop=FALSE]], list_shpf[[j]], method='bilinear') 
      x1@data <- cbind(x1@data, x2) 
      filename <- paste(names(list_shpf)[[j]], "_att", sep="")
      writeOGR(x1, ".", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)
  } else {
      x <- extract(envBrick,list_shpf[[j]],method="bilinear", sp=TRUE)
      filename <- paste(names(list_shpf)[[j]], "_att", sep="")
      writeOGR(x, ".", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)
  }
}

# final result is a point shapefile (or point shapefiles if you had more than one) 
# written to the same folder as the original point shapefile that is fully attributed
# with all layers of the brick.

# note that when there are categorical types, they end up sorted as the 
# first env columns in the output shapefile. Columns will need re-sorting
# at a later step. 