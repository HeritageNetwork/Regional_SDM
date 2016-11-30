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
# create a stack (assume you are using native R rasters)
raslist <- list.files(pattern = ".tif$")
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

####
# The remaining code explores bilinear interpolation options ----
####

## If we have any categorical data sets, then we need to extract the cell values
## directly. If continuous data, then we can (should) apply bilinear interpolation. 

# ### get categorical/continuous info from the lookup database
# db_file <- "D:/RegionalSDM/databases/SDM_lookupAndTracking.sqlite"
# db <- dbConnect(SQLite(),dbname=db_file)
# # get list of layers, select from the db, put into a dataframe
# layerList <- paste(names(envStack), collapse = "', '")
# query <- paste("Select dataType from lkpEnvVars where code in ('", layerList, "');", sep="")
# dataTypes <- dbGetQuery(db,query)
# dataTypes <- cbind(dataTypes, layer = names(envStack))
# dataTypes$method <- ifelse(dataTypes$dataType == "categorical", "simple", "bilinear")
# dbDisconnect(db)
# rm(db)

#   # assume mixed simple/bilinear or bilinear only
#   if("simple" %in% dataTypes$method){
#       # clean vector method suggested by Robert Hijmans
#       k <- dataTypes$method == "simple"
#       x1 <- extract(envStack[[which(k), drop=FALSE]], shpf, sp=TRUE)
#       x2 <- extract(envStack[[which(!k), drop=FALSE]], shpf, method='bilinear') 
#       x1@data <- cbind(x1@data, x2) 
#       filename <- paste(code_name, "_att", sep="")
#       writeOGR(x1, ".", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)
#   } else {
#       x <- extract(envStack,shpf,method="bilinear", sp=TRUE)
#       filename <- paste(code_name, "_att", sep="")
#       writeOGR(x, ".", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)
#   }

# final result is a point shapefile (or point shapefiles if you had more than one) 
# written to the same folder as the original point shapefile that is fully attributed
# with all layers of the brick.

# note that when there are categorical types, they end up sorted as the 
# first env columns in the output shapefile. Columns will need re-sorting
# at a later step. 


#### if we have problems running out of memory,
## this code below will subset the point layers into groups by state
## and extract by these smaller subsets. 
# numgroups <- 10
# groupSize <- floor(nrow(shpf)/numgroups)
# 
# for (i in 1:(numgroups+1)){
#   begin <- ((i-1)*groupSize) + 1
#   end <- min(c((groupSize * i), nrow(shpf)))
#   #print(paste(c(begin, end),sep = ", "))
#   y <- extract(envStack,shpf[begin:end, ],method="bilinear", sp=TRUE)
#   if (i == 1){
#     z <- y
#   } else {
#     z <- spRbind(z, y)
#   }
# }
# 
# filename <- paste(code_name, "_att", sep="")
# writeOGR(z, ".", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)
# 
