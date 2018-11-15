# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(randomForest)
library(sf)
library(stars)
removeTmpFiles(48) # clean old (>2days) Raster temporary files

####
## two lines need your attention. The one directly below (loc_scripts)
## and about line 26 where you choose which Rdata file to use,

# get paths, other settings
# get the customized version of the predict function
# source(paste(loc_scripts, "helper/RasterPredictMod.R", sep = "/"), local = TRUE)

# load data ----
# get the rdata file
setwd(loc_model)
dir.create(paste0(model_species,"/outputs/model_predictions"), recursive = T, showWarnings = F)
setwd(paste0(model_species,"/outputs"))

# load rdata
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

##Make the raster stack
stackOrder <- names(df.full)[indVarCols]
setwd(loc_envVars)

# find matching var rasters (with folder for temporal vars)
raslist <- list.files(pattern = ".tif$", recursive = TRUE)
#raslist <- paste(getwd(), raslist, sep = "/")

fullL <- list()

# attach file names to env var names
for (i in 1:length(stackOrder)) {
  rs <- raslist[grep(paste0(stackOrder[i],".tif"), raslist, ignore.case = TRUE)]
  if (length(rs) > 1) {
    # always take most recent temporal raster
    rs1 <- do.call(rbind.data.frame, strsplit(rs, "_|/"))
    rs1$nm <- rs
    rs <- rs1$nm[which.max(as.numeric(rs1[,2]))]
  }
  fullL[[i]] <- rs
}
names(fullL) <- stackOrder
rm(rs,rs1)

envStack <- stack(fullL)


## Get Range info
library(RSQLite)

# get range info from the DB (as a list of HUCs)
db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLquery <- paste0("SELECT huc10_id from lkpRange 
                   inner join lkpSpecies on lkpRange.EGT_ID = lkpSpecies.EGT_ID
                   where lkpSpecies.sp_code = '", model_species, "';")
hucList <- dbGetQuery(db, statement = SQLquery)$huc10_id
dbDisconnect(db)
rm(db)

# now get that info spatially
nm_range <- "E:/Range/HUC10.shp"
qry <- paste("SELECT * from HUC10 where HUC10 IN ('", paste(hucList, collapse = "', '"), "')", sep = "")
hucRange <- st_read(nm_range, query = qry)
 
# library(microbenchmark)
# mb <- microbenchmark(
#   "readEntireHUC"={
#     hucs_sf <- st_read(nm_range)
#     hucRange <- hucs_sf[hucs_sf$HUC10 %in% hucList,]
#       }, 
#   "readwithQuery"={
#     qry <- paste("SELECT * from HUC10 where HUC10 IN ('", paste(hucList, collapse = "', '"), "')", sep = "")
#     hucs_sf_q <- st_read(nm_range, query = qry)
#   }, times = 10
# )

# get the extent of the range
hucExtent <- extent(hucRange)
huc_bb <- st_bbox(hucRange)


#####
### crop and predict the raster way -----
####
start_time <- Sys.time()
envStk.c <- crop(envStack, extent(hucRange))
envStk.m <- mask(envStk.c, hucRange)
# run prediction ----
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/", model_run_name,"rasCrop.tif")
outRas <- predict(object=envStk.m, model=rf.full, type = "prob", index=2,
                  filename=fileNm, format = "GTiff", overwrite=TRUE)
end_time <- Sys.time()
rasCropTime <- end_time - start_time

####
### crop with stars -----
####
setwd(loc_envVars)
st_crs(hucRange) <- st_dimensions(read_stars(fullL[[1]], proxy = TRUE))$x$refsys

start_time <- Sys.time()
setwd(loc_envVars)
rasRasList <- vector("list", length(fullL))
for(i in 1:length(fullL)){
  x <- read_stars(fullL[[i]], proxy = TRUE)
  xsub <- x[hucRange]
  rasRasList[[i]] <- as(st_as_stars(xsub), "Raster")
}
stk2 <- stack(unlist(rasRasList))
names(stk2) <- names(fullL)
stk2.m <- mask(envStk.c, hucRange)
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/", model_run_name,"_stars.tif")
outRas <- predict(object=stk2.m, model=rf.full, type = "prob", index=2,
                  filename=fileNm, format = "GTiff", overwrite=TRUE)
end_time <- Sys.time()
starsTime <- end_time - start_time

####
### no crop (use ext) ----
####
start_time <- Sys.time()
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/", model_run_name,"_noCrop.tif")
outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2, ext = hucExtent,
          format = "GTiff", overwrite=TRUE)
outRas.m <- mask(outRas, hucRange) #slows it back down a bunch
writeRaster(outRas.m, filename = fileNm, format = "GTiff", overwrite = TRUE)
end_time <- Sys.time()
noCropTime <- end_time - start_time


####
## rasterIO approach ----
####
start_time <- Sys.time()
setwd(loc_envVars)
# get dimensions from layer 1 in stack
x <- read_stars(fullL[[1]], proxy = TRUE)
xoff <- st_dimensions(x)$x$offset
xdelt <- st_dimensions(x)$x$delta
yoff <- st_dimensions(x)$y$offset
ydelt <- st_dimensions(x)$y$delta
cropXoff <- (huc_bb$xmin - xoff + xdelt)/xdelt
cropXsize <- (huc_bb$xmax - huc_bb$xmin)/xdelt
cropYoff <- (huc_bb$ymin - yoff + ydelt)/ydelt
cropYsize <- (huc_bb$ymax - huc_bb$ymin)/ydelt

st_crs(hucRange) <- st_dimensions(x)$x$refsys

# if ydelt is negative, get abs of ysize and move yoffset to the top
if(cropYsize < 0){
  cropYsize <- abs(cropYsize)
  cropYoff <- cropYoff - cropYsize  
}
rasterio <- list(nXOff = cropXoff, nYOff = cropYoff, nXSize = cropXsize, nYSize = cropYsize, bands = c(1))

rasRasList <- vector("list", length(fullL))
for(i in 1:length(fullL)){
  x <- read_stars(fullL[[i]], RasterIO = rasterio)
  rasRasList[[i]] <- as(st_as_stars(x), "Raster")
}
stk2 <- stack(unlist(rasRasList))
names(stk2) <- names(fullL)
stk2.m <- mask(stk2, hucRange)
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/", model_run_name,"_starsRIO2.tif")
outRas <- predict(object=stk2.m, model=rf.full, type = "prob", index=2,
                  filename=fileNm, format = "GTiff", overwrite=TRUE)
end_time <- Sys.time()
RIO_time <- end_time - start_time
# about 2 minutes. 

####
## compare times
rasCropTime
starsTime
noCropTime
RIO_time


    
# run prediction ----
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/", model_run_name,".tif")

# use parallel processing if packages installed
if (all(c("snow","parallel") %in% installed.packages())) {
  try({
    beginCluster(type = "SOCK")
    outRas <- clusterR(stk2, predict, args = list(model=rf.full, type = "prob", index = 2, filename = fileNm), 
                       verbose = TRUE)
    writeRaster(outRas, filename = fileNm, format = "GTiff", overwrite = TRUE)
  })
  try(endCluster())
  if (!exists("outRas")) {
    cat("Cluster processing failed. Falling back to single-core processing...\n")
    outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2, ext = hucExtent,
                      filename=fileNm, format = "GTiff", overwrite=TRUE)
  }
  # otherwise regular processing
} else {
  cat("Using single-core processing for prediction.\nInstall package 'snow' for faster cluster (multi-core) processing.\n")
  outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2, ext = hucExtent,
                    filename=fileNm, format = "GTiff", overwrite=TRUE)
  
}
