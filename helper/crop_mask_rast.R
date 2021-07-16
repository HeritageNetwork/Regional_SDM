library(sf)
library(raster)
library(snow)
library(smoothr) 
library(fasterize) # if using raster method for cropping instead of calling gdalwarp

# needs raster list (fullL), loc_envVars, model_species

# get range info ----
# has the range already been created? If so, don't waste time re-creating
allRanges_fn <- paste0(loc_scripts, "/_data/other_spatial/feature/","Ranges_dissolved.gdb")
if(file.exists(allRanges_fn)) {
  allRanges <- st_read(allRanges_fn)
  targRange <- allRanges[allRanges$EGT_ID == ElementNames$EGT_ID,]
  #make sure a range actually got extracted above
  if(length(st_dimension(targRange)) > 0){
    # rangeClipped <- targRange
    # fill holes/slivers
    rangeClipped <- fill_holes(targRange, threshold = units::set_units(10, km^2))
  }
}

# get the range the long way if rangeClipped didn't get created, above
if(!exists("rangeClipped")){
  db <- dbConnect(SQLite(),dbname=nm_db_file)   
  # The IS operator allows nulls to be equal (https://www.sqlite.org/lang_expr.html#isisnot)
  SQLquery <- paste0("SELECT huc10_id from lkpRange ",
                     "inner join lkpSpecies on ", 
                     "(lkpRange.EGT_ID = lkpSpecies.EGT_ID and ", 
                     "nullif(lkpRange.location_use_class,'') IS nullif(lkpSpecies.location_use_class,'')) ", 
                     "where lkpSpecies.sp_code = '", model_species, "';")
  hucList <- dbGetQuery(db, statement = SQLquery)$huc10_id
  dbDisconnect(db)
  rm(db)
  
  # now get that info spatially
  nm_range <- nm_HUC_file
  qry <- paste("SELECT * from HUC10 where HUC10 IN ('", paste(hucList, collapse = "', '"), "')", sep = "")
  hucRange <- st_zm(st_read(nm_range, query = qry))
  
  # dissolve it
  rangeDissolved <- st_union(hucRange)
  # fill holes/slivers
  rangeDissHolesFilled <- fill_holes(rangeDissolved, threshold = units::set_units(10, km^2))
  # crop to CONUS boundary
  # use the dissolved version
  conus <- st_read(paste0(strsplit(nm_refBoundaries, "[.]")[[1]][[1]], "_dissolve.shp"))
  #conus <- st_read(nm_refBoundaries)
  rangeClipped <- st_intersection(rangeDissHolesFilled, conus)
  #dissolve again (if not using the dissolved version)
  #rangeDissolved_2 <- st_union(rangeClipped)
  # write out a dissolved version of hucRange for 'study area'

  rm(hucRange, rangeDissolved, rangeDissHolesFilled, conus)
}

#check if shape is valid
if(FALSE %in% st_is_valid(rangeClipped)){
  # st_make_valid not available to this install
  rangeClipped <- st_buffer(rangeClipped, 0)
}

st_write(rangeClipped, delete_dsn = TRUE,
         file.path(loc_model, model_species,"inputs","model_input",paste0(model_run_name, "_studyArea.gpkg")))

## crop/mask rasters to a temp directory ----

# delete temp rasts folder, create new
temp <- paste0(options("rasterTmpDir")[1], "/", modelrun_meta_data$model_run_name)
if (dir.exists(temp)) {
  unlink(x = temp, recursive = TRUE, force = TRUE)
}
dir.create(temp, showWarnings = FALSE)

# get proj info from 1 raster
rtemp <- raster(paste0(loc_envVars,"/",fullL[[1]]))

# if debugging with already clipped rasters, paste the tmp path in
#newL <- lapply(fullL, FUN = function(x) paste0(temp,"/",x))

# clipping/masking boundary
rng <- st_transform(rangeClipped, crs = as.character(rtemp@crs))
rng <- st_sf(geometry = st_cast(st_union(rng), "POLYGON"))
rng$id <- 1:length(rng$geometry)
ext <- st_bbox(rng)

# create and write the raster mask
clipRas <- paste0(temp, "/", "clipRas.tif")
rasExtent <- raster::crop(rtemp, extent(as(rng, "Spatial")))
cropRas <- fasterize(rng, rasExtent)
writeRaster(cropRas, clipRas, options="COMPRESS=NONE")

rm(rtemp, rng)

# ## memory intensive way using clusters
# message("Spawning clusters for cropping")
# # # cluster process rasters
# cl <- snow::makeCluster(parallel::detectCores() - 10, type = "SOCK")
# snow::clusterExport(cl, list("temp", "clipRas", "loc_envVars"), envir = environment())
# 
# message("Creating raster subsets for species for ", length(fullL) , " environmental variables...")
# newL <- snow::parLapply(cl, x = fullL, fun = function(path) {
#   subnm <- gsub(paste0(loc_envVars,"/"), "", path)
#   if (grepl("/",subnm)) {
#     folderDepth <- length(gregexpr("/", subnm)[[1]])
#     subdir <- paste(strsplit(subnm, "/", fixed = T)[[1]][1:folderDepth],collapse = "/")
#     dir.create(paste0(temp, "/", subdir), showWarnings = FALSE, recursive = TRUE)
#   }
#   nnm <- paste0(temp, "/", subnm)
# 
#   ## with fasterize and raster
#   ras <- raster::raster(path) # read the raster
#   dtp <- raster::dataType(ras) # get data type, some crops are setting large values to NA
#   cropRas <- raster::raster(clipRas) # read the crop raster
#   rasAtExtent <- raster::crop(ras, raster::extent(cropRas), datatype = dtp) # crop extent to same as mask ras
#   outRas <- raster::mask(rasAtExtent, cropRas, filename = nnm, options="COMPRESS=NONE", datatype = dtp) # mask it
# 
#   return(nnm)
# })
# stopCluster(cl)
# rm(cl)

## less memory intensive but slower
message("Creating raster subsets for species for ", length(fullL) , " environmental variables...")
newL <- fullL

for(k in 1:length(fullL)){
  path <- fullL[[k]]
  subnm <- gsub(paste0(loc_envVars,"/"), "", path)
  if (grepl("/",subnm)) {
    folderDepth <- length(gregexpr("/", subnm)[[1]])
    subdir <- paste(strsplit(subnm, "/", fixed = T)[[1]][1:folderDepth],collapse = "/")
    dir.create(paste0(temp, "/", subdir), showWarnings = FALSE, recursive = TRUE)
  }
  nnm <- paste0(temp, "/", subnm)
  ras <- raster::raster(path)
  dtp <- raster::dataType(ras) # get data type, some crops are setting large values to NA
  cropRas <- raster::raster(clipRas) # read the crop raster
  rasAtExtent <- raster::crop(ras, raster::extent(cropRas), datatype = dtp) # crop extent to same as mask ras
  outRas <- raster::mask(rasAtExtent, cropRas, filename = nnm, options="COMPRESS=NONE", datatype = dtp, overwrite=TRUE) # mask it
  newL[[k]] <- nnm
  message("cropped ", k, " of ", length(fullL), " rasters for predict.")
  }


### if restarting with rasters already cropped, can get the proper newL with this
# for(k in 1:length(fullL)){
#   path <- fullL[[k]]
#   subnm <- gsub(paste0(loc_envVars,"/"), "", path)
#   if (grepl("/",subnm)) {
#     folderDepth <- length(gregexpr("/", subnm)[[1]])
#     subdir <- paste(strsplit(subnm, "/", fixed = T)[[1]][1:folderDepth],collapse = "/")
#     dir.create(paste0(temp, "/", subdir), showWarnings = FALSE, recursive = TRUE)
#   }
#   nnm <- paste0(temp, "/", subnm)
#   newL[[k]] <- nnm
#   message("cropped ", k, " of ", length(fullL), " rasters for predict.")
# }



closeAllConnections()

