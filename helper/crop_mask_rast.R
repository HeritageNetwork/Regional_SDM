library(sf)
library(raster)
library(snow)
library(smoothr)

# needs raster list (fullL), loc_envVars, model_species

########################################
# get range info from the DB (as a list of HUCs)
db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLquery <- paste0("SELECT huc10_id from lkpRange
                   inner join lkpSpecies on lkpRange.EGT_ID = lkpSpecies.EGT_ID
                   where lkpSpecies.sp_code = '", model_species, "';")
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
conus <- st_read(nm_refBoundaries)
rangeClipped <- st_intersection(rangeDissHolesFilled, conus)
#dissolve again
rangeDissolved_2 <- st_union(rangeClipped)
# write out a dissolved version of hucRange for 'study area'
st_write(rangeDissolved_2, delete_dsn = TRUE,
         here("_data","species",model_species,"inputs","model_input",paste0(model_run_name, "_studyArea.gpkg")))

rm(hucRange, rangeDissolved, rangeDissHolesFilled, conus, rangeClipped)

########################################
# hucRange <- st_zm(st_read(nm_studyAreaExtent,quiet = T)) #DNB TESTING ONLY

# crop/mask rasters to a temp directory 

# delete temp rasts folder, create new
temp <- paste0(options("rasterTmpDir")[1], "/", modelrun_meta_data$model_run_name)
if (dir.exists(temp)) {
  unlink(x = temp, recursive = T, force = T)
}
dir.create(temp, showWarnings = F)

# get proj info from 1 raster
rtemp <- raster(fullL[[1]])

# clipping/masking boundary
rng <- st_transform(rangeDissolved_2, crs = as.character(rtemp@crs))
rm(rtemp)
rng <- st_sf(geometry = st_cast(st_union(rng), "POLYGON"))
rng$id <- 1:length(rng$geometry)

# write shapes
clipshp <- paste0(temp, "/", "clipshp.shp")
st_write(rng, dsn = temp, layer = "clipshp.shp", driver="ESRI Shapefile", delete_layer = T, quiet = T)

ext <- st_bbox(rng)

# cluster process rasters
cl <- makeCluster(parallel::detectCores() - 1, type = "SOCK") 
clusterExport(cl, list("temp", "ext", "clipshp"), envir = environment()) 
clusterExport(cl, list("loc_envVars"), envir = environment()) 

message("Creating raster subsets for species for ", length(fullL) , " environmental variables...")
newL <- parLapply(cl, x = fullL, fun = function(x) {
  path <- x
  subnm <- gsub(paste0(loc_envVars,"/"), "", path)
  if (grepl("/",subnm)) {
    subdir <- strsplit(subnm, "/", fixed = T)[[1]][1]
    dir.create(paste0(temp, "/", subdir), showWarnings = F)
  }
  nnm <- paste0(temp, "/", subnm)
  
  # crop w/clip
  call <- paste0("gdalwarp -te ", paste(ext, collapse = " "), " -cutline ", clipshp, " ", path, " ", nnm, " -overwrite -q")
  system(call)
  return(nnm)
})
stopCluster(cl)