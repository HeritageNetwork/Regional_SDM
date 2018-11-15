library(sf)
library(raster)

# need raster list, range sf object [nm_studyAreaExtent], loc_envVars, model_species
fullL <- gridlist[tolower(justTheNames) %in% tolower(gridlistSub)]

temp <- paste0(loc_model, "/", model_species, "/inputs/temp_rasts")
dir.create(temp, showWarnings = F)
file.remove(list.files(temp, all.files = T, full.names = T, recursive = T, include.dirs = T))

rtemp <- raster(fullL[[1]])

# clip box
rng1 <- st_zm(st_read(nm_studyAreaExtent,quiet = T))
rng <- st_transform(rng1, crs = as.character(rtemp@crs)) # just take one for now
rng <- st_sf(geometry = st_cast(st_union(rng), "POLYGON"))
rng$id <- 1:length(rng$geometry)

clipshp <- paste0(temp, "/", "clipshp.shp")
st_write(rng, dsn = temp, layer = "clipshp.shp", driver="ESRI Shapefile", delete_layer = T)

ext <- st_bbox(rng)
newL <- list()

# loop over rasters
message("Creating raster subsets for species...")
for (p in 1:length(fullL)) {
  path <- fullL[[p]]
  subnm <- gsub(paste0(loc_envVars,"/"), "", path)
  if (grepl("/",subnm)) {
    subdir <- strsplit(subnm, "/", fixed = T)[[1]][1]
    dir.create(paste0(temp, "/", subdir), showWarnings = F)
  }
  nnm <- paste0(temp, "/", subnm)
  # print(nnm)
  
  # crop w/clip
  call <- paste0("gdalwarp -te ", paste(ext, collapse = " "), " -cutline ", clipshp, " ", path, " ", nnm, " -overwrite -q")
  system(call)
  
  newL[[p]] <- nnm
  names(newL)[p] <- names(fullL)[p]
}
