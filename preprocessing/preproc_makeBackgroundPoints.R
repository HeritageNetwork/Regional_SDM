# File: preproc_makeBackgroundPoints.r
# Purpose: sampling of study area polygon to generate background random points

## start with a fresh workspace with no objects loaded
rm(list=ls())
library(sf)
library(here)
library(RSQLite)

# background points shapefile (points are generated within this boundary). 
# this should be in the project's projection (matching the raster EVs)
StudyAreaPoly <- "C:/David/scratch/jurisbnd_lam_clipbound.shp"
# huc12s. These polygons are used to attribute points. Must have "HUC_12" column.
huc12 <- "C:/David/scratch/huc12s_VA.shp"
# number of points to generate
numpts <- 5000
# new/existing db table name (overwritten)
table <- "background_pts_VA"

# path to background points shapefile
pathToPts <- here("_data","env_vars","background")
# path to output tables (a database (background.sqlite) is generated here if it doesn't exist)
pathToTab <- here("_data","env_vars","tabular")

# load layers
sa <- st_read(StudyAreaPoly)
crs <- st_crs(sa)
huc <- st_transform(st_read(huc12), crs)

# generate points
samps1 <- st_sample(sa, size = numpts)
samps1 <- st_sf(fid = 1:length(samps1), geometry = samps1)
samps <- st_join(samps1, huc, join = st_intersects)[c("fid", "HUC_12")]
names(samps) <- c("fid", "huc12", "geometry")

# create DF
sampsDF <- data.frame(fid = samps$fid, huc12 = samps$huc12, wkt = st_as_text(samps$geometry))

# send to database
db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background.sqlite"))
dbWriteTable(db, table, sampsDF, overwrite = T)

# write CRS
tcrs <- data.frame(table_name = table, proj4string = as.character(crs$proj4string))
try(dbExecute(db, paste0("DELETE FROM lkpCRS where table_name = '", table, "';")), silent = T)
dbWriteTable(db, "lkpCRS", tcrs, append = T)

# also write to shapefile
st_write(samps, paste0(pathToPts, "/", table, "_RanPts.shp"), delete_layer = T)
