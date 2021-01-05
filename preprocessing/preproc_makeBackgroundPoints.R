# File: preproc_makeBackgroundPoints.r
# Purpose: sampling of study area polygon to generate background random points

## start with a fresh workspace with no objects loaded
rm(list=ls())
library(sf)
library(here)
library(RSQLite)

# HUC10 layer should be in the project's projection (matching the raster EVs)

setwd(here("_data","other_spatial","feature"))
# states <- st_read("US_States.shp")
stdyAreaHucs <- st_read("HUC10_full_bkg_area.gpkg",  "HUC10_bkg")

# continual problems with ESRI Albers. Set it here manually
# ignore the warning
# st_crs(stdyArea) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
suppressWarnings(st_crs(stdyAreaHucs) <- 42303)

# #study area is a dissolved representation
sa <- st_union(stdyAreaHucs)

# number of points to generate
numpts <- 200000

# new/existing db table name (overwritten)
table <- "background_pts"

# path to background points shapefile
pathToPts <- here("_data","env_vars","background")
# path to output tables (a database (background.sqlite) is generated here if it doesn't exist)
pathToTab <- here("_data","env_vars","tabular")


# generate points
samps1 <- st_sample(sa, size = numpts)
samps1 <- st_sf(fid = 1:length(samps1), geometry = samps1)
samps <- st_join(samps1, stdyAreaHucs, join = st_intersects)[c("fid", "HUC10")]
names(samps) <- c("fid", "huc10", "geometry")

# create DF
sampsDF <- data.frame(fid = samps$fid, huc10 = samps$huc10, wkt = st_as_text(samps$geometry))

# send to database
db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background_AZ.sqlite"))
tp <- as.vector("INTEGER")
names(tp) <- "fid"
dbWriteTable(db, table, sampsDF, overwrite = TRUE, field.types = tp)

# write CRS
tcrs <- data.frame(table_name = table, 
                   proj4string = st_crs(stdyAreaHucs)$proj4string,
                   wkt = st_crs(stdyAreaHucs)$wkt,
                   epsg = st_crs(stdyAreaHucs)$epsg)
try(dbExecute(db, paste0("DELETE FROM lkpCRS where table_name = '", table, "';")), silent = TRUE)
dbWriteTable(db, "lkpCRS", tcrs, append = TRUE)

# also write to geopackage  
st_write(samps, dsn = paste0(pathToPts, "/", table, ".gpkg"),
         layer = "bkg_pts",
         delete_layer = TRUE)

dbDisconnect(db)
rm(db)
