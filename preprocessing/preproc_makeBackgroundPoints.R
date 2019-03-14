# File: preproc_makeBackgroundPoints.r
# Purpose: sampling of study area polygon to generate background random points

## start with a fresh workspace with no objects loaded
rm(list=ls())
library(sf)
library(here)
library(RSQLite)

# temp - tgh - make a shapefile of study area based on HUC
# get range info from the DB (as a list of HUCs)
model_species <- "pletasup"
nm_db_file <- here("_data", "databases", "SDM_lookupAndTracking.sqlite")
db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLquery <- paste0("SELECT huc10_id from lkpRange
                   inner join lkpSpecies on lkpRange.EGT_ID = lkpSpecies.EGT_ID
                   where lkpSpecies.sp_code = '", model_species, "';")
hucList <- dbGetQuery(db, statement = SQLquery)$huc10_id
dbDisconnect(db)
rm(db)
# get the huc10 layer
nm_HUC_pth <- here("_data","other_spatial","feature")
nm_HUC_file <- "HUC10.shp"

op <- options()
options(useFancyQuotes = FALSE) #need straight quotes for query
qry <- paste0("SELECT * FROM \"HUC10\" where HUC10 IN (", paste(sQuote(hucList), collapse = ", ", sep = "")," )")
rng <- st_read(nm_HUC_pth, nm_HUC_file,
               query = qry)
options(op)
rm(op)

sa <- st_union(rng)
# 

# background points shapefile (points are generated within this boundary). 
# this should be in the project's projection (matching the raster EVs)

setwd(here("_data","other_spatial","feature"))
states <- st_read("US_States.shp")
#study area is a dissolved representation of CONUS
sa <- st_union(states)

# huc12s. These polygons are used to attribute points. Must have "HUC_12" column.
huc12 <- "N:/rangestuff/HUC_ref/huc12_fromChris/huc12.shp"

# number of points to generate
numpts <- 10000

# new/existing db table name (overwritten)
table <- "background_pts"

# path to background points shapefile
pathToPts <- here("_data","env_vars","background")
# path to output tables (a database (background.sqlite) is generated here if it doesn't exist)
pathToTab <- here("_data","env_vars","tabular")

# load layers
#sa <- st_read(StudyAreaPoly)
crs <- st_crs(sa)
huc <- st_read(huc12)

if (crs != st_crs(huc)){
  huc <- st_transform(st_read(huc12), crs)  
}

# generate points
samps1 <- st_sample(sa, size = numpts)
samps1 <- st_sf(fid = 1:length(samps1), geometry = samps1)
samps <- st_join(samps1, huc, join = st_intersects)[c("fid", "HUC_12")]
names(samps) <- c("fid", "huc12", "geometry")

# create DF
sampsDF <- data.frame(fid = samps$fid, huc12 = samps$huc12, wkt = st_as_text(samps$geometry))

# send to database
db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background_conus.sqlite"))
db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background_pletasup.sqlite"))
tp <- as.vector("INTEGER")
names(tp) <- "fid"
dbWriteTable(db, table, sampsDF, overwrite = T, field.types = tp)

# ### sample another 10 million, add to the first batch:
# numpts <- 10000000
# 
# # generate points
# samps1 <- st_sample(sa, size = numpts)
# samps1 <- st_sf(fid = 1:length(samps1), geometry = samps1)
# samps <- st_join(samps1, huc, join = st_intersects)[c("fid", "HUC_12")]
# names(samps) <- c("fid", "huc12", "geometry")
# 
# # create DF
# sampsDF <- data.frame(fid = samps$fid, huc12 = samps$huc12, wkt = st_as_text(samps$geometry))
# 
# dbWriteTable(db, table, sampsDF, append = TRUE)




# write CRS
tcrs <- data.frame(table_name = table, proj4string = as.character(crs$proj4string))
try(dbExecute(db, paste0("DELETE FROM lkpCRS where table_name = '", table, "';")), silent = T)
dbWriteTable(db, "lkpCRS", tcrs, append = T)

# also write to shapefile
st_write(samps, paste0(pathToPts, "/", table, "amazviri_RanPts.shp"), delete_layer = T)


dbDisconnect(db)
rm(db)
