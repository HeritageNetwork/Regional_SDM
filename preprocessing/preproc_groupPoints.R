# File: preproc_groupPoints.r
# Purpose: If we are working with observation data (not EO data), at least in part
#  then the observations need to be grouped spatially. This script should help do that.
#  

#
library(sf)
library(dplyr)
library(RSQLite)
library(here)

loc_scripts <- here()

### get a correct crs, to apply to get correct projections
x <- st_read(paste0(loc_scripts,"/_data/other_spatial/feature/US_States.shp"))
crs_aea <- st_crs(x)

# observation data
path <- paste0(loc_scripts,"/_data/occurrence")
setwd(path)

cutecode <- "dichhirs"

obd_name <- paste0(cutecode, "_pt.shp")

obd <- st_read(obd_name, stringsAsFactors = FALSE)
dat <- st_transform(obd, crs_aea)

# confirm points are only within the range polygon
# # get range info from the DB (as a list of HUCs)
# nm_db_file <- paste0(loc_scripts,"/_data/databases/SDM_lookupAndTracking.sqlite")
# model_species <- obd$SPECIES_CD[[1]]
# nm_HUC_file <- paste0(loc_scripts,"/_data/other_spatial/feature/HUC10.shp")
# 
# db <- dbConnect(SQLite(),dbname=nm_db_file)
# SQLquery <- paste0("SELECT huc10_id from lkpRange
#                    inner join lkpSpecies on lkpRange.EGT_ID = lkpSpecies.EGT_ID
#                    where lkpSpecies.sp_code = '", model_species, "';")
# hucList <- dbGetQuery(db, statement = SQLquery)$huc10_id
# dbDisconnect(db)
# rm(db)
# 
# # now get that info spatially
# nm_range <- nm_HUC_file
# qry <- paste("SELECT * from HUC10 where HUC10 IN ('", paste(hucList, collapse = "', '"), "')", sep = "")
# hucRange <- st_zm(st_read(nm_range, query = qry))
# 
# # intersect observations with range to get only points within range polys
# dat_rng <- st_intersection(dat, hucRange[,c("HUC10")])
# nrow(dat_rng)

dat<-dat_rng
# group by buffering (ideally buff distance would be sep dist)
# make sure distance is in the appropriate units
st_crs(dat_rng)$units 

dat_buff1km <- st_buffer(dat_rng, 1000)

#### want to try clustering?  see
## https://stackoverflow.com/questions/28672399/spatial-clustering-in-r-simple-example

grps <- st_union(dat_buff1km, by_feature = FALSE)
# convert to single-part, add attribute table
grps = st_cast(grps, "POLYGON")
df <- data.frame("grp_id" = c(1:length(grps)))
grps <- st_set_geometry(df, grps)

# attribute points with group ID
dat_grps <- st_intersection(dat_rng, grps)
table(dat_grps$grp_id)

# finally, convert to 'eo' or level polys, by buffering by smaller distance
# assume that actual point may have some accuracy issues so 
# make large enough to include closest adjacent cells
dat_eo <- st_buffer(dat_grps, 90)
# merge overlapping polys
eos <- st_union(dat_eo, by_feature = FALSE)
# convert to single-part, add attribute table
eos = st_cast(eos, "POLYGON")
eodf <- data.frame("eo_id" = c(1:length(eos)))
eos <- st_set_geometry(eodf, eos)

# get back groups AND date (using the most recent obs date for each EO)
eos_dat <- st_join(eos, dat_eo, join = st_intersects, left = TRUE)

eos_dat$RA <- "medium"

eos_grp <- aggregate(eos_dat[,"OBSDATE"], 
                     by = list(eos_dat$SPECIES_CD, eos_dat$grp_id, eos_dat$eo_id, eos_dat$RA),
                     FUN = function(x){ max(x)}
)
geomName <- names(eos_grp)[length(names(eos_grp))]
names(eos_grp) <- c("SPECIES_CD","GROUP_ID","UID","RA","OBSDATE",geomName)

#join it with the polys shpfile
# get the EO data 
eo_name <- paste0(cutecode, ".shp")
eo <- st_read(eo_name, stringsAsFactors = FALSE)
eo_dat <- st_transform(eo, crs_aea)

# get the max value for group ID; UID
max_gpid <- max(eo_dat$GROUP_ID)
max_uid <- max(eo_dat$UID)

# extend values in the point dataset to make everything unique
eos_grp$UID <- eos_grp$UID + max_uid
eos_grp$GROUP_ID <- eos_grp$GROUP_ID + max_gpid

# subset eo_dat, then merge the two
geomName <- names(eo_dat)[length(names(eo_dat))]
eo_dat <- eo_dat[,c("SPECIES_CD","GROUP_ID","UID","RA","OBSDATE",geomName)]

eo_dat_all <- rbind(eo_dat, eos_grp)

# move the existing (point and poly) files to the pre-processed folder
filesToMove <- list.files(pattern = cutecode)
file.rename(from = filesToMove, to = paste0("pre-processed/",filesToMove))

# write out the merged, all-poly shapefile
fullnm <- paste0(cutecode,".shp")
st_write(eo_dat_all, fullnm)
