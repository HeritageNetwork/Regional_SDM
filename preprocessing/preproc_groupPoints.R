# File: preproc_groupPoints.r
# Purpose: If we are working with observation data (not EO data), at least in part
#  then the observations need to be grouped spatially. 
#  
#  This script assumes these clean up tasks are already done:
#   - clean points/polys to make sure they are checked and cleaned with respect to
        # - range (as defined by HUC rangese)
        # - the chosen date threshold
        # - appropriate precision/accuracy
        # - appropriate type for appropriate species (e.g. only polys for G1, G2 if enough of them)

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

cutecode <- "scirlong"

# initialize in case repeatedly running
havePolyData <- FALSE
havePointData <- FALSE

# check for poly data
eo_name <- paste0(cutecode, ".shp")

if(file.exists(eo_name)){
  eo <- st_read(eo_name, stringsAsFactors = FALSE)
  eo_dat <- st_transform(eo, crs_aea)
  if(nrow(eo_dat) > 0 ){
    havePolyData <- TRUE  
  }
}

# check for point data
obd_name <- paste0(cutecode, "_pt.shp")

if(file.exists(obd_name)){
  obd <- st_read(obd_name, stringsAsFactors = FALSE)
  pt_dat <- st_transform(obd, crs_aea)
  if(nrow(pt_dat) > 0 ){
    havePointData <- TRUE  
  }
}

# process data given three options: points and polys, points only, polys only
if(havePointData){
  # double check units
  #st_crs(pt_dat)$units 
  # Buffer by those units (m), merge them
  dat_buff1km <- st_buffer(pt_dat, 1000)
  grps <- st_union(dat_buff1km, by_feature = FALSE)
  
  # convert to single-part, add attribute table
  grps = st_cast(grps, "POLYGON")
  df <- data.frame("grp_id" = c(1:length(grps)))
  grps <- st_set_geometry(df, grps)
  
  # attribute points with group ID
  dat_grps <- suppressWarnings(st_intersection(pt_dat, grps))
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
  
  if(havePolyData){
    ## have point and poly data
    #join it with the polys shpfile
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
    
  } else {
    ## have point data only
    # move the existing (point and poly) files to the pre-processed folder
    filesToMove <- list.files(pattern = cutecode)
    file.rename(from = filesToMove, to = paste0("pre-processed/",filesToMove))
    
    # write out the merged, all-poly shapefile
    fullnm <- paste0(cutecode,".shp")
    st_write(eos_grp, fullnm)
  }
} else {
  ## have poly data only 
  # don't need to do anything!
  print("only poly data")
}


