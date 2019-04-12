# File: 1_pointsInPolys_cleanBkgPts.r
# Purpose: 
# 1. Check input presence reach dataset for missing columns or data
# 2. Removing any reaches from the background dataset that are adjacent to
#  the input presence reach dataset.

library(RSQLite)
library(dplyr) # adding dplyr here as it may be causing downstream effects due to a bad interaction with 'sf'
library(sf)
library(stringr)


####
# Assumptions
# - the csv is named with the species code that is used in the lookup table (e.g. glypmuhl.shp)
# - There is lookup data in the sqlite database to link to other element information (full name, common name, etc.)
# - the csv has at least these fields EO_ID_ST, SNAME, SCOMNAME, COMID, OBSDATE, group_id, huc12

####
#### load input reaches ----
###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 38 where you choose which file to use


# set the working directory to the location of the csv of species by reaches
setwd(loc_model)
dir.create(paste0(model_species,"/inputs/presence"), recursive = T, showWarnings = F)
dir.create(paste0(model_species,"/inputs/model_input"), showWarnings = F)
setwd(paste0(loc_model,"/",model_species,"/inputs"))
# changing to this WD temporarily allows for presence file to be either in presence folder or specified with full path name

# load data, QC ----
presReaches <- read.csv(nm_presFile)

shpColNms <- names(presReaches)
desiredCols <- c("UID", "GROUP_ID", "SPECIES_CD", "COMID", "OBSDATE") # , "SNAME", "SCOMNAME", 

# check if all required names are in file
if("FALSE" %in% c(desiredCols %in% shpColNms)) {
  stop(paste0("Column(s) are missing or incorrectly named: ", paste(desiredCols[!desiredCols %in% shpColNms], collapse = ", ")))
} else {
  print("Required columns are present")
}

# check if all columns have complete data
if(any(is.na(presReaches[c("UID", "GROUP_ID", "SPECIES_CD", "COMID")]))) {   # "SNAME", "SCOMNAME", 
  stop("The columns 'UID', 'GROUP_ID', 'SPECIES_CD', and 'COMID' cannot have NA values.")
}

# check if file already exists; if it does, stop and print error
#if (!file.copy(nm_presFile, paste0(baseName, ".csv"))) {
#  stop("A file already exists with that name: '", 
#       paste0(getwd(), "/", baseName, ".csv"), "'. Rename or delete it to continue.")
#}
# set wd to inputs again
# setwd(paste0(loc_model,"/",model_species,"/inputs"))
# file in place, read it
#presReaches <- read.csv(paste0("presence/", baseName, ".csv"))

# arrange, pare down columns
presReaches <- presReaches[,desiredCols]

# set date/year column to [nearest] year, rounding when day is given
presReaches$OBSDATE <- as.character(presReaches$OBSDATE)
presReaches$date <- NA
for (d in 1:length(presReaches$OBSDATE)) {
  dt <- NA
  do <- presReaches$OBSDATE[d]
  if (grepl("^[0-9]{4}.{0,3}$", do)) {
    # year only formats
    dt <- as.numeric(substring(do,1,4))
  } else {
    if (grepl("^[0-9]{4}[-|/][0-9]{1,2}[-|/][0-9]{1,2}", do)) {
      # ymd formats
      try(dt <- as.Date(do), silent = TRUE)
    } else if (grepl("^[0-9]{1,2}[-|/][0-9]{1,2}[-|/][0-9]{4}", do)) {
      # mdy formats
      try(dt <- as.Date(do, format = "%m/%d/%Y"), silent = TRUE)
    }
    # if still no match, or if failed
    if (is.na(dt)) {
      if (grepl("[0-9]{4}", do)) {
        # use first 4-digit sequence as year
        dt <- regmatches(do, regexpr("[0-9]{4}", do))
        if (as.integer(dt) < 1900 | as.integer(dt) > format(Sys.Date(), "%Y")) {
          # years before 1900 and after current year get discarded
          dt <- Sys.Date()
        } else {
          dt <- as.Date(paste0(dt,"-01-01"))
        }
      }
      # put additional date formats here
    }
    # give up and assign current date
    if (is.na(dt)) {
      dt <- Sys.Date()
    }
    dt <- round(as.numeric(format(dt, "%Y")) + (as.numeric(format(dt,"%j"))/365.25))
  }
  presReaches$date[d] <- dt
}
desiredCols <- c(desiredCols, "date")

# just in case convert column names to lowercase
names(presReaches) <- tolower(names(presReaches))

###
# remove reaches from background dataset that have presence of the target species in the reach

# read in the shapefile, get the attribute data
dbEV <- dbConnect(SQLite(),dbname=nm_bkg[1])
SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2]," WHERE COMID IN ('", paste(presReaches$comid, collapse = "','"),"')") 
shapef <- dbGetQuery(dbEV, SQLQuery)
SQLQuery <- paste0("SELECT proj4string p FROM lkpCRS WHERE table_name = '", nm_bkg[2], "';") 
proj4 <- dbGetQuery(dbEV, SQLQuery)$p
# shapef <- st_read(nm_allflowlines)
names(shapef) <- tolower(names(shapef))
shapef <- st_sf(shapef[c("comid", "huc12","wacomid")], geometry=st_as_sfc(shapef$wkt), crs=proj4)
# testcatchments <- shapef@data
shapef$huc12 <- str_pad(shapef$huc12, 12, pad=0)

# get huc12s, geom
pres.geom <- merge(shapef, presReaches, by = "comid")

# define project background
# subset background reaches by HUC2 to prevent predictions into basics where the species is not known to occur
presHUCs <- pres.geom$huc12

# test at what level HUCS are the same, and choose that level to run the predictions at.  
# For example, if all know occurences are within the same HUC6, then the study area will be clipped to that HUC6. 
# If they are not same at any level, then the model will be run at the full extant of the predictor layer.  
# THis is used to define the project background below.
if (is.null(huc_level)) {
  if(length(unique(substr(presHUCs,1,8)))==1){
    huc_level <- 8
  } else if(length(unique(substr(presHUCs,1,6)))==1){
    huc_level <- 6  
  } else if(length(unique(substr(presHUCs,1,4)))==1){
    huc_level <- 4  
  } else if(length(unique(substr(presHUCs,1,2)))==1){
    huc_level <- 2 
  } else {
    huc_level <- 4 # changed from 2 to try to narrow up the prediction area
  }
  fn_args$huc_level <- huc_level
  save(fn_args, file = paste0(loc_model, "/" , model_species, "/runSDM_paths.Rdata"))
}
message("Using huc_level of ", huc_level , "...")

# create background geom based on HUCsubset
dbEV <- dbConnect(SQLite(),dbname=nm_bkg[1])
if (huc_level != 0) {
  # subset to huc if requested
  HUCsubset <- unique(substr(presHUCs, 1, huc_level)) # subset to number of huc digits
  SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2]," WHERE substr(huc12, 1, ", huc_level, ") IN ('", paste(HUCsubset, collapse = "','"),"');") 
} else {
  # otherwise take all reaches
  SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2],";") 
}
shapef <- dbGetQuery(dbEV, SQLQuery)
names(shapef) <- tolower(names(shapef))
SQLQuery <- paste0("SELECT proj4string p FROM lkpCRS WHERE table_name = '", nm_bkg[2], "';") 
proj4 <- dbGetQuery(dbEV, SQLQuery)$p
shapef <- st_sf(shapef[c("comid", "huc12")], geometry = st_as_sfc(shapef$wkt), crs = proj4)

# find presence and presence-adjacent reaches by intersection
bkgd.int <- st_intersects(st_zm(shapef), st_zm(pres.geom) , sparse = F)
bkgd.geom <- shapef[!apply(bkgd.int, 1, FUN = any),]
if (length(bkgd.geom$geometry) > 3000) { 
  bkgd.geom <- bkgd.geom[sort(sample(as.numeric(row.names(bkgd.geom)), size = 3000, replace = F)),]
}

# write species reach data
st_write(pres.geom,paste("presence/", baseName,"_prepped.shp",sep=""))
# wtite background reach data
st_write(bkgd.geom, paste("model_input/", baseName,"_bkgd_clean.shp",sep=""))
