# File: 1_pointsInPolys_cleanBkgPts.r
# Purpose: 
# 1. Check input presence reach dataset for missing columns or data
# 2. Removing any reaches from the background dataset that are adjacent to
#  the input presence reach dataset.

library(RSQLite)
library(rgdal)
library(rgeos)
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
setwd(paste0(loc_model,"/",model_species,"/inputs/presence"))
# changing to this WD temporarily allows for presence file to be either in presence folder or specified with full path name

# load data, QC ----
fileName <- basename(nm_presFile)
presReaches <- read.csv(nm_presFile, colClasses = c("huc12"="character"))

shpColNms <- names(presReaches)
desiredCols <- c("EO_ID_ST", "SNAME", "SCOMNAME", "COMID", "OBSDATE","group_id","huc12") 

# check if all required names are in file
if("FALSE" %in% c(desiredCols %in% shpColNms)) {
  stop(paste0("Column(s) are missing or incorrectly named: ", paste(desiredCols[!desiredCols %in% shpColNms], collapse = ", ")))
} else {
  print("Required columns are present")
}
# check if all columns have complete data
if(any(!complete.cases(presReaches[c("EO_ID_ST", "SNAME", "SCOMNAME", "COMID","group_id","huc12")]))) {
  stop("The columns 'EO_ID_ST', 'SNAME', 'SCOMNAME', 'COMID','huc12', and 'group_id' cannot have NA values.")
}

# check if file already exists; if it does, stop and print error
if (!file.copy(nm_presFile, paste0(baseName, ".csv"))) {
  stop("A file already exists with that name: '", 
       paste0(getwd(), "/", baseName, ".csv"), "'. Rename or delete it to continue.")
}
# set wd to inputs again
setwd(paste0(loc_model,"/",model_species,"/inputs"))
# file in place, read it
presReaches <- read.csv(paste0("presence/", baseName, ".csv"), colClasses = c("huc12"="character"))

# arrange, pare down columns
presReaches <- presReaches[,desiredCols]
# subset background reaches by HUC2 to prevent predictions into basics where the species is not known to occur
presReaches$huc12 <- str_pad(presReaches$huc12, 12, pad=0) # make sure huc12 values have leading zeros

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

#get the attribute table from above 
att.reaches <- presReaches

# just in case convert column names to lowercase
names(att.reaches) <- tolower(names(att.reaches))

# Write out various stats and data to the database ------
# prep the data
OutPut <- data.frame(tableCode = baseName,
  SciName = paste(att.reaches[1,"sname"]),
	CommName=paste(att.reaches[1,"scomname"]),
	ElemCode=model_species,
	RandomPtFile= paste0(getwd(), "/presence/", fileName), # do we need this?
	date = paste(Sys.Date()),
	time = format(Sys.time(), "%X"),
	Loc_Use=""
	)

#Write the data to the SQLite database
db <- dbConnect(SQLite(),dbname=nm_db_file)
dbWriteTable(db,"tblPrepStats",OutPut,append=TRUE)
dbDisconnect(db)

###
# remove reaches from background dataset that have presence of the target species in the reach
list_presReaches <- att.reaches$comid

# setwd(loc_otherSpatial)
# read in the shapefile, get the attribute data
layer <- strsplit(basename(nm_allflowlines),"\\.")[[1]][[1]]
layerdir <- dirname(nm_allflowlines)
shapef <- readOGR(layerdir, layer = layer)
testcatchments <- shapef@data
names(testcatchments) <- tolower(names(testcatchments))
testcatchments$huc12 <- str_pad(testcatchments$huc12, 12, pad=0)

# define project background
if (!is.null(huc_level)) {
  # subset to huc if requested
  HUCsubset <- unique(substr(presReaches$huc12, 1, huc_level)) # subset to number of huc digits
  list_projCatchments <- testcatchments$comid[substr(testcatchments$huc12,1,huc_level) %in% HUCsubset]
} else {
  # otherwise take all reaches
  list_projCatchments <- testcatchments$comid
}

# find presence and presence-adjacent reaches by intersection
pres.geom <- shapef[shapef$comid %in% list_presReaches,]
bkgd.int <- gIntersects(pres.geom, shapef, byid = TRUE, returnDense = TRUE)
bkgd.int <- apply(bkgd.int, 1, FUN = any)
bkgd.int <- names(bkgd.int)[as.vector(bkgd.int)]
bkgd.int <- shapef[row.names(shapef) %in% bkgd.int,]
list_removeBkgd <- bkgd.int$comid

bgpoints <- read.csv(nm_envVars, colClasses=c("huc12"="character"))
names(bgpoints) <- tolower(names(bgpoints))
bgpoints$huc12 <- str_pad(bgpoints$huc12, 12, pad=0)

selectedRows <- (bgpoints$comid %in% list_projCatchments & !(bgpoints$comid %in% list_removeBkgd)) 
bgpoints_cleaned <- bgpoints[selectedRows,] # selects rows by comid in list of project reaches, and also not bordering presence reaches

# write species reach data
write.csv(att.reaches,paste("model_input/", baseName,"_prepped.csv",sep=""), row.names = FALSE) 
# wtite background reach data
write.csv(bgpoints_cleaned, paste("model_input/", baseName,"_bgpoints_clean.csv",sep=""), row.names = FALSE)
