# File: 1_pointsInPolys_cleanBkgPts.r
# Purpose: 
# 1. Sampling of EDM polygons to create random points within the polygons
#  these are the random presence points being created here, from polygon presence data.
# 2. Removing any points from the background points dataset that overlap or are near
#  the input presence polygon dataset.

library(RSQLite)

####
# Assumptions
# - the shapefile is named with the species code that is used in the lookup table
#   e.g. glypmuhl.shp
# - There is lookup data in the sqlite database to link to other element information (full name, common name, etc.)
# - the polygon shapefile has at least these fields EO_ID_ST, SNAME, SCOMNAME, RA

####
#### load input poly ----

###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 38 where you choose which polygon file to use

loc_scripts <- "E:/SDM/Aquatic/scripts/Regional_SDM"
source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))

setwd(loc_spCatchment)

#get a list of what's in the directory
fileList <- dir( pattern = ".csv$")
fileList

#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

# load data, QC ----
fileName <- fileList[[n]]
presCatchments <- read.csv(fileName)

shpColNms <- names(presCatchments)
desiredCols <- c("EO_ID_ST", "SNAME", "SCOMNAME", "RA","COMID")  #drop RA
if("FALSE" %in% c(desiredCols %in% shpColNms)) {
	  stop("at least one column is missing or incorrectly named")
  } else {
    print("Required columns are present")
  }

#pare down columns
presCatchments <- presCatchments[,desiredCols]

#get the attribute table from above 
att.pt <- presCatchments

# just in case convert to lower
names(att.pt) <- tolower(names(att.pt))

write.csv(att.pt,"lasmcomp_prepped.csv")

# Write out various stats and data to the database ------
# prep the data
OutPut <- data.frame(SciName = paste(att.pt[1,"sname"]),
	CommName=paste(att.pt[1,"scomname"]),
	#ElemCode=sppCode,
	#RandomPtFile=nm.RanPtFile, # do we need this?
	date = paste(Sys.Date()),
	time = format(Sys.time(), "%X"),
	Loc_Use=""
	)

#Write the data to the SQLite database
db <- dbConnect(SQLite(),dbname=nm_db_file)
dbWriteTable(db,"tblPrepStats",OutPut,append=TRUE)
dbDisconnect(db)

# remove catchments from background that have presence points
list_presCatchments <- att.pt$comid



#list_projCatchments <- att.pt$comid

setwd(loc_envVars)

bgpoints <- read.csv("EnvVars.csv") #may need additional code for field types

selectedRows <- (bgpoints$COMID %in% list_projCatchments)

bgpoints_cleaned <- bgpoints[selectedRows,]
selectedRows <- !(bgpoints_cleaned$COMID %in% list_presCatchments)
bgpoints_cleaned <- bgpoints_cleaned[selectedRows,]

names(bgpoints_cleaned) <- tolower(names(bgpoints_cleaned))

setwd(loc_bkgReach)
write.csv(bgpoints_cleaned,"bgpoints_clean.csv")

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())

