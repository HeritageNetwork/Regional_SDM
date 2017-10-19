# File: 1_pointsInPolys_cleanBkgPts.r
# Purpose: 
# 1. Sampling of EDM polygons to create random points within the polygons
#  these are the random presence points being created here, from polygon presence data.
# 2. Removing any points from the background points dataset that overlap or are near
#  the input presence polygon dataset.

library(RSQLite)
library(rgdal)
library(rgeos)

####
# Assumptions
# - the csv is named with the species code that is used in the lookup table (e.g. glypmuhl.shp)
# - There is lookup data in the sqlite database to link to other element information (full name, common name, etc.)
# - the csv has at least these fields EO_ID_ST, SNAME, SCOMNAME, RA

####
#### load input reaches ----
###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 38 where you choose which polygon file to use


# set the working directory to the location of the csv of species by reaches
setwd(loc_spReaches)

#get a list of what's in the directory
fileList <- dir( pattern = ".csv$")
fileList

#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

# load data, QC ----
fileName <- fileList[[n]]
shpName <- strsplit(fileName,"\\.")[[1]][[1]]
sppCode <- shpName
presReaches <- read.csv(fileName, colClasses = c("huc12"="character"))

shpColNms <- names(presReaches)
desiredCols <- c("EO_ID_ST", "SNAME", "SCOMNAME", "COMID", "OBSDATE","group_id") 

if("FALSE" %in% c(desiredCols %in% shpColNms)) {
  stop(paste0("Column(s) are missing or incorrectly named: ", paste(desiredCols[!desiredCols %in% shpColNms], collapse = ", ")))
} else {
  print("Required columns are present")
}
if(any(!complete.cases(presReaches[c("EO_ID_ST", "SNAME", "SCOMNAME", "COMID","group_id")]))) {
  stop("The columns 'EO_ID_ST', 'SNAME', 'SCOMNAME', 'COMID', and 'group_id' cannot have NA values.")
}

#pare down columns
presReaches <- presReaches[,desiredCols]

# set date/year column to [nearest] year, rounding when day is given
presReaches$date <- as.numeric(substr(presReaches$OBSDATE, 1, 4))
try({
roundUpYear <- format(as.Date(presReaches$OBSDATE), "%j")
roundUpYear <- ifelse(roundUpYear < 183 | is.na(roundUpYear), 0, 1)
}, silent = TRUE)
if(exists("roundUpYear")) {
  presReaches$date <- presReaches$date + roundUpYear
  rm(roundUpYear)
}
desiredCols <- c(desiredCols, "date")

#get the attribute table from above 
att.reaches <- presReaches

# just in case convert column names to lowercase
names(att.reaches) <- tolower(names(att.reaches))

#write out the CSV file
# moved to end of script, to attach huc12 ids
# write.csv(att.reaches,paste(sppCode,"_prepped.csv",sep=""), row.names = FALSE) 

# Write out various stats and data to the database ------
# prep the data
OutPut <- data.frame(SciName = paste(att.reaches[1,"sname"]),
	CommName=paste(att.reaches[1,"scomname"]),
	ElemCode=sppCode,
	RandomPtFile= "NA/Aquatic", # do we need this?
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

setwd(loc_otherSpatial)
StudyAreaReaches <- nm_allflowlines # the name of the study area flowlines
# read in the shapefile, get the attribute data
layer <- strsplit(StudyAreaReaches,"\\.")[[1]][[1]]
shapef <- readOGR(loc_otherSpatial, layer = layer)
testcatchments <- shapef@data
names(testcatchments) <- tolower(names(testcatchments))
list_projCatchments <- testcatchments$comid

# find presence and presence-adjacent reaches
pres.geom <- shapef[shapef$comid %in% list_presReaches,]
bkgd.int <- gIntersects(pres.geom, shapef, byid = TRUE, returnDense = TRUE)
bkgd.int <- apply(bkgd.int, 1, FUN = any)
bkgd.int <- names(bkgd.int)[as.vector(bkgd.int)]
bkgd.int <- shapef[row.names(shapef) %in% bkgd.int,]
list_removeBkgd <- bkgd.int$comid

setwd(loc_envVars)
bgpoints <- read.csv("EnvVars.csv")
names(bgpoints) <- tolower(names(bgpoints))
selectedRows <- (bgpoints$comid %in% list_projCatchments & !(bgpoints$comid %in% list_removeBkgd))
bgpoints_cleaned <- bgpoints[selectedRows,]

# write species reach data with huc12 IDS
setwd(loc_spReaches)
# att.reaches <- merge(att.reaches, testcatchments[c("comid","huc12")], by = "comid")
write.csv(att.reaches,paste(sppCode,"_prepped.csv",sep=""), row.names = FALSE) 

# wtite background reach data with huc12 IDS
setwd(loc_bkgReach)
# bgpoints_cleaned <- merge(bgpoints_cleaned, testcatchments[c("comid","huc12")], by = "comid")
write.csv(bgpoints_cleaned,"bgpoints_clean.csv", row.names = FALSE)
