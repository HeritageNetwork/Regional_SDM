# File: 1_pointsInPolys_cleanBkgPts.r
# Purpose: 
# 1. Check input presence reach dataset for missing columns or data
# 2. Removing any reaches from the background dataset that are adjacent to
#  the input presence reach dataset.

library(RSQLite)
library(rgdal)
library(rgeos)

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
desiredCols <- c("EO_ID_ST", "SNAME", "SCOMNAME", "COMID", "OBSDATE","group_id","huc12") 

# check if all required names are in file
if("FALSE" %in% c(desiredCols %in% shpColNms)) {
  stop(paste0("Column(s) are missing or incorrectly named: ", paste(desiredCols[!desiredCols %in% shpColNms], collapse = ", ")))
} else {
  print("Required columns are present")
}
# check if all columns have complete data
if(any(!complete.cases(presReaches[c("EO_ID_ST", "SNAME", "SCOMNAME", "COMID","group_id")]))) {
  stop("The columns 'EO_ID_ST', 'SNAME', 'SCOMNAME', 'COMID', and 'group_id' cannot have NA values.")
}

# arrange, pare down columns
presReaches <- presReaches[,desiredCols]

# set date/year column to [nearest] year, rounding when day is given
presReaches$OBSDATE <- as.character(presReaches$OBSDATE)
presReaches$date <- NA
for (d in 1:length(presReaches$OBSDATE)) {
  if (grepl("^[0-9]{4}.{0,3}$", presReaches$OBSDATE[d])) {
    dt <- as.numeric(substring(presReaches$OBSDATE[d],1,4))
  } else {
    if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", presReaches$OBSDATE[d])) {
      dt <- as.Date(presReaches$OBSDATE[d])
    } else if (grepl("^[0-9]+/[0-9]+/[0-9]+", presReaches$OBSDATE[d])) {
      dt <- as.Date(presReaches$OBSDATE[d], format = "%m/%d/%Y") 
    } else {
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

# find presence and presence-adjacent reaches by intersection
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

# write species reach data
setwd(loc_spReaches)
write.csv(att.reaches,paste(sppCode,"_prepped.csv",sep=""), row.names = FALSE) 

# wtite background reach data
setwd(loc_bkgReach)
write.csv(bgpoints_cleaned,"bgpoints_clean.csv", row.names = FALSE)
