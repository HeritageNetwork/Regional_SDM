# File: 1_pointsInPolys_cleanBkgPts.r
# Purpose: 
# 1. Sampling of EDM polygons to create random points within the polygons.
#  these are the random presence points being created here, from polygon presence data.
# 2. Removing any points from the background points dataset that overlap or are near
#  the input presence polygon dataset.
library(here)
library(RSQLite)
#library(rgdal)
#library(sp)
#library(rgeos)
library(raster)
library(sf)
library(dplyr)


####
# Assumptions
# - the shapefile is named with the species code that is used in the lookup table
#   e.g. glypmuhl.shp
# - There is lookup data in the sqlite database to link to other element information (full name, common name, etc.)
# - the polygon shapefile has at least these fields EO_ID_ST, SNAME, SCOMNAME, RA

####
#### load input polys ----

###
# if using user_run_SDM.R, then loc_spPoly will be present and this should run cleanly,
# otherwise, get settings from the 0 script.

if(exists("loc_spPoly")) {
  setwd(loc_spPoly)
} else {
  source(here("0_pathsAndSettings.R"))
}

# set up folder system for inputs
dir.create(here("_data/species",model_species,"/inputs/presence"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("_data/species",model_species,"/inputs/model_input"), showWarnings = FALSE)
#setwd(here("_data/species",model_species,"inputs/presence"))
# changing to this WD temporarily allows for presence file to be either in presence folder or specified with full path name


#get a list of what's in the directory
fileList <- dir(here("_data/inputs"), pattern = ".shp$")

#find a match for the target species (from 0_pathsAndSettings.R)
polysFile <- fileList[match(nm_presPolys, fileList)]
ptsFile <- fileList[match(nm_presPts, fileList)]

if(is.na(ptsFile)){
  print("no match for the points file")
}

if(is.na(polysFile)){
  print("no match for the polygon file")
}

# load data, QC ----
#fileName <- fileList[[n]]
#shpName <- strsplit(fileName,"\\.")[[1]][[1]]
#sppCode <- shpName

presPolys <- st_read(here("_data/inputs",polysFile))

#presPolys <- readOGR(fileName, layer = shpName) #Z-dimension discarded msg is OK
#check for proper column names. If no error from next code block, then good to go
#presPolys$RA <- presPolys$SFRACalc
shpColNms <- names(presPolys)
desiredCols <- c("EO_ID_ST", "SNAME", "SCOMNAME", "RA", "OBSDATE")
if("FALSE" %in% c(desiredCols %in% shpColNms)) {
	  stop(paste0("Column(s) are missing or incorrectly named: ", paste(desiredCols[!desiredCols %in% shpColNms], collapse = ", ")))
  } else {
    print("Required columns are present")
  }
if(any(is.na(presPolys[,c("EO_ID_ST", "SNAME", "SCOMNAME", "RA")]))) {
  stop("The columns 'EO_ID_ST','SNAME','SCOMNAME', and 'RA' (SFRACalc) cannot have NA values.")
}

#pare down columns
presPolys <- presPolys[,desiredCols]

# set date/year column to [nearest] year, rounding when day is given
presPolys$OBSDATE <- as.character(presPolys$OBSDATE)
presPolys$date <- NA
for (d in 1:length(presPolys$OBSDATE)) {
  dt <- NA
  do <- presPolys$OBSDATE[d]
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
  presPolys$date[d] <- dt
}
desiredCols <- c(desiredCols, "date")

# explode multi-part polys ----
suppressWarnings(shp_expl <- st_cast(presPolys, "POLYGON"))

#add some columns (explode id and area)
shp_expl <- cbind(shp_expl, 
                       EXPL_ID = rownames(shp_expl), 
                       AREAM2 = st_area(shp_expl))


#write out the exploded polygon set
nm.PyFile <- here("_data/species",model_species,"inputs/presence",paste(model_species, "_expl.shp", sep = ""))
st_write(shp_expl, nm.PyFile, driver="ESRI Shapefile", delete_layer = TRUE)

####
####  Placing random points within each sample unit (polygon/EO) ----
####

# just in case convert to lower
names(shp_expl) <- tolower(names(shp_expl))

#calculate Number of points for each poly, stick into new field
shp_expl$PolySampNum <- round(400*((2/(1+exp(-(as.numeric(shp_expl$aream2)/900+1)*0.004)))-1))
#make a new field for the design, providing a stratum name
shp_expl <- cbind(shp_expl, "stratum" = paste("poly_",shp_expl$expl_id, sep=""))

# sample must be equal or larger than the RA sample size in the random forest model
shp_expl$ra <- factor(tolower(as.character(shp_expl$ra)))

# QC step: are any records attributed with values other than these?
raLevels <- c("very high", "high", "medium", "low", "very low")
if("FALSE" %in% c(shp_expl$ra %in% raLevels)) {
  stop("at least one record is not attributed with RA appropriately")
} else {
  print("RA levels attributed correctly")
}

#EObyRA <- unique(shp_expl[,c("expl_id", "eo_id_st","ra")])
shp_expl$minSamps[shp_expl$ra == "very high"] <- 5
shp_expl$minSamps[shp_expl$ra == "high"] <- 4
shp_expl$minSamps[shp_expl$ra == "medium"] <- 3
shp_expl$minSamps[shp_expl$ra == "low"] <- 2
shp_expl$minSamps[shp_expl$ra == "very low"] <- 1

# att.pt.2 <- merge(x = shp_expl, y = EObyRA[,c("expl_id","minSamps")], 
#                   all.x = TRUE, by.x = "expl_id", by.y = "expl_id")

shp_expl$finalSampNum <- ifelse(shp_expl$PolySampNum < shp_expl$minSamps, 
                                shp_expl$minSamps, 
                                shp_expl$PolySampNum)

# add two more cols for later (backwards compatibility with old GRTS routine)
#att.pt.2$stratum <- att.pt.2$panelNum
#att.pt.2$siteID <- paste(model_species, att.pt.2$expl_id, sep = "-")
# get these data into the spatial poly df
# shp_expl_dat <- merge(shp_expl, att.pt.2[,c("expl_id","finalSampNum","siteID","stratum")],
#                       by.x = "EXPL_ID", by.y = "expl_id")



ranPts <- st_sample(shp_expl, size = shp_expl$finalSampNum)
ranPts.sf <- st_sf(ranPts)
names(ranPts.sf) <- "geometry"
st_geometry(ranPts.sf) <- "geometry"

ranPts.joined <- st_join(ranPts.sf, shp_expl)

# check for polys that didn't get any points
polysWithNoPoints <- shp_expl[!shp_expl$EXPL_ID %in% ranPts.joined$EXPL_ID,]
if(nrow(polysWithNoPoints) > 0){
  stop("One or more polygons didn't get any points placed in them.")
}

## if you got polys with no points, read the numbers reported and 
## view the poly with this command
# plot(polysWithNoPoints[,])
# e.g. plot(polysWithNoPoints[], max.plot = 1)


#check for cases where sample smaller than requested
# how many points actually generated?
ptCount <- table(ranPts.joined$expl_id)

overUnderSampled <- ptCount - shp_expl$PolySampNum

#positive vals are oversamples, negative vals are undersamples
table(overUnderSampled)
# If you get large negative values then there are many undersamples and 
# exploration might be worthwhile

names(ranPts.joined) <- tolower(names(ranPts.joined))

colsToKeep <- c("stratum", tolower(desiredCols))
ranPts.joined <- ranPts.joined[,colsToKeep]

#name of random points output shapefile
nm.RanPtFile <- here("_data/species",model_species,"inputs/presence", paste(model_species, "_RanPts.shp", sep = ""))

st_write(ranPts.joined, nm.RanPtFile, driver="ESRI Shapefile", delete_layer = TRUE)




# #get the attribute table from above 
# att.pt <- shp_expl %>% st_set_geometry(NULL)
# 
# # just in case convert to lower
# names(att.pt) <- tolower(names(att.pt))
# 
# #calculate Number of points for each poly, stick into new field
# att.pt$PolySampNum <- round(400*((2/(1+exp(-(as.numeric(att.pt[,"aream2"])/900+1)*0.004)))-1))
# #make a new field for the design, providing a stratum name
# att.pt <- cbind(att.pt, "panelNum" = paste("poly_",att.pt$expl_id, sep=""))
# 
# # sample must be equal or larger than the RA sample size in the random forest model
# att.pt$ra <- factor(tolower(as.character(att.pt$ra)))


# 
# 
# # QC step: are any records attributed with values other than these?
# raLevels <- c("very high", "high", "medium", "low", "very low")
# if("FALSE" %in% c(att.pt$ra %in% raLevels)) {
#   stop("at least one record is not attributed with RA appropriately")
# } else {
#   print("RA levels attributed correctly")
# }
# 
# EObyRA <- unique(att.pt[,c("expl_id", "eo_id_st","ra")])
# EObyRA$minSamps[EObyRA$ra == "very high"] <- 5
# EObyRA$minSamps[EObyRA$ra == "high"] <- 4
# EObyRA$minSamps[EObyRA$ra == "medium"] <- 3
# EObyRA$minSamps[EObyRA$ra == "low"] <- 2
# EObyRA$minSamps[EObyRA$ra == "very low"] <- 1
# 
# att.pt.2 <- merge(x = att.pt, y = EObyRA[,c("expl_id","minSamps")], 
#                   all.x = TRUE, by.x = "expl_id", by.y = "expl_id")
# 
# att.pt.2$finalSampNum <- ifelse(att.pt.2$PolySampNum < att.pt.2$minSamps, 
#                                 att.pt.2$minSamps, 
#                                 att.pt.2$PolySampNum)
# 
# # add two more cols for later (backwards compatibility with old GRTS routine)
# att.pt.2$stratum <- att.pt.2$panelNum
# att.pt.2$siteID <- paste(model_species, att.pt.2$expl_id, sep = "-")
# # get these data into the spatial poly df
# shp_expl_dat <- merge(shp_expl, att.pt.2[,c("expl_id","finalSampNum","siteID","stratum")],
#                       by.x = "EXPL_ID", by.y = "expl_id")
# 
# 
# 
# ranPts <- st_sample(shp_expl_dat, size = shp_expl_dat$finalSampNum)
# ranPts.sf <- st_sf(ranPts)
# names(ranPts.sf) <- "geometry"
# st_geometry(ranPts.sf) <- "geometry"
# 
# ranPts.joined <- st_join(test.sf, shp_expl_dat)
# 
# # check for polys that didn't get any points
# polysWithNoPoints <- shp_expl_dat[!shp_expl_dat$EXPL_ID %in% ranPts.joined$EXPL_ID,]
# if(nrow(polysWithNoPoints) > 0){
#   stop("One or more polygons didn't get any points placed in them.")
# }
# 
# ## if you got polys with no points, read the numbers reported and 
# ## view the poly with this command
# # plot(polysWithNoPoints[,])
# # e.g. plot(polysWithNoPoints[], max.plot = 1)
# 
# 
# #check for cases where sample smaller than requested
# # how many points actually generated?
# ptCount <- table(ranPts.joined$EXPL_ID)
# 
# overUnderSampled <- ptCount - att.pt.2$PolySampNum
# 
# #positive vals are oversamples, negative vals are undersamples
# table(overUnderSampled)
# # If you get large negative values then there are many undersamples and 
# # exploration might be worthwhile
# 
# names(ranPts.joined) <- tolower(names(ranPts.joined))
# 
# colsToKeep <- c("siteid", "stratum", tolower(desiredCols))
# ranPts.joined <- ranPts.joined[,colsToKeep]
# 
# #name of random points output shapefile
# nm.RanPtFile <- here("_data/species",model_species,"inputs/presence", paste(model_species, "_RanPts.shp", sep = ""))
# 
# st_write(ranPts.joined, nm.RanPtFile, driver="ESRI Shapefile")

# Write out various stats and data to the database ------
# prep the data

OutPut <- data.frame(SciName = paste(att.pt[1,"sname"]),
	CommName=paste(att.pt[1,"scomname"]),
	ElemCode=sppCode,
	RandomPtFile=nm.RanPtFile,
	date = paste(Sys.Date()),
	time = format(Sys.time(), "%X"),
	Loc_Use=""
	)

#Write the data to the SQLite database
db <- dbConnect(SQLite(),dbname=nm_db_file)
dbWriteTable(db,"tblPrepStats",OutPut,append=TRUE)
dbDisconnect(db)

###
### remove Coincident Background points ----
###

# get the background shapefile
backgShapef <- st_read(here("_data/inputs",nm_bkgPts))

#backgShapef <- readOGR(dsn=loc_bkgPts, layer=nm_bkgPts)

#get projection info for later
#projInfo <- backgShapef@proj4string

# find coincident points ----
#buffer the poly shapefile 30 m
# polybuff <- spTransform(presPolys, projInfo) # transform just to be sure
# polybuff <- gBuffer(polybuff, width = 30)

polybuff <- st_transform(shp_expl, st_crs(backgShapef))
polybuff <- st_buffer(polybuff, dist = 30)

coincidentPts <- unlist(st_contains(polybuff, backgShapef, sparse = TRUE))

# remove them
backgSubset <- backgShapef[-coincidentPts,]


# # find points that fall within the buffered polygons, subset the sp object
# coincidentPts <- gContains(polybuff, backgShapef, byid = TRUE)
# colnames(coincidentPts) <- "insideBuff"
# backgShapef@data <- cbind(backgShapef@data, coincidentPts)
# backgSubset <- backgShapef[backgShapef@data$insideBuff == FALSE,]

# # projection info doesn't stick, apply from what we grabbed earlier
# backgSubset@proj4string <- projInfo

# write it out (note that `background` folder must exist in base species folder)---
# outFileName <- paste(nm_bkgPts, "_clean", sep="")
# writeOGR(backgSubset, dsn = "../background", layer = outFileName, 
#          driver="ESRI Shapefile", overwrite_layer=TRUE)

# strip .shp from name
nm_bkgPts_noshp <- sub("\\.shp","",nm_bkgPts)
outFileName <- here("_data/inputs",paste0(nm_bkgPts_noshp, "_clean.shp"))
st_write(backgSubset, outFileName, driver="ESRI Shapefile", delete_layer = TRUE)


