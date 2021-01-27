# File: 1_pointsInPolys_cleanBkgPts.r
# Purpose: 
# 1. Sampling of EDM polygons to create random points within the polygons.
#  these are the random presence points being created here, from polygon presence data.
# 2. Removing any points from the background points dataset that overlap or are near
#  the input presence polygon dataset.
library(RSQLite)
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

setwd(loc_model)

# set up folder system for inputs
dir.create(paste0(model_species,"/inputs/presence"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(model_species,"/inputs/model_input"), showWarnings = FALSE)

# setwd(paste0(loc_model,"/",model_species,"/inputs/presence"))
# changing to this WD temporarily allows for presence file to be either in presence folder or specified with full path name

# load data, QC ----
# if geopackage is there, open that one
if(grepl("gpkg", nm_presFile)){
  presPolys <- st_zm(st_read(nm_presFile, model_species, quiet = TRUE))  
} else {
  presPolys <- st_zm(st_read(nm_presFile, quiet = TRUE))
}


#check for proper column names. If no error from next code block, then good to go
#presPolys$RA <- presPolys$SFRACalc
shpColNms <- names(presPolys)
desiredCols <- c("UID", "GROUP_ID", "SPECIES_CD", "RA", "OBSDATE")
if("FALSE" %in% c(desiredCols %in% shpColNms)) {
	  stop(paste0("Column(s) are missing or incorrectly named: ", paste(desiredCols[!desiredCols %in% shpColNms], collapse = ", ")))
  } else {
    print("Required columns are present")
  }

if(any(is.na(presPolys[,c("UID", "GROUP_ID", "SPECIES_CD", "RA")]))) {
  stop("The columns 'UID','GROUP_ID','SPECIES_CD', and 'RA' (SFRACalc) cannot have NA values.")
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

# What's the name of the geometry column? Change it to geometry if needed
# presPolys <- backu
# backu <- presPolys

if(!attr(presPolys, "sf_column") == "geometry"){
  oldName <- attr(presPolys, "sf_column")
  #add new col
  names(presPolys)[names(presPolys) == oldName] <- "geometry"
  attr(presPolys, "sf_column") = "geometry"
}

# explode multi-part polys ----
suppressWarnings(shp_expl <- st_cast(presPolys, "POLYGON"))

#add some columns (explode id and area)
shp_expl <- cbind(shp_expl, 
                       EXPL_ID = 1:nrow(shp_expl), 
                       AREAM2 = st_area(shp_expl))
names(shp_expl$geometry) <- NULL # temp fix until sf patches error

#write out the exploded polygon set

nm.PyFile <- paste(loc_model, model_species,"inputs/presence",paste0(baseName, "_expl.shp"), sep = "/")
st_write(shp_expl, nm.PyFile, driver="ESRI Shapefile", delete_layer = TRUE)

# also write to geopackage to get into the habit. At this point (Mar 2020),
# some custom projections don't write properly (https://github.com/r-spatial/sf/issues/1293), 
# so transform to WGS84 first
shp_expl_wgs84 <- st_transform(shp_expl, 4326)
nm.pylyr <- paste0(baseName, "_expl")
nm.gpkg <- file.path(loc_model, model_species,"inputs","presence",paste0(baseName, ".gpkg"))
st_write(shp_expl_wgs84, dsn = nm.gpkg, layer = nm.pylyr, delete_layer = TRUE)

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


#EObyRA <- unique(shp_expl[,c("expl_id", "group_id","ra")])
shp_expl$minSamps[shp_expl$ra == "very high"] <- 5
shp_expl$minSamps[shp_expl$ra == "high"] <- 4
shp_expl$minSamps[shp_expl$ra == "medium"] <- 3
shp_expl$minSamps[shp_expl$ra == "low"] <- 2
shp_expl$minSamps[shp_expl$ra == "very low"] <- 1

shp_expl$finalSampNum <- ifelse(shp_expl$PolySampNum < shp_expl$minSamps, 
                                shp_expl$minSamps, 
                                shp_expl$PolySampNum)

ranPts <- st_sample(shp_expl, size = shp_expl$finalSampNum * 2)
ranPts.sf <- st_sf(ranPts)
names(ranPts.sf) <- "geometry"
st_geometry(ranPts.sf) <- "geometry"

ranPts.joined <- st_join(ranPts.sf, shp_expl)

# check for polys that didn't get any points
polysWithNoPoints <- shp_expl[!shp_expl$expl_id %in% ranPts.joined$expl_id,]
if(nrow(polysWithNoPoints) > 0){
  stop("One or more polygons didn't get any points placed in them.")
}

#  remove extras using straight table work

# this randomly assigns digits to each point by group (stratum) then next row only takes 
# members in group that are less than target number of points
rndid <- with(ranPts.joined, ave(expl_id, stratum, FUN=function(x) {sample.int(length(x))}))
ranPts.joined2 <- ranPts.joined[rndid <= ranPts.joined$finalSampNum,]

ranPts.joined <- ranPts.joined2
rm(rndid, ranPts.joined2)

#check for cases where sample smaller than requested
# how many points actually generated?
ptCount <- table(ranPts.joined$expl_id)
targCount <- shp_expl[order(shp_expl$expl_id),"finalSampNum"]
overUnderSampled <- ptCount - targCount$finalSampNum

#positive vals are oversamples, negative vals are undersamples
print(table(overUnderSampled))
# If you get large negative values then there are many undersamples and 
# exploration might be worthwhile

names(ranPts.joined) <- tolower(names(ranPts.joined))

colsToKeep <- c("stratum", tolower(desiredCols))
ranPts.joined <- ranPts.joined[,colsToKeep]

# name of random points output shapefile
nm.RanPtFile <- paste(loc_model, model_species,"inputs/presence",paste(baseName, "_RanPts.shp", sep = ""), sep = "/")
# write it out
st_write(ranPts.joined, nm.RanPtFile, driver="ESRI Shapefile", delete_layer = TRUE)

# also write to geopackage
# transform to WGS84 first (see above)
ranPts_joined_wgs84 <- st_transform(ranPts.joined, 4326)
nm.pylyr <- paste0(baseName, "_RanPts")
st_write(ranPts_joined_wgs84, dsn = nm.gpkg, layer = nm.pylyr, delete_layer = TRUE)


###
### remove Coincident Background points ----
###

# get range info from the DB (as a list of HUCs)
db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLquery <- paste0("SELECT huc10_id from lkpRange
                   inner join lkpSpecies on lkpRange.EGT_ID = lkpSpecies.EGT_ID
                   where lkpSpecies.sp_code = '", model_species, "';")
hucList <- dbGetQuery(db, statement = SQLquery)$huc10_id
dbDisconnect(db)
rm(db)

op <- options()
options(useFancyQuotes = FALSE) #need straight quotes for query
# get the background data from the DB
db <- dbConnect(SQLite(), nm_bkgPts[1])
#qry <- paste0("SELECT * from ", nm_bkgPts[2], " where substr(huc12,1,10) IN (", paste(sQuote(hucList), collapse = ", ", sep = "")," );")
qry <- paste0("SELECT * from ", nm_bkgPts[2], " where huc10 IN (", paste(sQuote(hucList), collapse = ", ", sep = "")," );")
bkgd <- dbGetQuery(db, qry)
tcrs <- dbGetQuery(db, paste0("SELECT proj4string p from lkpCRS where table_name = '", nm_bkgPts[2], "';"))$p
samps <- st_sf(bkgd, geometry = st_as_sfc(bkgd$wkt, crs = tcrs))
options(op)
rm(op)

# find coincident points ----
polybuff <- st_transform(shp_expl, st_crs(samps))
polybuff <- st_buffer(polybuff, dist = 30)

coincidentPts <- unlist(st_contains(polybuff, samps, sparse = TRUE))

# remove them (if any)
if (length(coincidentPts) > 0) {
    backgSubset <- samps[-coincidentPts,] 
  } else {
    backgSubset <- samps
  } 


###
### clear background pts from unsampled areas (for AZ: native lands) ----
###
if(!is.null(nm_bkgExclAreas)){
  
  # load exclusion areas
  exclA <- st_read(nm_bkgExclAreas[[1]], nm_bkgExclAreas[[2]])
  exclA <- st_transform(exclA, tcrs)
  
  
  exclPts <- unlist(st_contains(exclA, backgSubset, sparse = TRUE))
  # remove them (if any)
  if (length(exclPts) > 0){
    backgSubset <- backgSubset[-exclPts,]
  } else {
    backgSubset <- backgSubset
  }
  # clean up
  rm(exclA, exclPts)
}


###
### give background points same road bias as presence points if requested ----
###
if(!is.null(nm_biasDistRas)){
  library(raster)
  # get roads distance layer
  distRas <- raster(nm_biasDistRas)
  # get distribution of bias from pres points
  presPtsDistVals <- raster::extract(distRas, ranPts.joined, method="simple")
  #plot(density(presPtsDistVals))
  #plot(ecdf(presPtsDistVals))
  
  # get distribution of background pts
  bkgPtsDist <- raster::extract(distRas, backgSubset, method="simple")
  #plot(density(bkgPtsDist))
  #plot(ecdf(bkgPtsDist))
  
  # estimate shape of density (estimate the PDF)
  presDensFun <- approxfun(density(presPtsDistVals))

  ## use rejection sampling
  ## https://theoreticalecology.wordpress.com/2015/04/22/a-simple-explanation-of-rejection-sampling-in-r/
  ## https://web.mit.edu/urban_or_book/www/book/chapter7/7.1.3.html
  
  # rejection sampling:
  # create a target density based on approxfun of the density
  targetDensity <- presDensFun(bkgPtsDist)
  # presDensFun creates NA when out of bounds, convert to zero
  targetDensity[is.na(targetDensity)] <- 0
  # each point is compared to a random draw, if random draw is within target curve (below the value),
  # that point is accepted
  accepted1 <- ifelse(runif(length(targetDensity),0,1) < targetDensity/max(targetDensity), TRUE, FALSE)
  #backgSubset2 <- backgSubset[accepted,]
  #try repeating to gain a few more -- this will mess up the density somewhat
  accepted2 <- ifelse(runif(length(targetDensity),0,1) < targetDensity/max(targetDensity), TRUE, FALSE)
  accepted3 <- ifelse(runif(length(targetDensity),0,1) < targetDensity/max(targetDensity), TRUE, FALSE)
  accepted4 <- ifelse(runif(length(targetDensity),0,1) < targetDensity/max(targetDensity), TRUE, FALSE)
  accepted5 <- ifelse(runif(length(targetDensity),0,1) < targetDensity/max(targetDensity), TRUE, FALSE)
  accepted6 <- ifelse(runif(length(targetDensity),0,1) < targetDensity/max(targetDensity), TRUE, FALSE)

  accepted <- accepted1 | accepted2 | accepted3 | accepted4 | accepted5 | accepted6
  #backgSubset2 <- backgSubset[accepted,]
  backgSubset <- backgSubset[accepted,]

  # full circle test
  # circleTest <- extract(distRas, backgSubset2, method="simple")
  # plot(ecdf(circleTest))
    
  # Metropolis-hastings sampler increase acceptance
  # http://rstudio-pubs-static.s3.amazonaws.com/221320_ae6274302f884f95ac2820b2c6bfc6ef.html

#this seems close but does not create a correct distribution   
  # MHSampling <- function(target_density, R) {
  #   ## Initialize a vector (all 0's)
  #   v <- numeric(length = R)
  #   ## Acceptance indicator (all FALSE)
  #   a <- logical(length = R)
  #   v[1] <- bkgPtsDist[1]
  #   a[1] <- TRUE
  # 
  #   for (r in seq_len(R)[-1]) {
  #     ## Obtain a proposed value (here the proposal distribution is symmetric)
  #     proposal <- bkgPtsDist[r]
  #     ## Evaluate both densities at the proposed value
  #     #target_density_at_proposal <- target_density(proposal)
  #     target_density_at_proposal <- ifelse(is.na(target_density(proposal)),
  #                                          0,
  #                                          target_density(proposal))
  #     target_density_at_previous <- ifelse(is.na(target_density(bkgPtsDist[r-1])),
  #                                         0,
  #                                         target_density(bkgPtsDist[r-1]))
  #     
  #     acceptance_ratio <- ifelse(target_density_at_previous == 0, 
  #                                0, 
  #                                 min(target_density_at_proposal / target_density_at_previous, 1))
  #     if (runif(n = 1) <= acceptance_ratio) {
  #       ## If U(0,1) is less than or equal to acceptance ratio
  #       ## Make a move
  #       v[r] <- proposal
  #       a[r] <- TRUE
  #     } else {
  #       ## Otherwise, don't move
  #       v[r] <- v[r-1]
  #     }
  #   }
  #   
  #   data.frame(v = v,
  #              a = a)
  # }
  # 
  # out1 <- MHSampling(target_density = presDensFun,
  #                                         R = length(bkgPtsDist))
  # out2 <- out1[out1$a == TRUE,]
  # plot(ecdf(out2$v))


  # clean up
  rm(distRas, presPtsDisVals, bkgPtsDist, presDensFun, targetDensity, accepted)
  
}

### reduce the number of bkg points if huge ----

# use the greater of 20 * pres points or 50,000
bkgTarg <- max(nrow(backgSubset) * 20, 50000)
if(nrow(backgSubset) > bkgTarg){
  backgSubset <- backgSubset[sample(nrow(backgSubset), bkgTarg),]
}

print(paste0("number of background pts: ", nrow(backgSubset)))

#write it up and do join in sqlite (faster than downloading entire att set) ----
st_geometry(backgSubset) <- NULL
tmpTableName <- paste0(nm_bkgPts[2], "_", baseName)
dbWriteTable(db, tmpTableName, backgSubset, overwrite = TRUE)

# do the join, get all the data back down
# qry <- paste0("SELECT ", tmpTableName, ".huc12, ", tmpTableName, ".wkt, ", nm_bkgPts[2], "_att.*", 
#            " from ", tmpTableName, " INNER JOIN ", nm_bkgPts[2], "_att on ",
#               tmpTableName,".fid = ", nm_bkgPts[2], "_att.fid;")
qry <- paste0("SELECT ", tmpTableName, ".huc10, ", tmpTableName, ".wkt, ", nm_bkgPts[2], "_att.*", 
              " from ", tmpTableName, " INNER JOIN ", nm_bkgPts[2], "_att on ",
              tmpTableName,".fid = ", nm_bkgPts[2], "_att.fid;")
bgSubsAtt <- dbGetQuery(db, qry)
# delete the table on the db
dbRemoveTable(db, tmpTableName)
dbDisconnect(db)

#remove extra fid column(s) (dots in colname corrupts gpkg layer for esri)
#bgSubsAtt <- bgSubsAtt[,-grep("fid.+",names(bgSubsAtt))] #package update may have removed dot
fidlocs <- grep("fid.*",names(bgSubsAtt))
if(length(fidlocs) > 1){
  bgSubsAtt <- bgSubsAtt[,-fidlocs[2:length(fidlocs)]]  
}


bgSubsAtt <- bgSubsAtt[complete.cases(bgSubsAtt),]
nrow(bgSubsAtt)

dbName <- paste0(loc_model, "/", model_species, "/inputs/model_input/", baseName, "_att.sqlite")
db <- dbConnect(SQLite(), dbName)
dbWriteTable(db, paste0(nm_bkgPts[2], "_clean"), bgSubsAtt, overwrite = TRUE)

dbDisconnect(db)

## TODO. Note, writing bkg to presence folder (geopackage) because it's cleaner. 
## perhaps cleanup and remove these folders inside 'inputs'

# also write to geopackage
# transform to WGS84 first (see above)
bgSubsAtt <- st_sf(bgSubsAtt, geometry = st_as_sfc(bgSubsAtt$wkt, crs = tcrs))
bgSubsAtt_wgs84 <- st_transform(bgSubsAtt, 4326)
nm.pylyr <- paste0(baseName, "_bkgPts_clean")
st_write(bgSubsAtt_wgs84, dsn = nm.gpkg, layer = nm.pylyr, delete_layer = TRUE)


rm(db, do, dt, qry, tmpTableName)
