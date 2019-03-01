# File: preproc_attributeBackgroundPts.r
# Purpose: attribute environmental data to random background points

## start with a fresh workspace with no objects loaded
rm(list=ls())
library(raster)
library(sf)
library(here)
library(RSQLite)
library(snowfall)

# path where .tif env. var rasters are stored
pathToRas <- here("_data","env_vars","raster","ras")
# path to output tables (a database is generated here if it doesn't exist)
pathToTab <- here("_data","env_vars","tabular")
# background points table name (this should be already created with preproc_makeBackgroundPoints.R)
table <- "background_pts"
# lkpEnvVars database
dbLookup <- dbConnect(SQLite(), here("_data","databases","SDM_lookupAndTracking.sqlite"))

## create a stack from all envvars ----
setwd(pathToRas)

## create a stack.
raslist <- list.files(pattern = ".tif$", recursive = TRUE)

# temporal groups -> take only max year by group
tv <- list.dirs(recursive = FALSE, full.names = FALSE)
if (length(tv) > 1) {
  tv_grp <- as.character(do.call(rbind.data.frame, strsplit(tv,"_",fixed = TRUE))[,1])
  for (t in unique(tv_grp)) {
    stv <- tv[grep(t, tv)]
    nouse <- stv[!stv %in% max(stv[grep(t,stv)])]
    raslist <- raslist[-grep(paste(nouse,collapse="|"),raslist)]
  }
}
gridlist <- as.list(paste(pathToRas,raslist,sep = "/"))

# get coded names
lkp <- dbGetQuery(dbLookup, "SELECT gridName, fileName from lkpEnvVars;")


# method 1
# start_time <- Sys.time()
nm <- unlist(lapply(strsplit(raslist, "/", fixed = TRUE), FUN = function(x) {x[length(x)]}))
names(gridlist) <- nm

shortNames <- merge(data.frame(fileName = names(gridlist)), lkp, all.x = TRUE)
gridlist <- gridlist[order(names(gridlist))]
names(gridlist) <- shortNames[order(shortNames$fileName),"gridName"]

nulls <- gridlist[is.na(names(gridlist))]
if(length(nulls) > 0){
  print(nulls)
  stop("Some grids are not in DB.")
}

envStack <- stack(gridlist)
# end_time <- Sys.time()
# end_time - start_time

# method 2 -- slightly slower
# start_time <- Sys.time()
# nm <- substr(raslist,1,nchar(raslist) - 4)
# nm <- unlist(lapply(strsplit(nm, "/", fixed = TRUE), FUN = function(x) {x[length(x)]}))
# envStack <- stack(gridlist)
# 
# for (n in 1:length(names(envStack))) {
#   nm <- names(envStack)[n]
#   try(names(envStack)[n] <- lkp$gridName[paste0(nm, ".tif") == lkp$fileName]) 
#   # errors mean the fileName is missing from the DB.
# }
# end_time <- Sys.time()
# end_time - start_time

s.list <- unstack(envStack)
names(s.list) <- names(envStack)

## Get random points table ----
db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background_CONUS.sqlite"))

db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background_amazviri.sqlite"))


## read in 1 million at a time
# sql_countRows <- paste0("SELECT COUNT(*) AS c from ", table, ";")
# countRows <- dbGetQuery(db, sql_countRows)
# loops <- ceiling(countRows/1000000)

tcrs <- dbGetQuery(db, paste0("SELECT proj4string p from lkpCRS where table_name = '", table, "';"))$p

# get all the points
bkgd <- dbReadTable(db, table)
bkgd <- bkgd[complete.cases(bkgd),]

# attribute and write the table with two points
bkgd_subs <- bkgd[1:2,]
samps <- st_sf(bkgd_subs, geometry = st_as_sfc(bkgd_subs$wkt, crs = tcrs))
att <- extract(envStack, samps, method="simple")
sampsAtt <- as.data.frame(cbind(fid = as.integer(samps$fid), att))
tp <- as.vector("INTEGER")
names(tp) <- "fid"
dbWriteTable(db, paste0(table, "_att"), sampsAtt, overwrite = TRUE, field.types = tp)

# now loop through the rest
bkgd <- bkgd[-c(1:2),]
brks <- cut(bkgd$fid, breaks = 30)
brkgrps <- unique(brks)


for(i in 1:length(brkgrps)){
  bg <- bkgd[brks == brkgrps[i],]
  samps <- st_sf(bg, geometry = st_as_sfc(bg$wkt, crs = tcrs))

  # create an R cluster using all the machine cores minus two
  sfInit(parallel=TRUE, cpus=parallel:::detectCores()-2)
  # Load the required packages inside the cluster
  sfLibrary(raster)
  sfLibrary(sf)
  # Run parallelized 'extract' function and stop cluster
  e.df <- sfSapply(s.list, extract, y=samps, method = "simple")
  sfStop()
  DF <- data.frame(e.df)
  sampsAtt <- as.data.frame(cbind(fid = as.integer(samps$fid), DF))
  # write to DB
  dbWriteTable(db, paste0(table, "_att"), sampsAtt, overwrite = FALSE, append = TRUE)
  print(paste0("done with ", i, " of 30 loops"))
}


# without looping
# bkgd <- dbReadTable(db, table)
# 
# #remove any huc12 nulls
# bkgd <- bkgd[complete.cases(bkgd),]
# 
# tcrs <- dbGetQuery(db, paste0("SELECT proj4string p from lkpCRS where table_name = '", table, "';"))$p
samps <- st_sf(bkgd, geometry = st_as_sfc(bkgd$wkt, crs = tcrs))


#### multi-core !
# https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
# Extract values to a data frame - multicore approach
# First, convert raster stack to list of single raster layers
# s.list <- unstack(envStack)
# names(s.list) <- names(envStack)
# # Now, create a R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)
# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sf)
# Run parallelized 'extract' function and stop cluster
e.df <- sfSapply(s.list, extract, y=samps, method = "simple")
sfStop()

DF <- data.frame(e.df)
sampsAtt <- as.data.frame(cbind(fid = as.integer(samps$fid), DF))

# write to DB
tp <- as.vector("INTEGER")
names(tp) <- "fid"
dbWriteTable(db, paste0(table, "_att"), sampsAtt, overwrite = T, field.types = tp)
# # not writing shapefile, since base shapefile already exists

dbDisconnect(db)
rm(db)

## clean up ----
# remove all objects before using another script
rm(list=ls())
