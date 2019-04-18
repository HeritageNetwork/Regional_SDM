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
pathToRas <- here("_data","env_vars","raster")
# path to output tables (a database is generated here if it doesn't exist)
pathToTab <- here("_data","env_vars","tabular")
# background points table name (this should be already created with preproc_makeBackgroundPoints.R)
pts_table <- "background_pts"
# lkpEnvVars database
dbLookup <- dbConnect(SQLite(), here("_data","databases","SDM_lookupAndTracking.sqlite"))

## create a stack from all envvars ----
setwd(pathToRas)

## create a stack.
raslist <- list.files(pattern = ".tif$", recursive = TRUE)

x <- grepl("ras/",raslist)
raslist <- raslist[!grepl("ras/",raslist)]

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

# get short names

lkp <- dbGetQuery(dbLookup, "SELECT gridName, fileName from lkpEnvVars;")

#align names
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

#stack only half of it (54 layers)
#gl <- gridlist[1:35]

envStack <- stack(gridlist)

# prep for parallel
s.list <- unstack(envStack)
names(s.list) <- names(envStack)

## Get random points table ----
db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background_CA.sqlite"))

tcrs <- dbGetQuery(db, paste0("SELECT proj4string p from lkpCRS where table_name = '", pts_table, "';"))$p

# get all the points
bkgd <- dbReadTable(db, pts_table)
bkgd <- bkgd[complete.cases(bkgd),]

## export to shp
#samps <- st_sf(bkgd, geometry = st_as_sfc(bkgd$wkt, crs = tcrs))
#st_write(samps, dsn = "N:/_TerrestrialModels/_data/env_vars/background/california_bkgd.shp")
##

## somewhere over 1 million points is problematic 
## there are three different ways of attributing below. 
## take your choice on what you try. 

# method #1, looping through subsets, in parallel ----
# attribute and write the table with two points
bkgd_subs <- bkgd[1:2,]
samps <- st_sf(bkgd_subs, geometry = st_as_sfc(bkgd_subs$wkt, crs = tcrs))
att <- extract(envStack, samps, method="simple")
sampsAtt <- as.data.frame(cbind(fid = as.integer(samps$fid), att))
tp <- as.vector("INTEGER")
names(tp) <- "fid"
dbWriteTable(db, paste0(pts_table, "_att"), sampsAtt, overwrite = TRUE, field.types = tp)
#dbWriteTable(db, paste0(pts_table, "_att"), sampsAtt, overwrite = FALSE, append = TRUE)

# now loop through the rest
bkgd <- bkgd[-c(1:2),]
brks <- cut(bkgd$fid, breaks = 100)
brkgrps <- unique(brks)

for(i in 8:length(brkgrps)){
  bg <- bkgd[brks == brkgrps[i],]
  samps <- st_sf(bg, geometry = st_as_sfc(bg$wkt, crs = tcrs))

  # create an R cluster using all the machine cores minus two
  sfInit(parallel=TRUE, cpus=parallel:::detectCores()-4)
  # Load the required packages inside the cluster
  sfLibrary(raster)
  sfLibrary(sf)
  # Run parallelized 'extract' function and stop cluster
  e.df <- sfSapply(s.list, extract, y=samps, method = "simple")
  sfStop()
  DF <- data.frame(e.df)
  sampsAtt <- as.data.frame(cbind(fid = as.integer(samps$fid), DF))
  # write to DB
  dbWriteTable(db, paste0(pts_table, "_att"), sampsAtt, overwrite = FALSE, append = TRUE)
  print(paste0("done with ", i, " of 100 loops"))
}


# method #2, looping through subsetes, no parallel ----
# this uses all of setup in #1, except for the loop.
#  RAM isn't a problem here, it just takes a very long time
for(i in 1:length(brkgrps)){
  bg <- bkgd[brks == brkgrps[i],]
  samps <- st_sf(bg, geometry = st_as_sfc(bg$wkt, crs = tcrs))
  att <- extract(envStack, samps, method="simple")
  sampsAtt <- as.data.frame(cbind(fid = as.integer(samps$fid), att))
  dbWriteTable(db, paste0(pts_table, "_att"), sampsAtt, overwrite = FALSE, append = TRUE)
  print(paste0("done with ", i, " of 100 loops"))
}

# method #3. no looping, in parallel ----

bkgd <- dbReadTable(db, pts_table)
#remove any huc12 nulls
bkgd <- bkgd[complete.cases(bkgd),]

tcrs <- dbGetQuery(db, paste0("SELECT proj4string p from lkpCRS where table_name = '", pts_table, "';"))$p
samps <- st_sf(bkgd, geometry = st_as_sfc(bkgd$wkt, crs = tcrs))

# multi-core method from here
# https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
# Extract values to a data frame - multicore approach
# First, convert raster stack to list of single raster layers
s.list <- unstack(envStack)
names(s.list) <- names(envStack)
# create a R cluster using all the machine cores minus one
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
dbWriteTable(db, paste0(pts_table, "_att"), sampsAtt, overwrite = T, field.types = tp)
# not writing shapefile, since base shapefile already exists

dbDisconnect(db)
rm(db)

## clean up ----
# remove all objects before using another script
rm(list=ls())
