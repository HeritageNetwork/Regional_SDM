# File: preproc_attributeBackgroundPts.r
# Purpose: attribute environmental data to random background points

## start with a fresh workspace with no objects loaded
rm(list=ls())
library(raster)
library(sf)
library(here)
library(RSQLite)

# path where .tif env. var rasters are stored
pathToRas <- here("_data","env_vars","raster", "ras")
# path to output tables (a database is generated here if it doesn't exist)
pathToTab <- here("_data","env_vars","tabular")
# background points table name (this should be already created with preproc_makeBackgroundPoints.R)
table <- "background_pts"
# lkpEnvVars database
dbLookup <- dbConnect(SQLite(), here("_data","databases","SDM_lookupAndTracking.sqlite"))

## create a stack from all envvars ----
setwd(pathToRas)

## create a stack. Note this is using native R rasters
raslist <- list.files(pattern = ".tif$", recursive = FALSE)

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
nm <- substr(raslist,1,nchar(raslist) - 4)
nm <- unlist(lapply(strsplit(nm, "/", fixed = TRUE), FUN = function(x) {x[length(x)]}))
names(gridlist) <- nm
envStack <- stack(gridlist)

# change stack original (file) names to coded names
lkp <- dbGetQuery(dbLookup, "SELECT gridName, fileName from lkpEnvVars;")
for (n in 1:length(names(envStack))) {
  nm <- names(envStack)[n]
  try(names(envStack)[n] <- lkp$gridName[paste0(nm, ".tif") == lkp$fileName]) 
  # errors mean the fileName is missing from the DB.
}

## Get random points table ----
db <- dbConnect(SQLite(), paste0(pathToTab, "/", "background.sqlite"))
bkgd <- dbReadTable(db, table)
tcrs <- dbGetQuery(db, paste0("SELECT proj4string p from lkpCRS where table_name = '", table, "';"))$p
samps <- st_sf(bkgd, geometry = st_as_sfc(bkgd$wkt, crs = tcrs))

# extract values
x <- extract(envStack, samps, method="simple")
sampsAtt <- as.data.frame(cbind(fid = as.integer(samps$fid), x))

# write to DB
tp <- as.vector("INTEGER")
names(tp) <- "fid"
dbWriteTable(db, paste0(table, "_att"), sampsAtt, overwrite = T, field.types = tp)
# not writing shapefile, since base shapefile already exists

dbDisconnect(db)
rm(db)

## clean up ----
# remove all objects before using another script
rm(list=ls())
