# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(RSQLite)
library(maptools)

# load data, QC ----
setwd(loc_envVars)

# create a stack
# if using TIFFs, use this line
raslist <- list.files(pattern = ".tif$", recursive = TRUE)
# if using native R rasters, use this line
#raslist <- list.files(pattern = ".grd$")

# find temporal vars (placed in subfolders)
raslist.t <- raslist[grep("/",raslist,fixed = TRUE)]
# exclude temporal vars, for the moment
# raslist <- raslist[-grep("/",raslist,fixed = TRUE)]

gridlist <- as.list(paste(loc_envVars,raslist,sep = "/"))
nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm

# check to make sure there are no names greater than 10 chars
nmLen <- unlist(lapply(nm, nchar))
max(nmLen) # if this result is greater than 10, you've got a renegade

# Set working directory to the random points location
setwd(paste0(loc_model, "/", model_species, "/inputs"))

ranPtsFilesNoExt <- paste0(baseName, "_RanPts")
shpf <- readOGR("model_input", layer = ranPtsFilesNoExt)

#get projection info for later
projInfo <- shpf@proj4string

# subset input env. vars by model type (terrestrial, shore, etc)
db <- dbConnect(SQLite(),dbname=nm_db_file)
# get MODTYPE
SQLQuery <- paste0("SELECT MODTYPE m FROM lkpSpecies WHERE sp_code = '", model_species, "';")
modType <- dbGetQuery(db, SQLQuery)$m

SQLQuery <- paste0("SELECT gridName g FROM lkpEnvVars WHERE use_",modType," = 1;")
gridlistSub <- dbGetQuery(db, SQLQuery)$g

# get just names of grids (removes folder for temporal vars)
justTheNames <- unlist(lapply(strsplit(names(gridlist), "/", fixed = TRUE), FUN = function(x) {x[length(x)]}))

## account for add/remove vars
if (!is.null(add_vars)) {
  add_vars1 <- add_vars
  add_vars <- tolower(add_vars)

  db <- dbConnect(SQLite(),dbname=nm_db_file)
  SQLQuery <- paste0("SELECT gridName g FROM lkpEnvVars;")
  gridlistAll <- tolower(dbGetQuery(db, SQLQuery)$g)
  dbDisconnect(db)
  
  if (!all(add_vars %in% gridlistAll)) {
    stop("Some environmental variables listed in `add_vars` were not found in `nm_EnvVars` dataset: ",
         paste(add_vars1[!add_vars %in% gridlistSub], collapse = ", "), ".")
  }
  gridlistSub <- c(gridlistSub, add_vars)
}
if (!is.null(remove_vars)) {
  remove_vars1 <- remove_vars
  remove_vars <- tolower(remove_vars)
  if (!all(remove_vars %in% gridlistSub)) {
    message("Some environmental variables listed in `remove_vars` were not found in the `nm_EnvVars` dataset: ",
            paste(remove_vars1[!remove_vars %in% gridlistSub], collapse = ", "), ".")
  } 
  gridlistSub <- gridlistSub[!tolower(gridlistSub) %in% tolower(remove_vars)]
}

# make grid stack with subset
envStack <- stack(gridlist[tolower(justTheNames) %in% tolower(gridlistSub)])
rm(justTheNames, gridlistSub, modType)

# extract raster data to points ----
##  Bilinear interpolation is a *huge* memory hog. 
##  Do it all as 'simple' 

points_attributed <- extract(envStack, shpf, method="simple", sp=TRUE)

# temporal variables data handling
pa <- points_attributed
tv <- names(pa)[grep(".",names(pa), fixed = TRUE)]
if (length(tv) > 0) {
  tvDataYear <- do.call(rbind.data.frame, strsplit(tv, "_|\\."))
  names(tvDataYear) <- c("dataset", "date", "envvar")
  tvDataYear$date <- as.numeric(as.character(tvDataYear$date))
  
  # loop over temporal variables
  for (i in unique(tvDataYear$envvar)) {
    tvDataYear.s <- subset(tvDataYear, tvDataYear$envvar == i)
    yrs <- sort(unique(tvDataYear.s$date))
    
    # add 0.1 to occurrence date/year, avoiding cases where date is exactly between two years
    closestYear <- unlist(lapply(as.numeric(pa$date) + 0.1, FUN = function(x) {
      y <- abs(x - yrs)
      yrs[which.min(y)]}))
    
    # DECIDE IF THERE SHOULD BE A CUTOFF FOR WHEN OBSERVATION YEAR IS NOT CLOSE TO ANY OF THE DATES #
    
    vals <- unlist(lapply(1:length(pa), FUN = function(x) {
      eval(parse(text = paste0("pa$", tvDataYear.s$dataset[1],"_",closestYear[x],".",i, "[", x , "]")
                 ))
    }))
    
    # add to pa
    eval(parse(text = paste0("pa$", i, " <- vals")))
  }
  
  points_attributed <- pa[-grep(".", names(pa), fixed = TRUE)]
}
suppressWarnings(rm(tv,tvDataYear,tvDataYear.s, yrs, closestYear, vals, pa))

# write it out ----
points_attributed@proj4string <- projInfo
filename <- paste(baseName, "_att", sep="")
writeOGR(points_attributed, "model_input", layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)
