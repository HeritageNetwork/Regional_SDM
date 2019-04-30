# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

library(raster)
library(sf)
library(RSQLite)
library(snowfall)

# load data, QC ----
setwd(loc_envVars)

# get the rasters
raslist <- list.files(pattern = ".tif$", recursive = TRUE)

# get short names from the DB
# first shorten names in subfolders
raslist.short <- unlist(
  lapply(strsplit(raslist, "/"), function(x) {x[length(x)]})
)

db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLQuery <- "select gridName, fileName from lkpEnvVars;"
evs <- dbGetQuery(db, SQLQuery)
shrtNms <- merge(data.frame(fileName = raslist.short, fullname = raslist, stringsAsFactors = FALSE), evs)
dbDisconnect(db)

gridlist <- as.list(paste(loc_envVars,shrtNms$fullname,sep = "/"))
names(gridlist) <- shrtNms$gridName

nulls <- gridlist[is.na(names(gridlist))]
if(length(nulls) > 0){
  print(nulls)
  stop("Some grids are not in DB.")
}

# check to make sure there are no names greater than 10 chars
nmLen <- unlist(lapply(names(gridlist),nchar))
max(nmLen) # if this result is greater than 10, you've got a renegade

# Set working directory to the random points location
setwd(paste0(loc_model, "/", model_species, "/inputs"))

shpf <- st_read(paste0("presence/", baseName, "_RanPts.shp"),quiet = T)

# subset input env. vars by model type (terrestrial, shore, etc)
db <- dbConnect(SQLite(),dbname=nm_db_file)
# get MODTYPE
SQLQuery <- paste0("SELECT MODTYPE m FROM lkpSpecies WHERE sp_code = '", model_species, "';")
modType <- dbGetQuery(db, SQLQuery)$m

# if modtype is both (B), flip it to A or T
# what git branch are we on?
branches <- system("git branch", intern = TRUE)
activeBranch <- branches[grep("\\*", branches)]
activeBranch <- sub("\\*", "", activeBranch)
activeBranch <- gsub(" ", "", activeBranch)

if(modType == "B"){
  if(activeBranch == "terrestrial") modType <- "T"
  if(activeBranch == "aquatic") modType <- "A"
}

# gridlistSub is a running list of variables to use. Uses fileName from lkpEnvVars
SQLQuery <- paste0("SELECT gridName, fileName FROM lkpEnvVars WHERE use_",modType," = 1;")
gridlistSub <- dbGetQuery(db, SQLQuery)
gridlistSub$fileName <- gsub(".tif$","",gridlistSub$fileName)
gridlistSub$gridName <- tolower(gridlistSub$gridName)
dbDisconnect(db)

## account for add/remove vars
if (!is.null(add_vars)) {
  add_vars1 <- add_vars
  add_vars <- tolower(add_vars)
  
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  SQLQuery <- paste0("SELECT gridName, fileName FROM lkpEnvVars;")
  gridlistAll <- dbGetQuery(db, SQLQuery)
  gridlistAll$fileName <- gsub(".tif$","",gridlistAll$fileName)
  gridlistAll$gridName <- tolower(gridlistAll$gridName)
  dbDisconnect(db)
  
  if (!all(add_vars %in% gridlistAll$gridName)) {
    stop("Some environmental variables listed in `add_vars` were not found in `nm_EnvVars` dataset: ",
         paste(add_vars1[!add_vars %in% gridlistAll$gridName], collapse = ", "), ".")
  }
  # add the variables
  add_vars_df <- gridlistAll[gridlistAll$gridName %in% add_vars,]
  gridlistSub <- rbind(gridlistSub, add_vars_df)
}
if (!is.null(remove_vars)) {
  remove_vars1 <- remove_vars
  remove_vars <- tolower(remove_vars)
  if (!all(remove_vars %in% gridlistSub$gridName)) {
    message("Some environmental variables listed in `remove_vars` were not found in the `nm_EnvVars` dataset: ",
            paste(remove_vars1[!remove_vars %in% gridlistSub$gridName], collapse = ", "), ".")
  } 
  # remove the variables
  gridlistSub <- gridlistSub[!tolower(gridlistSub$gridName) %in% tolower(remove_vars),]
}
# remove duplicates, then subset
gridlistSub <- gridlistSub[!duplicated(gridlistSub),]
fullL <- gridlist[names(gridlist) %in% tolower(gridlistSub$gridName)]

# Could use this script here crop/mask rasters
#source(paste0(loc_scripts, "/helper/crop_mask_rast.R"), local = TRUE)
#envStack <- stack(newL)

# make grid stack with subset
envStack <- stack(fullL)
rm(fullL, gridlistSub, modType, branches, activeBranch)

# extract raster data to points ----

# Extract values to a data frame - multicore approach using snowfall
# First, convert raster stack to list of single raster layers
s.list <- unstack(envStack)
names(s.list) <- names(envStack)
# Now, create a R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-3)
# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sf)
# Run parallelized 'extract' function and stop cluster
e.df <- sfSapply(s.list, extract, y=shpf, method = "simple")
sfStop()

points_attributed <- st_sf(cbind(data.frame(shpf), data.frame(e.df)))

# method without using snowfall
#points_attributed <- extract(envStack, shpf, method="simple", sp=TRUE)

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

# write it out to the att db
dbName <- paste(baseName, "_att.sqlite", sep="")
db <- dbConnect(SQLite(), paste0("model_input/",dbName))
att_dat <- points_attributed
st_geometry(att_dat) <- NULL
#att_dat <- points_attributed@data
dbWriteTable(db, paste0(baseName, "_att"), att_dat, overwrite = TRUE)
dbDisconnect(db)
rm(db, SQLQuery)

