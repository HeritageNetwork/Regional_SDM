
# File: 2_attributePoints.r
# Purpose: attribute environmental data to presence points

library(raster)
library(sf)
library(RSQLite)
library(snowfall)
library(stars)

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

# 
# x <- fullL[1:50]
# y <- stack(x)
# 
# z <- fullL[grepl("snap",fullL)]
# a <- stack(z)
# 
# j <- extent(raster(z[[1]]))
# for(i in 1:length(z)){
#   
#   z[[i]]$ext <- extent(raster(z[[i]]))
# }

# make grid stack with subset
#with stars, may not need envStack 
envStack <- stack(fullL)

#PJM: grabbingpaths and  names of rasters with their paths from fullL, used with stars_read in loop
#to create stars_proxy objects further down
#names could be updated to more appropriate convention- such as grid_paths, grid_names
raster_paths<-unlist(fullL, use.names=F)
raster_names<-names(fullL)

#Notes on trouble shooting stars from PJM
#temp_stars<-read_stars(raster_paths[1:38]) #works
#temp_stars<-read_stars(raster_paths[1:40]) #beersx1000 & crvslpx100 are exmples that won't read in with others
#temp_stars<-read_stars(raster_paths[39:40])
#stars_1<-read_stars(raster_paths[1]) #clim_bio1
#stars_39<-read_stars(raster_paths[39]) #beersx1000
#combine_1_39<-c(stars_1, stars_39, along=1)
#combine_1_39  #results printed to screen sugest possibly different CRS for some rasters?
 
rm(fullL, gridlistSub, modType, branches, activeBranch)

# extract raster data to points ----

# Extract values to a data frame - multicore approach using snowfall
# First, convert raster stack to list of single raster layers
#PJM: slist not utilized in current script, nor is envStack
s.list <- unstack(envStack)
names(s.list) <- names(envStack)


# PJM: commented this bit about break out to try looping through rasters with stars package
# # with contributions from PJM
# # if more than 10k points, split into groups approx 5k each
# if(nrow(shpf) > 10000){
#   shpRows <- 1:nrow(shpf)
#   numBrks <- ceiling(nrow(shpf)/5000)
#   brks <- cut(shpRows, breaks = numBrks)
#   brkgrps<- unique(brks)  
# } else {
#   brks <- rep(1, nrow(shpf))
#   brkgrps <- 1
# }

#debug/checking
#print(paste0("num gps for attribute: ", length(brkgrps)))

#PJM: tempory fix for large ranges using stars below
# loops through rasters individually because of problems found combining some rasters into a single stars object
#  Would be better to combine rasters into a cube or single stars proxy object and 
# handle together. Commented out parallel portion- seems incredibly speedy without
# once rasters are combined into a cube with stars, should approach in parallel


# start cluster
#sfInit(parallel=TRUE, cpus=parallel:::detectCores()-31)
#sfLibrary(raster)
#sfLibrary(sf)
#sfLibrary(stars)
# extract by subsets to keep RAM from maxing out #No longer relevant 
e.df_list<-list()
for(i in 1:length(raster_paths)){
  stars_rast<-read_stars(raster_paths[i])  #reads in rasters individually as stars object or stars_proxy depending on size.  
  # Run parallelized 'extract' function
  #e.df_list[[i]] <- sfSapply(s.list, extract, y=shpf_sub, method = "simple")
  #e.df_list[[i]] <- sfSapply(temp_stars, st_extract, y=shpf) #tried this on a group of stars_objects
 
  e.df_extract <- st_extract(stars_rast,shpf)  #uses st_extract function to extract raster data to points
  e.df_extract<-as.data.frame(e.df_extract)   #this bit was to get rid of the geometry  stuck on the end 
  e.df_extract<-subset(e.df_extract, select=-geometry)  #this bit was to get rid of the geometry object stuch on the end 
  e.df_list[[i]]<-as.data.frame(e.df_extract)   #
  
}
# stop cluster
#sfStop()

e.df<-(do.call(cbind, e.df_list))
names(e.df)
names(e.df)<-names(envStack)
points_attributed <- st_sf(cbind(data.frame(shpf), data.frame(e.df)))

#seemed to be throwing an error due to geometry field at end arising from st_extract, so removing geometry 

points_attributed<-subset(points_attributed, select=-geometry)  
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
