# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
#library(raster) # do we stil need this - CT
library(raster)
library(rgdal)
library(randomForest)
library(data.table)

####
## two lines need your attention. The one directly below (loc_scripts)
## and about line 26 where you choose which Rdata file to use,

# get paths, other settings
#source(paste(loc_scripts,"0_pathsAndSettings.R", sep="/"))
# get the customized version of the predict function
#source(paste(loc_scripts, "RasterPredictMod.R", sep = "/"))

# load data ----
# get the rdata file
setwd(loc_RDataOut)
# fileList <- dir(pattern = ".Rdata$",full.names=FALSE)
# fileList
# # choose one to run, load it #### requires editing ####
# n <- 1
# load(fileList[[n]])
load(paste(modelrun_meta_data$model_run_name,".Rdata", sep=""))


# load the environmental variables -- analogous to the development of the raster stack in the terr models
setwd(loc_envVars)

EnvVars <- read.csv("EnvVars.csv", colClasses=c("huc12"="character"))
names(EnvVars) <- tolower(names(EnvVars))
EnvVars$huc12 <- NULL


# run prediction ----
df.all <- df.full

result <- predict(rf.full, df.all[,indVarCols], type="prob")

#### CHECK THIS - uses numeric index for columns; likely need to change it
result <- result[,-1]
df.all$probability <- result
results_join_table <- df.all[c("comid","probability")]
#### END CHECK

# load the reach shapefile for the study area
setwd(loc_otherSpatial)
StudyAreaReaches <- nm_allflowlines # the name of the study area flowlines
layer <- strsplit(StudyAreaReaches,"\\.")[[1]][[1]]
shapef <- readOGR(loc_otherSpatial, layer = layer)

# join probability to shapefile -- https://stackoverflow.com/questions/5732064/merge-data-vector-to-shapefile-data-slot
# shapef@data <- data.frame(shapef@data, results_join_table[match(shapef@data$comid, results_join_table$comid),])
shapef <- merge(shapef, results_join_table, by = "comid")

# write the shapefile
writeOGR(obj=shapef, dsn= loc_outVector, layer= paste0(modelrun_meta_data$model_run_name, "_results"), driver="ESRI Shapefile")
