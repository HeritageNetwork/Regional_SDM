# File: 4_predictModelToStudyArea.r
# Purpose: create the shapefile with model predictions

## start with a fresh workspace with no objects loaded
library(rgdal)
library(randomForest)
library(data.table)

####
## two lines need your attention. The one directly below (loc_scripts)
## and about line 26 where you choose which Rdata file to use

# load data ----
# get the rdata file
setwd(loc_modelOut)
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

# load the environmental variables -- analogous to the development of the raster stack in the terr models
setwd(loc_envVars)
EnvVars <- read.csv("EnvVars.csv", colClasses=c("huc12"="character"))
names(EnvVars) <- tolower(names(EnvVars))

if (!is.null(huc_level)) {
  # subset to huc if requested
  EnvVars$huc12 <- str_pad(EnvVars$huc12, 12, pad=0)
  presHUC <- str_pad(as.character(df.full$huc12[df.full$pres==1]), 12, pad = 0 )
  HUCsubset <- unique(substr(presHUC, 1, huc_level)) # subset to number of huc digits
  EnvVars <- EnvVars[substr(EnvVars$huc12,1, huc_level) %in% HUCsubset,]
}
EnvVars$huc12 <- NULL

# run prediction, using all rows in EnvVars with complete cases----
df.all <- df.full
df.all.pred <- EnvVars[c("comid",names(df.all)[indVarCols])]
df.all.pred <- df.all.pred[complete.cases(df.all.pred),]

result <- as.data.frame(predict(rf.full, df.all.pred[names(df.all)[indVarCols]], type="prob"))

## get probability for presence (column name = "1")
result <- result[,"1"]
results_join_table <- data.frame(comid=df.all.pred$comid, prbblty=result)

# load the reach shapefile for the study area
setwd(loc_otherSpatial)
StudyAreaReaches <- nm_allflowlines # the name of the study area flowlines
layer <- strsplit(StudyAreaReaches,"\\.")[[1]][[1]]
shapef <- readOGR(loc_otherSpatial, layer = layer)

# join probability to shapefile
shapef <- merge(shapef, results_join_table, by = "comid", all.x = F)

# write the shapefile
writeOGR(obj=shapef, dsn= paste0(loc_modelOut, "/model_predictions"), layer= paste0(modelrun_meta_data$model_run_name, "_results"), driver="ESRI Shapefile", overwrite_layer = TRUE)