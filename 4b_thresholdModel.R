# File: 4b_thresholdModel.r
# Purpose: calculate thresholds and store values in db


### find and load model data ----
setwd(loc_model)
setwd(paste0(model_species,"/outputs"))
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

# get thresholds for each algo that was run

for(algo in ensemble_algos){
  message(paste0("getting threshold info for the ", algo, " model."))
  scriptToCall <- paste0(algo, "_4b_thresholdModel.R")
  source(here("ensemble", scriptToCall))
}



# create a combined, mean suitability raster for the metadata
# 
# actual ensemble rasters to be used should probably be of a different 
# variety: thresholded and merged, not this simple mean. 

predStackPths <- vector("list",length(ensemble_algos))
names(predStackPths) <- ensemble_algos

pth <- file.path(loc_model, model_species,"outputs","model_predictions")

for(algo in ensemble_algos){
  # build a raster stack
  fileNm <- paste0(model_run_name,"_",algo,".tif")  
  predStackPths[algo] <- file.path(pth, fileNm)
}

predStack <- stack(predStackPths)

#calculate the mean
fileNm <- paste0(model_run_name,"_meanSuitabilities.tif")
outName <- file.path(pth, fileNm)
mnOut <- calc(predStack, fun=mean, filename = outName)

# document that this is the raster to use 
# in the metadata pdf map
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("UPDATE tblModelResults SET raster_for_metadata_figure = '",
             fileNm, "' WHERE model_run_name = '", 
             model_run_name, "';")
dbExecute(db, sql)
dbDisconnect(db)

