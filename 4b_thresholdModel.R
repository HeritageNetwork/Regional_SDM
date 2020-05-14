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

