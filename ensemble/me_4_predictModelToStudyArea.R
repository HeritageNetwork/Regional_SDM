# File: me_4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster for a maxent model

library(here)
library(raster)

options(java.parameters = "-Xmx5g" )
library(dismo)

# reduce, rearrange the raster stack
envStack_ss <- envStack[[names(me.out.fin@presence)]]

# run prediction ----
setwd(file.path(loc_model, model_species,"outputs","model_predictions"))
fileNm <- paste0(model_run_name,"_",algo,".tif")

cat("... predicting throughout study area \n")

pd <- predict(me.out.fin, envStack_ss, filename = fileNm, silent = "TRUE")

