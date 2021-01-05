# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(here)
library(raster)
library(RSQLite)

# rearrange and subset envStack
#varList <- names(marsFit1$trainingData)[!names(marsFit1$trainingData) %in% c(".outcome")]
varList <- rownames(varImp(marsFit1)$importance)
envStack_ss <- envStack[[varList]]


# run prediction ----
setwd(file.path(loc_model, model_species,"outputs","model_predictions"))
fileNm <- paste0(model_run_name,"_",algo,".tif")

pd <- raster::predict(envStack, marsFit1, filename = fileNm, type = "prob", index = 2)

