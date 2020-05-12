# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(sf)
library(ROCR)
library(RSQLite)
library(DBI)
removeTmpFiles(48) # clean old (>2days) Raster temporary files

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

# 
# # collate and write to DB ----
# 
# # number of thresholds to write to the db
# numThresh <- length(cutList)
# 
# allThresh <- data.frame("model_run_name" = rep(modelrun_meta_data$model_run_name, numThresh),
#                         "ElemCode" = rep(ElementNames$Code, numThresh),
#                 "dateTime" = rep(as.character(Sys.time()), numThresh),
#                 "cutCode" = unlist(lapply(cutList, function(x) x["code"])),
#                 "cutValue" = unlist(lapply(cutList, function(x) x["value"])),
#                 "capturedEOs" = unlist(lapply(cutList, function(x) x["capturedEOs"])),
#                 "capturedPolys" = unlist(lapply(cutList, function(x) x["capturedPolys"])),
#                 "capturedPts" = unlist(lapply(cutList, function(x) x["capturedPts"])),
#                 stringsAsFactors = FALSE)
# 
# db <- dbConnect(SQLite(),dbname=nm_db_file)
# 
# 
# op <- options("useFancyQuotes")
# options(useFancyQuotes = FALSE)
# 
# dbWriteTable(db, "tblModelResultsCutoffs", allThresh, append = TRUE)
# # clean up
# options(op)
# dbDisconnect(db)
# 
# ## create all thresholds grid
# t2 <- sort(unique(allThresh$cutValue))
# t2 <- t2[!is.na(t2)]
# # get unique thresholds
# t3 <- data.frame(cutCodes = unlist(lapply(t2, FUN = function(x) {paste(allThresh$cutCode[allThresh$cutValue == x], collapse = ";")})),
#                  cutValue = t2,
#                  order = 1:length(t2))
# 
# # load the prediction grid
# ras <- raster(paste0("model_predictions/", model_run_name, ".tif"))
# 
# # reclassify the raster based on the threshold into binary 0/1
# m <- cbind(
#   from = c(-Inf, t3$cutValue),
#   to = c(t3$cutValue, Inf),
#   becomes = c(0, t3$order)
# )
# # reclassify (multi-core try)
# if (all(c("snow","parallel") %in% installed.packages())) {
#   try({
#     cat("Using multi-core processing...\n")
#     beginCluster(type = "SOCK")
#     rasrc <- clusterR(ras, reclassify, args = list(rcl = m))
#   })
#   try(endCluster())
#   if (!exists("rasrc")) {
#     cat("Cluster processing failed. Falling back to single-core processing...\n")
#     rasrc <- reclassify(ras, m)
#   }
# } else {
#   rasrc <- reclassify(ras, m)
# }
# rasrc <- as.factor(rasrc)
# levels(rasrc) <- merge(levels(rasrc), t3, by.x = "ID", by.y = "order", all.x = T)
# 
# outfile <- paste("model_predictions/",model_run_name,"_all_thresholds.tif", sep = "")
# writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE, datatype = "INT2U")
# 
# #clean up
# rm(m, rasrc)
# 
# ## continuous grid that drops cells below lowest calculated thresh ----
# # reclassify the raster based on the threshold into Na below thresh
# m <- cbind(
#   from = c(-Inf),
#   to = min(t3$cutValue),
#   becomes = c(NA)
# )
# 
# # reclassify (multi-core try)
# if (all(c("snow","parallel") %in% installed.packages())) {
#   try({
#     cat("Using multi-core processing...\n")
#     beginCluster(type = "SOCK")
#     rasrc <- clusterR(ras, reclassify, args = list(rcl = m))
#   })
#   try(endCluster())
#   if (!exists("rasrc")) {
#     cat("Cluster processing failed. Falling back to single-core processing...\n")
#     rasrc <- reclassify(ras, m)
#   }
# } else {
#   rasrc <- reclassify(ras, m)
# }
# 
# #plot(rasrc)
# outfile <- paste("model_predictions/",model_run_name,"_min_thresh_continuous.tif", sep = "")
# writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)
