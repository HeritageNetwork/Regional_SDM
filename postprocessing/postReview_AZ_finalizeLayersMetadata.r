## The goal of this script is to scan the tracking DB for models passing review
#  and ready for any final modifications and export to a final output  folder
# 
# Each species is tagged in the DB in two places. The tag is "[usethis]" and it 
# is placed in  modeled_reviewed_com (dbo_V2_workflows) and thresh_note_low (dbo_MRT_OverallFeedback)

# possibilities that need to be accounted for
# - one or more than one algorithm chosen for each species run
# - one or more than one threshold chosen for each algorithm
# - HUCs chosen to be added or removed at this final step
# - create a final metadata pdf with review status and stars

#setup ----

library(here)
library(RSQLite)
library(rgdal)
library(sf)
library(odbc)
library(DBI)

# RStudio trys to autoload some or all of these packages
# when the saved data (model_rdata) are loaded. It fails miserably. 
# Load them manually here
library(dismo)
library(raster)
library(ROCR)
library(xgboost)
library(randomForest)

library(stars)
library(dplyr)
library(terra)
library(smoothr)

library(xtable)
library(knitr)
library(RColorBrewer)
library(stringi)  #only used once, could clean up?

library(tmap)
library(tmaptools)
library(OpenStreetMap)

library(tidyr)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)

# load in the data from the species run
#rm(list=ls())

# get tracking DB connection info
 trackerDsnInfo <- here("_data","databases", "hsm_tracker_connection_string_short.dsn")
# what project?
 prjct <- "AZ_SWG"
# what sqlite db
 sqliteDB <- here("_data","databases","SDM_lookupAndTracking_AZ_phase12spp.sqlite")
# HUC layer
 nm_HUC_file <- here("_data","other_spatial","feature","HUC10.shp")
 
# get species that are ready for export ----

## get model cycle info from the tracking db
cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
# get species with a "usethis" tag
sql <- paste0("SELECT v2_Elements.ID as elements_id, v2_Elements.Taxonomic_Group, v2_Elements.Scientific_Name, ",
              "v2_Cutecodes.cutecode, ",
              "v2_Projects.project, ",
              "v2_ModelCycle.ID as model_cycle_ID, v2_ModelCycle.model_cycle, ",
              "v2_Workflows.modeled, v2_Workflows.model_reviewed_com ",
              "FROM (((v2_Elements INNER JOIN v2_Projects ON v2_Elements.ID = v2_Projects.Elements_ID) ",
              "INNER JOIN v2_ModelCycle ON v2_Elements.ID = v2_ModelCycle.Elements_ID) ",
              "INNER JOIN v2_Workflows ON v2_ModelCycle.ID = v2_Workflows.model_cycle_ID) ",
              "INNER JOIN v2_Cutecodes ON v2_Elements.ID = v2_Cutecodes.Elements_ID ",
              "WHERE (((v2_Projects.project)='", prjct, "') ", 
              "AND ((v2_Workflows.model_reviewed_com) Like '%usethis%')) ",
              "ORDER BY v2_Elements.Taxonomic_Group, v2_Elements.Scientific_Name;")

spp_ready <- dbGetQuery(cn, sql)

sql <- paste0("SELECT MRTOverallFeedback.model_cycle_ID, ",
              "MRTOverallFeedback.model_version, ",
              "MRTOverallFeedback.mrt_cutecode, ",
              "MRTOverallFeedback.mrt_UserID, MRTOverallFeedback.user_rating, ",
              "MRTOverallFeedback.user_thresh_low, MRTOverallFeedback.thresh_note_low, ",
              "MRTOverallFeedback.user_thresh_mid, MRTOverallFeedback.thresh_note_mid, ",
              "MRTOverallFeedback.user_thresh_high, MRTOverallFeedback.thresh_note_high ",
              "FROM MRTOverallFeedback ",
              "WHERE (((MRTOverallFeedback.model_cycle_ID) In (", paste(spp_ready$model_cycle_ID, collapse = ","), ")) ",
              "AND ((MRTOverallFeedback.thresh_note_low) Like '%usethis%'));")

threshInfo <- dbGetQuery(cn, sql)

dbDisconnect(cn)
rm(cn)

# what species have been completed ---
# where are we checking
fp <- file.path("P:", "Regional_SDM","_data","species")
allFiles <- list.files(path = fp, recursive = TRUE, include.dirs = TRUE)
# look for a pdf in the final products folder.
filesDone <- grep("final_products.*.pdf$", allFiles, value = TRUE)
# model runs done
sppDone <- data.frame(fullPath = filesDone,
                cutecode = sapply(filesDone, function(x) strsplit(x,"/")[[1]][[1]], USE.NAMES = FALSE),
                pdf = sapply(filesDone, function(x) strsplit(x,"/")[[1]][[length(strsplit(x,"/")[[1]])]], USE.NAMES = FALSE)
  )
sppDone$model_run <- sub(".pdf","",sppDone$pdf)


# subset the ready list
spp_ready <- spp_ready[!spp_ready$cutecode %in% sppDone$cutecode,]

## Loop through all species ----
for(cc in spp_ready$cutecode){
  print(paste0("working on ", cc))
  # make the output folder
  dir.create(file.path(fp, cc, "outputs","final_products"), showWarnings = FALSE)
  # what algorithms were accepted and what are their thresholds
  tInf <- threshInfo[grepl(cc,threshInfo$model_version),]
  algs <- unique(tInf$mrt_cutecode)
  # if same alg extracted more than once (multiple reviewers of same alg)
  # average their suggested thresholds (only using Low thresh for this project)
  threshByAlg <- aggregate(user_thresh_low ~ mrt_cutecode, tInf, mean)
  threshByAlg$alg <- apply(threshByAlg, MARGIN = 1, 
                           FUN = function(y) strsplit(y[["mrt_cutecode"]], "_")[[1]][[length(strsplit(y[["mrt_cutecode"]], "_")[[1]])]])
  
  ## before thresholding get any new crop/add requests
  # get the original HUC list from sqlite
  db <- dbConnect(SQLite(),dbname=sqliteDB)
  qry <- paste0("SELECT huc10_id from lkpRange ",
                "inner join lkpSpecies on ", 
                "(lkpRange.EGT_ID = lkpSpecies.EGT_ID and ", 
                "nullif(lkpRange.location_use_class,'') IS nullif(lkpSpecies.location_use_class,'')) ", 
                "where lkpSpecies.sp_code = '", cc, "';")
  hucList <- dbGetQuery(db, statement = qry)$huc10_id
  dbDisconnect(db)
  rm(db)
  # get HUC mods
  cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
  sql <- paste0("SELECT model_version, mrt_cutecode, huc_status_type, huc_id ", 
                "FROM MRTDetailedFeedback ", 
                "WHERE model_version = '", tInf$model_version, "';")
  hucMods <- dbGetQuery(cn, sql)
  dbDisconnect(cn)
  rm(cn)
  # for full clarity, join in the actions based on the id
  lkpHucAction <- data.frame("id" = c(1,2,3), "action" = c("add","remove","comment"))  
  hucMods <- merge(hucMods, lkpHucAction, by.x = "huc_status_type", by.y = "id")
    # x <- hucMods[hucMods$action == "remove","huc_id"]
    # x %in% hucList
  # add the adds
  hucList <- c(hucList, hucMods[hucMods$action == "add","huc_id"])
  # remove the removes
  hucList <- hucList[!hucList %in% hucMods[hucMods$action == "remove","huc_id"]]
  # clear dups
  hucList <- unique(hucList)
  # make it spatial (sf)
  qry <- paste("SELECT * from HUC10 where HUC10 IN ('", paste(hucList, collapse = "', '"), "')", sep = "")
  hucRange <- st_zm(st_read(nm_HUC_file, query = qry))
  # dissolve it
  rangeDissolved <- st_union(hucRange)
  # fill holes/slivers
  rangeDissHolesFilled <- smoothr::fill_holes(rangeDissolved, threshold = units::set_units(10, km^2))
  # this is the standard NAD83 Albers projection we are using
  crs_aea <- "EPSG:5070"
  rangeDissHolesFilled <- st_transform(rangeDissHolesFilled, crs_aea)
  
  # get raster from first alg for a mask template
  # to make mask outside of the following loop
  mv <- tInf[tInf$mrt_cutecode == threshByAlg[1,"mrt_cutecode"],"model_version"]
  alg <- threshByAlg[1,"alg"]
  fp_tif_alg <- file.path(fp, cc, "outputs","model_predictions",paste0(mv,"_",alg,".tif"))
  templRas <- terra::rast(fp_tif_alg)
  
  #to spatVect and spatRast
  rangeSV <- vect(rangeDissHolesFilled)
  cropR <- terra::rasterize(rangeSV, templRas)

  # for each alg, create a thresholded raster
  for(i in 1:nrow(threshByAlg)){
    mv <- tInf[tInf$mrt_cutecode == threshByAlg[i,"mrt_cutecode"],"model_version"]
    alg <- threshByAlg[i,"alg"]
    fp_tif_alg <- file.path(fp, cc, "outputs","model_predictions",paste0(mv,"_",alg,".tif"))
    fp_out_tif <- file.path(fp, cc, "outputs","final_products",paste0(mv,"_",alg,"_thresh.tif"))
    inRas <- rast(fp_tif_alg)
    # classify 
    thresh <- threshByAlg[i,"user_thresh_low"]
    # all values >= thresh and <= 1 become 1, etc.
    m <- c(0, thresh, 0,
           thresh, 1, 1)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    # rc1 <- terra::classify(inRas, rclmat, include.lowest=TRUE, 
    #                 filename = fp_out_tif, overwrite=TRUE, datatype = "INT2S", 
    #                 gdal=c("TFW=YES", "TILED=YES", "COMPRESS=LZW"))
    rc1 <- terra::classify(inRas, rclmat, include.lowest=TRUE)
    #fp_out_tif_crop <- file.path(fp, cc, "outputs","final_products",paste0(mv,"_",alg,"_thresh_crop.tif"))
    # mask it
    maskedRas <- terra::mask(rc1, cropR, filetype = "GTiff", filename=fp_out_tif, datatype = "INT2S", 
                             overwrite=TRUE, gdal=c("TFW=YES", "TILED=YES", "COMPRESS=LZW"))
    # also write out continuous raster as a deliverable
    outRas <- file.path(fp, cc, "outputs","final_products",paste0(mv,"_",alg,".tif"))
    maskedContinuousRas <- terra::mask(inRas, cropR, filetype = "GTiff", filename=outRas,
                                       overwrite=TRUE, gdal=c("TFW=YES", "TILED=YES", "COMPRESS=LZW"))
  }
  # if more than one alg, create an 'agreement' raster, summing the 0/1
  if(nrow(threshByAlg)>1){
    stop("untested code for an agreement raster")
    algs <- sapply(threshByAlg[,"mrt_cutecode"], function(x) strsplit(x,"_")[[1]][[length(strsplit(x,"_")[[1]])]], USE.NAMES = FALSE)
    mv <- unique(tInf[,"model_version"])
    if(length(mv) > 1){stop("model version mismatch")}
    inRasVec <- file.path(fp, cc, "outputs","final_products",paste0(mv,"_",algs,"_thresh.tif"))
    outRas <- file.path(fp, cc, "outputs","final_products",paste0(mv,"_sum_thresh.tif"))
    rStack <- rast(inRasVec)
    rOut <- sum(rStack, filename = outRas, overwrite = TRUE)
    #writeRaster(y, outRas)
  }
  # update and write pdf ----
  #### most cribbed from script 4c

  # load Rdata files ----
  runSDM_dat <- file.path(fp, cc,paste0("runSDM_paths_",mv,".RData"))
  load(runSDM_dat)
  for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])

  rdataFile <- file.path(fp, cc, "outputs","rdata",paste0(mv,".RData"))
  load(rdataFile)
  
  ## Additional Metadata Comments: get any current documentation ----
  db <- dbConnect(SQLite(),dbname=sqliteDB)
  
  SQLquery <- paste("SELECT * ",
                    " FROM tblModelResults ",
                    "WHERE model_run_name ='", mv , "'; ", sep="")
  dbDisconnect(db)
  ## update Model Evaluation and Use data (rubric table) ----
  # get most recent data from the tracking DB for two of the rubric table fields that may vary
  cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
  sql <- paste0("SELECT v2_Elements.ID, ",
                "v2_Cutecodes.Elements_ID, ",
                "v2_Cutecodes.cutecode, ",
                "v2_ModelCycle.model_cycle, ",
                "v2_Workflows.locality_data_eval_rubric, ",
                "v2_Workflows.model_reviewed ", 
                "FROM ((v2_Elements INNER JOIN v2_Cutecodes ON v2_Elements.ID = v2_Cutecodes.Elements_ID) ", 
                "INNER JOIN v2_ModelCycle ON v2_Elements.ID = v2_ModelCycle.Elements_ID) ",
                "INNER JOIN v2_Workflows ON v2_ModelCycle.ID = v2_Workflows.model_cycle_ID ",
                "WHERE (((v2_Cutecodes.cutecode)='", cc, "'));")
  evalAndReviewStatus <- dbGetQuery(cn, sql)
  modelCycleData <- evalAndReviewStatus[,c("cutecode","model_cycle")]
  # if more than one cycle, get the most recent cycle
  if(nrow(evalAndReviewStatus) > 1){
    evalAndReviewStatus <- evalAndReviewStatus[order(evalAndReviewStatus$model_cycle, decreasing = TRUE),]
    evalAndReviewStatus <- evalAndReviewStatus[1,]
  }
  dbDisconnect(cn)
  rm(cn)
  
  ## data quality ----
  dqMatrix <- data.frame("dataQuality" = c(1,2,3),
                         "dqAttribute" = c("C","A","I"),
                         "dqComments" = c("Data taken from outside sources and may or may not be vetted for accuracy or weighted for spatial representation.",
                                          "Heritage Network data augmented with outside data which may or may not be vetted for accuracy or weighted for spatial representation.",
                                          "Heritage Network (and possibly outside) data are vetted for accuracy and weighted for spatial representation."))
  dqUpdate <- dqMatrix[match(evalAndReviewStatus$locality_data_eval_rubric, dqMatrix$dataQuality),]
  #push it up to sqlite DB
  db <- dbConnect(SQLite(),dbname=sqliteDB)
  sql <- paste0("update lkpSpeciesRubric set spdata_dataqual = '", dqUpdate$dqAttribute, 
                "', spdata_dataqualNotes = '", dqUpdate$dqComments, 
                "' where sp_code = '", cc, "' ;")
  dbExecute(db, statement = sql)
  
  ## performance ----
  # get performance data
  
  # add a TSS column to theshByAlg
  threshByAlg$TSS <- NA
  # for each alg and expert thresh get TSS
  for(i in 1:nrow(threshByAlg)){
    alg <- strsplit(threshByAlg[i,"mrt_cutecode"], "_")[[1]][[length(strsplit(threshByAlg[i,"mrt_cutecode"], "_")[[1]])]]
    thresh <- threshByAlg[i,"user_thresh_low"]
    if(alg == "rf"){
      allVotes <- data.frame(rf.full$y, rf.full$votes/numCores, rf.df.full[,c("group_id", "stratum")])
      presPredpres <- allVotes[allVotes$rf.full.y ==1 & allVotes$X1 >= thresh,]
      presPredAbs <- allVotes[allVotes$rf.full.y ==1 & allVotes$X1 < thresh,]
      absPredAbs <- allVotes[allVotes$rf.full.y ==0 & allVotes$X1 < thresh,]
      absPredPres <- allVotes[allVotes$rf.full.y ==0 & allVotes$X1 >= thresh,]
      Sens <- (nrow(presPredpres) + nrow(absPredAbs))/nrow(allVotes)
      Spec <- nrow(absPredAbs)/((nrow(absPredPres) + nrow(absPredAbs)))
      threshByAlg[i,"TSS"] <- Sens + Spec - 1
    }
    if(alg == "xgb"){
      # the 'handle' might go awry
      xgb.full <- xgb.Booster.complete(xgb.full)
      #needs to be re-created ... contains an external pointer 
      df.full.s.xgb <- xgb.DMatrix(as.matrix(xgb.df.full.s[,6:ncol(xgb.df.full.s)]), 
                                   label=as.integer(as.character(xgb.df.full.s$pres)))
      xgb.predicted <- predict(xgb.full, df.full.s.xgb)
      xgb.predicted <- as.data.frame(cbind(xgb.df.full.s[,c("pres","group_id","stratum")], "pred" = xgb.predicted))
      presPredpres <- xgb.predicted[xgb.predicted$pres ==1 & xgb.predicted$pred >= thresh,]
      presPredAbs <- xgb.predicted[xgb.predicted$pres ==1 & xgb.predicted$pred < thresh,]
      absPredAbs <- xgb.predicted[xgb.predicted$pres ==0 & xgb.predicted$pred < thresh,]
      absPredPres <- xgb.predicted[xgb.predicted$pres ==0 & xgb.predicted$pred >= thresh,]
      Sens <- (nrow(presPredpres) + nrow(absPredAbs))/nrow(allVotes)
      Spec <- nrow(absPredAbs)/((nrow(absPredPres) + nrow(absPredAbs)))
      threshByAlg[i,"TSS"] <- Sens + Spec - 1
    }
    if(alg == "me"){
      me.predicted <- predict(me.out.fin, me.df.full.s, type = "prob")
      me.predicted <- as.data.frame(cbind(me.df.full.s[,c("pres","group_id","stratum")], "pred" = me.predicted))
      presPredpres <- me.predicted[me.predicted$pres ==1 & me.predicted$pred >= thresh,]
      presPredAbs <- me.predicted[me.predicted$pres ==1 & me.predicted$pred < thresh,]
      absPredAbs <- me.predicted[me.predicted$pres ==0 & me.predicted$pred < thresh,]
      absPredPres <- me.predicted[me.predicted$pres ==0 & me.predicted$pred >= thresh,]
      Sens <- (nrow(presPredpres) + nrow(absPredAbs))/nrow(allVotes)
      Spec <- nrow(absPredAbs)/((nrow(absPredPres) + nrow(absPredAbs)))
      threshByAlg[i,"TSS"] <- Sens + Spec - 1
    }
  }
  
  meanTSS <- mean(threshByAlg$TSS)

  prfmcMatrix <- data.frame("pAttribute" = c("C","A"),
                            "pComments" = c("Mean TSS for expert-derived thresholds < 0.6.",
                                            "Mean TSS for expert-derived thresholds >= 0.6."))
  prfmAtt <- ifelse(meanTSS<=0.6, "C", "A")
  prfmUpdate <- prfmcMatrix[match(prfmAtt, prfmcMatrix$pAttribute),]
  sql <- paste0("update lkpSpeciesRubric set process_perform = '", prfmUpdate$pAttribute,
                "', process_performNotes = '", prfmUpdate$pComments,
                "' where sp_code = '", ElementNames$Code, "' ;")
  dbExecute(db, statement = sql)
  
  ## model review ----
  revMatrix <- data.frame("rAttribute" = c("C","A"),
                          "rComments" = c("Model was not reviewed by regional, taxonomic experts.",
                                          "Model was reviewed by regional, taxonomic experts."))
  revAtt <- ifelse(!is.na(evalAndReviewStatus$model_reviewed) , "A", "C")
  revUpdate <- revMatrix[match(revAtt, revMatrix$rAttribute),]
  sql <- paste0("update lkpSpeciesRubric set process_review = '", revUpdate$rAttribute, 
                "', process_reviewNotes = '", revUpdate$rComments, 
                "' where sp_code = '", cc, "' ;")
  dbExecute(db, statement = sql)
  
  ## iterative ----
  iterMatrix <- data.frame("iAttribute" = c("C","A"),
                           "iComments" = c("Model not re-run with new or modified data.",
                                           "Model was re-run with new or modified data."))
  nCycles <- nrow(modelCycleData)
  maxCycle <- max(modelCycleData$model_cycle)
  if(nCycles > 1){
    iterAtt <- "A"
  } else {
    iterAtt <- "C"
  }
  iterUpdate <- iterMatrix[match(iterAtt, iterMatrix$iAttribute),]
  sql <- paste0("update lkpSpeciesRubric set iterative = '", iterUpdate$iAttribute, 
                "', iterativeNotes = '", iterUpdate$iComments, 
                "' where sp_code = '", cc, "' ;")
  dbExecute(db, statement = sql)
  
  ## clean up ----
  dbDisconnect(db)
  
  #### now the pdf ----
  # Mostly cribbed from 5_createMetadata.r

  setwd(file.path(fp, cc, "outputs","final_products"))
  nm_db_file <- sqliteDB

  ## get grank definition for header ----
  db <- dbConnect(SQLite(),dbname=nm_db_file) 
  SQLquery <- paste0("SELECT rank, rankname FROM lkpRankDefinitions where rank = '",ElementNames$rounded_g_rank,"';", sep="")
  grank_desc <- dbGetQuery(db, SQLquery)
  dbDisconnect(db)
  
  final_Alg_List <- threshByAlg$alg
  
  ##
  ## create table 1, ensemble summary ----
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("SELECT * from lkpAlgorithms;")
  ensemble_details <- dbGetQuery(db, statement = sql)
  dbDisconnect(db)
  
  ensemble_details <- ensemble_details[ensemble_details$shortCode %in% threshByAlg$alg,
                                       c("fullName","shortCode","rPackage")]
  names(ensemble_details) <- c("Name", "Code","R package")
  # ensemble_details is used in knitr file
  rm(db, sql)
  
  ##
  ## create table 2, summary of input data ----
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("SELECT * from tblModelInputs where model_run_name = '", 
                model_run_name, "';")
  inputs <- dbGetQuery(db, statement = sql)
  dbDisconnect(db)
  
  # Assume groups and pres inputs are the same among algorithms
  # but background inputs vary
  summ.table <- data.frame(
    Sample=c("Presence locations (groups)",
             "Subsamples within groups",
             "Total presence inputs",
             paste0("Background inputs - ", inputs$algorithm)),
    Count=c(
      ifelse(inputs$jckn_grp_column[[1]] == "stratum", 
             inputs$feat_count[[1]],
             inputs$feat_grp_count[[1]]
      ),
      round(inputs$mn_grp_subsamp[[1]],2),
      inputs$tot_obs_subsamp[[1]],
      paste0(inputs$tot_bkgd_subsamp)
    ))
  # summ.table is what gets used in knitr file
  rm(db, sql)
  
  ##
  ## create table 3, summary of validation statistics ----
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("SELECT * from tblModelResultsValidationStats where model_run_name = '", 
                model_run_name, "';")
  vstats <- dbGetQuery(db, statement = sql)
  dbDisconnect(db)
  
  metricsToGet <- c("AUC","Sensitivity","Specificity","TSS")
  colsToGet <- c("algorithm","metric","metric_mn","metric_sd")
  vstats <- vstats[vstats$metric %in% metricsToGet,colsToGet]
  names(vstats) <- c("algorithm","metric","mean","SD")
  vstats$evalOut <- paste0(round(vstats$mean,2), "(",round(vstats$SD,2), ")")
  vstats.s <- vstats[,c("algorithm","metric","evalOut")]
  
  # convert to wide format
  vstats.w <- spread(vstats.s, metric, evalOut)
  names(vstats.w) <- c("alg","AUC","Sens","Spec","TSS")
  
  # add expert revew TSS
  expertTSS <- threshByAlg[,c("alg","TSS")]
  names(expertTSS) <- c("alg","expertThreshTSS")
  expertTSS$expertThreshTSS <- round(expertTSS$expertThreshTSS, 2)
  vstats.w <- merge(vstats.w, expertTSS, all.x = TRUE)
  
  # vstats.w is what gets used in knitr file
  rm(db, sql, metricsToGet, colsToGet, vstats.s, vstats)
  
  ## use TSS calc from expert review for thermometer figure ----
  summaryTSS <- meanTSS
  
  ##
  ## create table with all other modeling input settings ----
  #  this is placed at the end in Appendix 2
  #first get envars counts
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("SELECT * from tblModelResultsVarsUsed where model_run_name = '", 
                model_run_name, "';")
  varsUsedStats <- dbGetQuery(db, statement = sql)
  dbDisconnect(db)
  varsUsedStats <- varsUsedStats[varsUsedStats$inFinalModel == 1,]
  vuStats <- aggregate(varsUsedStats$inFinalModel, by = list(varsUsedStats$algorithm), sum)
  names(vuStats) <- c("algo","value")
  
  vuStats <- cbind("Name"= "number of predictors used", vuStats)
  vuStatsList <- split(vuStats, f = vuStats$algo)
  vuStatsList <- lapply(vuStatsList, FUN = function(x) x[,!names(x)=="algo"])
  
  for(algo in names(vuStatsList)){
    if(algo == "me"){
      algodat <- data.frame(Name = c("linear feature type used",
                                     "product feature type used",
                                     "quadratic feature type used",
                                     "hinge feature type used"),
                            value = c("yes","yes","yes","yes"))
      vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
    }
    if(algo == "xgb"){
      algodat <- data.frame(Name = c(
        "iterations",
        "eta",
        "max depth",
        "gamma",
        "colsample by tree",
        "min child weight",
        "subsample",
        "objective"),
        value = c(
          xgb.full$niter,
          xgb.full$params$eta,
          xgb.full$params$max_depth,
          xgb.full$params$gamma,
          xgb.full$params$colsample_bytree,
          xgb.full$params$min_child_weight,
          xgb.full$params$subsample,
          xgb.full$params$objective
        ))
      #paste0(deparse(xgb.full$call), collapse = ""),
      vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
    }
    if(algo == "rf"){
      algodat <- data.frame(Name = c(
        "mtry",
        "number of trees",
        "type of trees"
      ),
      value = c(
        rf.full$mtry,
        rf.full$ntree,
        rf.full$type
      )
      )
      vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
    }
    if(algo == "gam"){
      algodat <- data.frame(Name = c(
        "method",
        "family",
        "validation method"
      ),
      value = c(
        "gamSpline",
        "binomial (logit)",
        "Leave one out by group (LGOCV)"
      )
      )
      vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
    }
    if(algo == "glm"){
      algodat <- data.frame(Name = c(
        "method",
        "model type",
        "family"
      ),
      value = c(
        glmFit1$method,
        glmFit1$modelType,
        paste0(glmFit1$finalModel$family$family,":", glmFit1$finalModel$family$link)
      )
      )
      vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
    }
    if(algo == "mars"){
      algodat <- data.frame(Name = c(
        "method",
        "model type",
        "nprune",
        "degree"
      ),
      value = c(
        marsFit1$method,
        marsFit1$modelType,
        marsFit1$bestTune$nprune,
        marsFit1$bestTune$degree
      )
      )
      vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
    }
  }
  attr(vuStatsList, "subheadings") <- paste0("\\textcolor{",
                                             names(vuStatsList), "Color}{", 
                                             "Algorithm = ",  names(vuStatsList),"}")
  
  # vuStatsList is what gets used in knitr file
  rm(db, sql, varsUsedStats, vuStats, algodat, algo)
  
  ##
  ## build ROC plot ----
  #### this is all in the rnw, possibly change to ggplot, then build it here and print it there
  
  ## build lookup for line details and colors ----
  lineColors <- brewer.pal(6, "Dark2")[1:length(ensemble_algos)]
  
  figSpecs <- data.frame(algos = ensemble_algos,
                         col = lineColors,
                         lwd = c(rep(2,length(ensemble_algos))),
                         lty = c(rep(1,length(ensemble_algos))),
                         htmlCol = sub("#","",lineColors),
                         stringsAsFactors = FALSE)
  
  # set the knitr hook to create color definitions within the rnw file
  # based on the algorithm color definitions
  defs <- vector("list",nrow(figSpecs))
  for(i in 1:nrow(figSpecs)){
    defs[[i]] <- paste0("\\\\definecolor{",figSpecs$algos[[i]],"Color}{HTML}{",figSpecs$htmlCol[[i]],"}")
  } 
  
  knit_hooks$set(document = function(x) {
    sub('%CustomColorDefsHere', paste0(unlist(defs), "\n", collapse = ""), x)
  })
  
  ##
  ## build importance plot ----
  
  # get the vars used for each
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("SELECT * from tblModelResultsVarsUsed where model_run_name = '", 
                model_run_name, "';")
  varsImp <- dbGetQuery(db, statement = sql)
  # get full names
  sql <- "SELECT gridName, fullName FROM lkpEnvVars"
  varNms <- dbGetQuery(db, statement = sql)
  dbDisconnect(db)
  
  # remove vars not used by any algo
  varsImp <- varsImp[varsImp$inFinalModel == 1,]
  
  # merge in full name, reduce cols, chop to algos used in final
  varsImp.full <- merge(varsImp, varNms)
  varsImp <- varsImp.full[varsImp.full$algorithm %in% final_Alg_List,c("algorithm","fullName","impVal")]
  
  # do it in long format ...
  # standardize to 0-1 then sort by mean importance (using factors)
  for(algo in final_Alg_List){
    algoLocs <- varsImp$algorithm == algo
    varsImp[algoLocs,"impVal"] <- varsImp[algoLocs, "impVal"]/max(varsImp[algoLocs,"impVal"])
  }
  
  #try sorting with zeros added
  varsSorted <- varsImp %>%
    group_by(fullName) %>%
    summarise_at(vars(impVal), function(x) { sum(x)/length(final_Alg_List)}) %>%
    arrange(impVal)
  
  # to factors for correct ordering in ggplot
  varsSorted$fullName <- factor(varsSorted$fullName, levels = varsSorted$fullName)
  varsImp$fullName <- factor(varsImp$fullName, levels = varsSorted$fullName)
  varsImp <- varsImp[order(as.integer(varsImp$fullName)),]
  
  #use same colors as ROC plot
  scaleVec <- figSpecs$col
  names(scaleVec) <- figSpecs$algos
  
  # with mean as thick grey line, need to plot it first so its on the bottom
  impPlot <- ggplot(data = varsSorted) + 
    xlab(bquote(atop("lower" %->% "greater", "importance"))) + 
    geom_path(data = varsSorted, aes(x=impVal, y=fullName, group = 1),
              color="grey60", size = 1.5) + 
    geom_point(data = varsImp, 
               aes(x = impVal, y = fullName, color = algorithm)) + 
    geom_path(data = varsImp, 
              aes(x = impVal, y = fullName, color = algorithm, group = algorithm)) + 
    scale_color_manual(values = scaleVec) + 
    theme_classic() + 
    theme(axis.title.y = element_blank(),
          text = element_text(size=8),
          legend.position = c(0.85,0.15)) + 
    geom_hline(yintercept = 1:nrow(varsSorted), 
               linetype = "18", color = "grey40", size = 0.1)
  
  ##
  ## build partial plots ----
  # find the longest of our set of algos
  # this lines are for the slim chance we have less than 9 vars total. Not likely now. 
  
  maxVars <- 0
  for(algo in final_Alg_List){
    objName <- paste0(algo, ".pPlots")
    if(algo == "rf"){
      numVars <- length(get(objName))
    } else {
      numVars <- length(get(objName)$fullNames)  
    }
    if(numVars > maxVars) {
      maxVars <- numVars
      mostPplots <- objName
      mostPplotsAlgo <- algo
    }
  }
  # create how many?
  if(maxVars < 9){
    numPPl <- maxVars
  } else {
    numPPl <- 9
  }
  rm(maxVars, objName, numVars)
  
  # get the order used in importance plot
  pplotVars <- varsSorted[order(varsSorted$impVal, decreasing = TRUE), "fullName"]
  pplotVars <- pplotVars[1:numPPl,]
  
  # using info from importance plot, which of the top 9 envars is in the most models?
  # easiest way to get the most complete legend
  topVars <- varsImp[varsImp$fullName %in% pplotVars$fullName,]
  varCount <- aggregate(topVars$fullName, list(topVars$fullName), length)
  varCount$order <- nrow(varCount):1
  maxCount <- max(varCount$x)
  varCount <- varCount[order(varCount$order),]
  plotForLeg <- min(grep(maxCount, varCount$x))
  rm(topVars, varCount, maxCount)
  
  # make a list to fill with grobs
  grobList <- vector("list",numPPl)
  names(grobList) <- 1:numPPl
  
  # get the location of the longest set
  if(mostPplotsAlgo == "rf"){
    elist <- unlist(lapply(get(mostPplots), FUN = function(x) x$fname))  
  } else {
    elist <- NA
  }
  
  for (plotpi in 1:numPPl){
    evar <- pplotVars$fullName[[plotpi]]
    
    #get gridname
    grdName <- unique(varsImp.full[varsImp.full$fullName == evar, "gridName"])
    
    #dens data
    df.full <- rbind(df.in, df.abs)
    densdat <- data.frame(x = df.full[,grdName], pres = df.full[,"pres"])
    
    # pplot data
    # do rf only if there are data
    if(exists("rf.pPlots")){
      rf.elist <- unlist(lapply(rf.pPlots, FUN = function(x) x$fname))
      rfLoc <- match(evar, rf.elist)
    } else {
      rfLoc <- NA
    }
    if(exists("rf.pPlots") & !is.na(rfLoc)){
      grdFullName <- rf.pPlots[[rfLoc]]$fname
      dat <- data.frame(x = rf.pPlots[[rfLoc]]$x, y = rf.pPlots[[rfLoc]]$y)
      dat <- cbind(dat, algo = "rf")
      #standardize 0-1
      dat$y <- (dat$y - min(dat$y))/(max(dat$y)-min(dat$y))    
    } else {
      dat <- data.frame(x = numeric(), y = numeric(), algo = character())
    }
    
    # check and use xgb if there are data
    if(exists("xgb.pPlots")){
      if(grdName %in% dimnames(xgb.pPlots$data)[[2]]){
        xgbdat <- data.frame(xgb.pPlots$data)
        xgbresp <- data.frame(xgb.pPlots$shap_contrib)
        xgbdat.b <- data.frame(x = xgbdat[,grdName], y = xgbresp[,grdName], algo = "xgb")
        # all zeros means xgb dropped it in final model; skip for partial plot
        # so if all responses don't equal zero, get the data
        if(all(xgbdat.b$y != 0)){
          #standardize 0-1
          xgbdat.b$y <- (xgbdat.b$y - min(xgbdat.b$y))/(max(xgbdat.b$y)-min(xgbdat.b$y))
          #order, ascending
          xgbdat.b <- xgbdat.b[order(xgbdat.b$x),]
          #smooth it
          #dat <- rbind(dat, xgbdat.b)
          dat <- rbind(dat, data.frame(supsmu(xgbdat.b$x, xgbdat.b$y), algo = "xgb"))
          #rm(xgbdat, xgbresp, xgbdat.b)
        }
      }
    }
    
    # check and use me if there are data
    if(exists("me.pPlots")){
      melist <- unlist(lapply(me.pPlots, FUN = function(x) x$gridName))
      if(grdName %in% melist){
        grdLoc <- match(grdName, melist)
        medat <- data.frame(x = me.pPlots[[grdLoc]]$x, y = me.pPlots[[grdLoc]]$y)
        medat <- cbind(medat, algo = "me")
        #standardize 0-1
        medat$y <- (medat$y - min(medat$y))/(max(medat$y)-min(medat$y))
        dat <- rbind(dat, medat)
        rm(grdLoc)
      }
    }
    
    # check and use gam if there are data
    if(exists("gam.pPlots")){
      dfPointer <- unlist(lapply(gam.pPlots, FUN = function(x){grdName %in% names(x)}))
      if(TRUE %in% dfPointer){
        gamdat <- gam.pPlots[dfPointer][[1]]
        names(gamdat) <- c("x","y")
        gamdat <- cbind(gamdat, algo = "gam")
        #standardize 0-1
        gamdat$y <- (gamdat$y - min(gamdat$y))/(max(gamdat$y)-min(gamdat$y))
        dat <- rbind(dat, gamdat)
        rm(dfPointer)
      }
    }
    # check and use glm if there are data
    if(exists("glm.pPlots")){
      dfPointer <- unlist(lapply(glm.pPlots, FUN = function(x){grdName %in% names(x)}))
      if(TRUE %in% dfPointer){
        glmdat <- glm.pPlots[dfPointer][[1]]
        names(glmdat) <- c("x","y")
        glmdat <- cbind(glmdat, algo = "glm")
        #standardize 0-1
        glmdat$y <- (glmdat$y - min(glmdat$y))/(max(glmdat$y)-min(glmdat$y))
        dat <- rbind(dat, glmdat)
        rm(dfPointer)
      }
    }
    # check and use mars if there are data
    if(exists("mars.pPlots")){
      dfPointer <- unlist(lapply(mars.pPlots, FUN = function(x){grdName %in% names(x)}))
      if(TRUE %in% dfPointer){
        marsdat <- mars.pPlots[dfPointer][[1]]
        names(marsdat) <- c("x","y")
        marsdat <- cbind(marsdat, algo = "mars")
        #standardize 0-1
        marsdat$y <- (marsdat$y - min(marsdat$y))/(max(marsdat$y)-min(marsdat$y))
        dat <- rbind(dat, marsdat)
        rm(dfPointer)
      }
    }
    
    pplot <- ggplot(data = dat, aes(x=x, y=y, color = algo)) + 
      geom_line(size = 1) +
      xlab(evar) + 
      scale_x_continuous(limits = c(min(dat$x), max(dat$x)), 
                         expand = expansion(mult = c(0.05))) +
      theme_classic() +
      theme(axis.title.y = element_blank(), legend.position = "none",
            plot.margin = margin(t = 1, r = 5, b = 5, l = 5, unit = "pt"),
            text = element_text(size=8),
            panel.border = element_rect(colour = "black", fill=NA, size=0.25)
      ) + 
      scale_color_manual(values = scaleVec)
    
    # create the density plot
    densplot <- ggplot(data = densdat, aes(x = x, color = factor(pres, labels = c("background","presence")))) + 
      geom_density(size = 0.5, show.legend = FALSE) + 
      # scale_x_continuous(limits = c(min(densdat$x), max(densdat$x)),
      #                    expand = expansion(mult = c(0.05)),
      #                    breaks = NULL) +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) + 
      theme_classic() + 
      theme(axis.title.y = element_blank(), legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            plot.margin = margin(t = 2, r = 0, b = 1, l = 0, unit = "pt")) +
      scale_color_manual(values=c("grey60", "black")) 
    #theme_void()
    
    # now do the layout
    gdens <- ggplotGrob(densplot)
    gpplt <- ggplotGrob(pplot)
    panel_id <- gpplt$layout[gpplt$layout$name == "panel",c("t","l")]
    gpplt <- gtable_add_rows(gpplt, unit(0.25,"null"), 0)
    gpplt <- gtable_add_grob(gpplt, gdens,
                             t = 1, l = panel_id$l)
    #grid.newpage()
    #grid.draw(gpplt)
    grobList[[plotpi]] <- gpplt
    
    # if on loop with most lines, extract legends
    if(plotpi == plotForLeg){
      # Function to extract legend
      g_legend <- function(a.gplot){ 
        tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
        legend <- tmp$grobs[[leg]] 
        legend
      } 
      # extract them
      legPlot1 <- pplot + 
        labs(color = "Algorithm") +
        theme(legend.position = "bottom",
              legend.margin=margin(t=0, r=0, b=0, l=0, unit="pt"),
              text = element_text(size=14))
      legend1 <- g_legend(legPlot1) 
      
      legPlot2 <- densplot + 
        geom_freqpoly(binwidth = 1000) + # hack to get lines instead of squares in legend
        #scale_x_continuous() + 
        labs(color = "Density") +
        theme(legend.position = "bottom",
              legend.margin=margin(t=0, r=0, b=0, l=0, unit="pt"),
              text = element_text(size=14))
      legend2 <- g_legend(legPlot2)
    }
    
  }
  
  # set up legend grobs
  legGb <- arrangeGrob(grobs=list(legend2, legend1), 
                       layout_matrix=rbind(c(1,2)))
  
  # set up full figure
  gt <- arrangeGrob(grobs=grobList, 
                    layout_matrix=rbind(c(1,2,3),
                                        c(4,5,6),
                                        c(7,8,9)),
                    left = "Relative Response")
  
  gtl <- gtable_add_rows(gt, unit(0.25, "null"), pos = -1)
  gtl <- gtable_add_grob(gtl, legGb, t = 4, l = 2, b = 4, r = 4)
  
  #grid.newpage()
  #grid.draw(gtl)

  ## build the map ----
  
  # get the name of the raster we'll be using based on work above
  # if there's a summed thresh raster, use that, otherwise use the singleton continuous ras
  sumThreshRas <- file.path(fp, cc, "outputs","final_products",paste0(mv,"_sum_thresh.tif"))
  continuousRas <- file.path(fp, cc, "outputs","final_products",paste0(mv,"_",final_Alg_List[[1]],".tif"))
  
  if (file.exists(sumThreshRas)){
    rasName <- sumThreshRas
  } else {
    rasName <- continuousRas
  }

  ras <- terra::rast(rasName)
  crs(ras) <- crs_aea
  studyAreaExtent <- rangeDissHolesFilled
  nm_refBoundaries <- sub("G:/_Projects/AZGFD/","P:/",nm_refBoundaries) # fix for G to P switch
  referenceBoundaries <- st_read(nm_refBoundaries, quiet = TRUE) # name of state boundaries file
  referenceBoundaries <- st_transform(referenceBoundaries, crs_aea)
  
  # project to match raster, just in case
  studyAreaExtent <- st_transform(studyAreaExtent, crs_aea)

  # set up figure
  nclr <- 5
  clrs <- brewer.pal('Blues',n=nclr)
  
  # figure out size of study area, expand if less than 889km across,
  #  which is 1;5,000,000 when figure is 7 inches wide (which it is here)
  bbox <- bb(studyAreaExtent)
  studyAreaWidth <- bbox$xmax - bbox$xmin
  studyAreaHeight <- bbox$ymax - bbox$ymin
  if(studyAreaWidth < 889000){
    bbox <- bb(bbox, width = 889000, relative = FALSE)
  }
  if(studyAreaHeight < 889000){
    bbox <- bb(bbox, height = 889000, relative = FALSE)
  }
  
  tmap_options(max.raster = c("plot" = 300000, "view" = 100000))
  tmap_mode("plot")
  # get the basemap
  # for basemap options see http://leaflet-extras.github.io/leaflet-providers/preview/
  # for native options provided by read_osm, see ?OpenStreetMap::openmap
  
  ## this is Esri.WorldGrayCanvas
  #mtype <- 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}.png?'
  
  ## this is CartoDB.Positron
  mtype <- 'https://a.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png'
  basetiles <- read_osm(bbox, type = mtype, ext = 1.1)
  # plot it
  mapFig <- qtm(basetiles) +
    tm_shape(ras) +
    tm_raster(palette = clrs, title = "modeled suitability",
              labels = c("Low Habitat Suitability", rep(" ", nclr-2), "High Habitat Suitability")) +
    tm_shape(referenceBoundaries) +
    tm_borders(col = "grey", lwd = 1) +
    tm_shape(studyAreaExtent) +
    tm_borders(col = "red", lwd = 2) +
    tm_compass(north = 0, type = "arrow", position = c("left","bottom")) +
    tm_scale_bar()
  
  ## Get Program and Data Sources info ----
  op <- options("useFancyQuotes")
  options(useFancyQuotes = FALSE)
  
  db <- dbConnect(SQLite(),dbname=sqliteDB)  
  SQLquery <- paste("Select lkpModelers.ProgramName, lkpModelers.FullOrganizationName, ",
                    "lkpModelers.City, lkpModelers.State, lkpSpecies.sp_code ",
                    "FROM lkpModelers ", 
                    "INNER JOIN lkpSpecies ON lkpModelers.ModelerID=lkpSpecies.ModelerID ", 
                    "WHERE lkpSpecies.sp_code='", model_species, "'; ", sep="")
  sdm.modeler <- dbGetQuery(db, statement = SQLquery)
  # NOTE: use column should be populated with 1/0 for sources of data used
  SQLquery <- paste("SELECT sp.sp_code, sr.ProgramName, sr.State ",
                    "FROM lkpSpecies as sp ",
                    "INNER JOIN mapDataSourcesToSpp as mp ON mp.EGT_ID=sp.EGT_ID ",
                    "INNER JOIN lkpDataSources as sr ON mp.DataSourcesID=sr.DataSourcesID ",
                    # "WHERE mp.use = 1 ",
                    "AND sp.sp_code ='", model_species, "'; ", sep="")
  sdm.dataSources <- dbGetQuery(db, statement = SQLquery)
  sdm.dataSources <- sdm.dataSources[order(sdm.dataSources$ProgramName),]
  
  SQLquery <- paste("SELECT model_end_time date, egt_id, metadata_comments comments",
                    " FROM tblModelResults ", 
                    "WHERE model_run_name ='", model_run_name, "'; ", sep="")
  sdm.customComments <- dbGetQuery(db, statement = SQLquery)
  dbDisconnect(db)
  # assume you want the most recently entered comments, if there are multiple entries
  if(nrow(sdm.customComments) > 1) {
    sdm.customComments <- sdm.customComments[order(sdm.customComments$date, decreasing = TRUE),]
    sdm.customComments.subset <- sdm.customComments[1,]
  } else {
    sdm.customComments.subset <- sdm.customComments
  }
  
  ## Get threshold table information ----
  # get thresholds
  db <- dbConnect(SQLite(),dbname=sqliteDB)  
  SQLquery <- paste("Select ElemCode, algorithm, dateTime, cutCode, cutValue, 
                  capturedGPs, capturedPolys, capturedPts, prpCapGPs, prpCapPolys, prpCapPts ", 
                    "FROM tblModelResultsCutoffs ", 
                    "WHERE model_run_name ='", model_run_name, "'; ", sep="")
  sdm.thresholds <- dbGetQuery(db, statement = SQLquery)
  # restrict to algs we chose
  sdm.thresholds <- sdm.thresholds[sdm.thresholds$algorithm %in% final_Alg_List, ]
  
  # get info about thresholds
  SQLquery <- paste("SELECT cutCode, cutFullName, cutDescription, cutCitationShort, cutCitationFull, sortOrder ", 
                    "FROM lkpThresholdTypes ", 
                    "WHERE cutCode IN (", 
                    toString(sQuote(sdm.thresholds$cutCode)),
                    ");", sep = "")
  sdm.thresh.info <- dbGetQuery(db, statement = SQLquery)
  dbDisconnect(db)
  rm(db)
  
  # merge and sort
  sdm.thresh.merge <- merge(sdm.thresholds, sdm.thresh.info)
  sdm.thresh.merge <- sdm.thresh.merge[order(sdm.thresh.merge$sortOrder),]
  # remove metrics we don't want to display
  sdm.thresh.merge <- sdm.thresh.merge[!sdm.thresh.merge$cutCode %in% 
                                         c("FMeasPt01", "MPVP", "MPVG"),]
  # extract descriptions of those we are using
  thresh.descr <- unique(sdm.thresh.merge[,c("cutCode","cutFullName","cutDescription")])
  names(thresh.descr) <- c("Code","Threshold full name","Threshold description")
  
  sdm.thresh.merge$pctCapGPs <- paste0(round(sdm.thresh.merge$prpCapGPs * 100),"(",
                                       round(sdm.thresh.merge$capturedGPs),")")
  
  sdm.thresh.merge$pctCapPts <- round(sdm.thresh.merge$prpCapPts * 100)
  # subset and remove metrics we don't want to display
  sdm.thresh.merge <- sdm.thresh.merge[,c("cutCode","algorithm","cutValue",
                                          "pctCapGPs","pctCapPts")]
  names(sdm.thresh.merge) <- c("Code","algorithm","Value","Groups","Points")
  sdm.thresh.list <- split(sdm.thresh.merge, f = sdm.thresh.merge$algorithm)
  sdm.thresh.list <- lapply(sdm.thresh.list, FUN = function(x) x[,!names(x)=="algorithm"])
  
  # no colored text
  #attr(sdm.thresh.list, "subheadings") <- paste0("Algorithm = ", names(sdm.thresh.list))
  # with colored text following lines on figures
  attr(sdm.thresh.list, "subheadings") <- paste0("\\textcolor{",
                                                 names(sdm.thresh.list), "Color}{", 
                                                 "Algorithm = ", 
                                                 names(sdm.thresh.list),"}")
  
  # can't get xtable's sanitize functions to work, manually escape % here. 
  # attr(sdm.thresh.list, "message") <- paste0(thresh.descr$cutCode, ": ",
  #                             gsub("%","\\%",thresh.descr$cutDescription, fixed = TRUE))
  
  sdm.thresh.list.xtbl <- xtableList(sdm.thresh.list, 
                                     align = "llrrr",
                                     digits=c(0,0,3,0,0))
  
  thresh.descr.xtbl <- xtable(thresh.descr, 
                              align = "lllp{3in}")
  
  
  # make a url to NatureServe Explorer
  NSurl <- paste("http://explorer.natureserve.org/servlet/NatureServe?searchName=",gsub(" ", "+", ElementNames[[1]], fixed=TRUE), sep="")
  
  ## get Model Evaluation and Use data ----
  db <- dbConnect(SQLite(),dbname=sqliteDB) 
  SQLquery <- paste("Select spdata_dataqual, spdata_abs, spdata_eval, envvar_relevance, envvar_align, process_algo, process_sens, process_rigor, process_perform, process_review, products_mapped, products_support, products_repo, iterative, spdata_dataqualNotes, spdata_absNotes, spdata_evalNotes, envvar_relevanceNotes, envvar_alignNotes, process_algoNotes, process_sensNotes, process_rigorNotes, process_performNotes, process_reviewNotes, products_mappedNotes, products_supportNotes, products_repoNotes, iterativeNotes ", 
                    "FROM lkpSpeciesRubric ", 
                    "WHERE sp_code ='", model_species, "'; ", sep="")
  sdm.modeluse <- dbGetQuery(db, statement = SQLquery)
  dbDisconnect(db)
  
  sdm.modeluse[is.na(sdm.modeluse)] <- " "
  sdm.modeluse[sdm.modeluse=="I"] <- "\\cellcolor[HTML]{9AFF99} Ideal"
  sdm.modeluse[sdm.modeluse=="A"] <- "\\cellcolor[HTML]{FFFFC7} Acceptable"
  sdm.modeluse[sdm.modeluse=="C"] <- "\\cellcolor[HTML]{FD6864} Interpret with Caution"
  
  ## Get env. var lookup table ----
  db <- dbConnect(SQLite(),dbname=sqliteDB) 
  SQLquery <- paste0("SELECT gridName g from tblModelResultsVarsUsed where model_run_name = '",
                     model_run_name, "' and inFinalModel = 1;")
  var_names <- dbGetQuery(db, SQLquery)$g
  SQLquery <- paste("SELECT fullName, description ",
                    "FROM lkpEnvVars ",
                    "WHERE gridName COLLATE NOCASE IN (",
                    toString(sQuote(var_names)),
                    ") ORDER BY fullName;", sep = "")
  sdm.var.info <- dbGetQuery(db, statement = SQLquery)
  names(sdm.var.info) <- c("Variable Name","Variable Description")
  dbDisconnect(db)
  
  # escape symbols for latex
  ls <- c("&","%","$","#","_","{","}")
  for (l in ls) {
    sdm.var.info$`Variable Name` <- gsub(l, paste0("\\",l), sdm.var.info$`Variable Name`, fixed = T)
    sdm.var.info$`Variable Description` <- gsub(l, paste0("\\",l), sdm.var.info$`Variable Description`, fixed = T)
  }
  # replace degree symbols
  for (l in 1:length(sdm.var.info$`Variable Description`)) {
    new.desc <- stri_escape_unicode(sdm.var.info$`Variable Description`[l])
    if (grepl("\\u00b0",new.desc, fixed = T)) 
      sdm.var.info$`Variable Description`[l] <- gsub("\\u00b0", "$^\\circ$", new.desc, fixed = T)
  }
  # put descriptions in parboxes for multiple lines
  sdm.var.info$`Variable Description` <- paste0("\\parbox{20cm}{",sdm.var.info$`Variable Description`,"}")
  
  # fix greater than and less than symbol in rubric table
  sdm.modeluse$process_performNotes <- gsub(">=","$\\\\geq$", sdm.modeluse$process_performNotes)
  sdm.modeluse$process_performNotes <- gsub("<","$<$ ", sdm.modeluse$process_performNotes)
  
  
  ## Run knitr and create metadata ----
  loc_scripts <- sub("G:/_Projects/AZGFD/","P:/",loc_scripts)
  
  knit2pdf(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
  
  # delete .txt, .log etc if pdf is created successfully.
  # fn_ext <- c(".log",".aux",".out")
  # if (file.exists(paste(model_run_name, ".pdf",sep=""))){
  #   #setInternet2(TRUE)
  #   #download.file(fileURL ,destfile,method="auto")
  #   for(i in 1:NROW(fn_ext)){
  #     fn <- paste(model_run_name, fn_ext[i],sep="")
  #     if (file.exists(fn)){ 
  #       file.remove(fn)
  #     }
  #   }
  # }
  
  ## write up output info to tracking DB  ---
  ## If metadata pdf creation successful, then full model run complete,
  ## so this is an appropriate time to populate Tracking DB
  # get tracking DB connection info
  trackerDsnInfo <- here("_data","databases", "hsm_tracker_connection_string_short.dsn")
  
  
  ## get model cycle info from the tracking db
  cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
  # get model cycle we are on
  sql <- paste0("SELECT v2_Elements.ID, v2_Elements.Taxonomic_Group, V2_Elements.Location_Use_Class, ",
                "v2_Cutecodes.cutecode, ",
                "v2_ModelCycle.ID, v2_ModelCycle.model_cycle ",
                "FROM (v2_Elements INNER JOIN v2_Cutecodes ON v2_Elements.ID = v2_Cutecodes.Elements_ID) ",
                "INNER JOIN v2_ModelCycle ON v2_Elements.ID = v2_ModelCycle.Elements_ID ",
                "WHERE (((v2_Cutecodes.cutecode)= '", ElementNames$Code, "'));")
  
  model_cycle <- dbGetQuery(cn, sql)
  names(model_cycle) <- c("Elements_ID","taxonomic_group","luc","cutecode","model_cycle_ID", "model_cycle")
  dbDisconnect(cn)
  
  
  # get most recent cycle (last row after sorting)
  model_cycle <- model_cycle[order(model_cycle$model_cycle),]
  model_cycle <- model_cycle[nrow(model_cycle),]
  
  outputsDat <- model_cycle[,c("model_cycle_ID"), drop = FALSE]
  names(outputsDat) <- "model_cycle_id"
  outputsDat$model_run_name <- model_run_name
  outputsDat$path_to_output <- file.path(loc_model, model_species,"outputs","model_predictions")
  outputsDat$modeling_machine <- model_comp_name
  outputsDat$comment <- NA
  outputsDat$ensemble_code <- NA
  
  # duplicate rows based on number of algorithms
  repTimes <- length(final_Alg_List)
  outputsDat <- outputsDat[rep(1, repTimes),]
  
  outputsDat$algorithm_code <- final_Alg_List
  outputsDat$output_file_name <- paste0(model_run_name,"_",final_Alg_List,".tif")  
  # decision: don't put up info about the mean suitabilities ensemble. Only use it for the metadata map
  
  outputsDat <- outputsDat[,c("model_cycle_id","model_run_name","algorithm_code","output_file_name",
                              "path_to_output","modeling_machine","comment")]
  
  # push up the data
  cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
  
  # are these data already up there? if so, delete and re-upload
  # base it on file name
  sql <- paste0("SELECT * from v2_Outputs where output_file_name IN (",
                toString(sQuote(outputsDat$output_file_name, q = FALSE)), ");")
  
  datUpThere <- dbGetQuery(cn, sql)
  
  if(nrow(datUpThere) > 0){
    sql <- paste0("DELETE from v2_Outputs where ID IN (",
                  toString(datUpThere$ID), ");")
    dbExecute(cn, sql)
  }
  
  # now upload the rows
  dbWriteTable(cn,"v2_Outputs", outputsDat, append = TRUE, row.names = FALSE)
  dbDisconnect(cn)
  rm(cn)
  
  ## clean up ----
  #dbDisconnect(db)
  options(op)
  
  
}


