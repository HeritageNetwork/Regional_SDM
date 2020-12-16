## for exporting outputs for the model review tool
#
# the requested formats are:
#
# It needs to have a folder name equal to a cutecode. Inside, the folder are 3 files:
#  1. modeling-extent.csv
#   a. it only has one field “HUC_10”.
#  2. predictedhabitat-{line or poly}.gdb.zip
#   a. this should be a zipped fgdb
#   b. It should have one feature class
#    i. The feature class should have one field “cutecode”
#    ii. It should already have only data that should show for predicted habitat results.
#  3. {cutecode}.pdf  (the metadata file)
#  4. a JSON file with extra, accessible info for the model review tool

## NOTE: 4-11-2019 For multi species processing, the script first scans for models 
##  that have completed Step 5 but have not yet been packaged
## NOTE: Edit the vector "exclude_these" in line 46 to exclude models that are complete but unready for export
## NOTE: To run a single model manually, enter the species CuteCode on line 69 below and uncomment it

## NOTE: using only rf model (see row 99) so not accounting for ensemble yet!

library(checkpoint)
checkpoint("2020-04-22", scanForPackages = FALSE)

library(raster)
library(here)
library(RSQLite)
library(rgdal)
#library(reticulate) # for direct python calls (alternative to arcgisbinding)
library(RSQLite)
#### arcgisbinding. Here's the call to install if you don't have it
#### install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
#library(arcgisbinding)
library(sf)
library(stars)
library(RJSONIO)
library(geojsonio)

# load in the data from the species run
rm(list=ls())

# get tracking DB connection info
trackerDsnInfo <- here("_data","databases", "hsm_tracker_connection_string_short.dsn")

# arcgisbinding requirement ... 
#arc.check_product()

#####Scan the directories to detect species that have not been packaged for upload and package them all#####

#Splits out character vector of cutecodes fo complete models *Finished*
#Scans for models that have produced the full metadata pdf (finished script 5)
finished <- unique(unlist(sapply(grep("2019",list.files(path=here("_data","species"),pattern=".pdf",recursive = TRUE),value=TRUE),function(x) strsplit(x,"/")[[1]][[1]],USE.NAMES = FALSE)))

#Splits out character vector of cutecodes for species that are finished and already packaged for the model review tool
#Specifically checks for the existence of the metadata pdf inside a model_review_output folder
packaged <- unique(unlist(sapply(grep("model_review_output",list.files(path=here("_data","species"),pattern=".pdf",recursive = TRUE),value=TRUE),function(x) strsplit(x,"/")[[1]][[1]],USE.NAMES = FALSE)))

#Selects cutecodes for models that are finished but not yet exported for review
not_yet_exported <- setdiff(finished,packaged)
not_yet_exported
##If there are models in the list you do not want to export, remove them by adding their cutecodes to "exclude_these"
exclude_these=c("arbopomo") #Delete codes in this vector when they are ready to be packaged

not_yet_exported <- not_yet_exported[!not_yet_exported %in% exclude_these]
not_yet_exported ##These are the final set of models that will be packaged up
length(not_yet_exported)

####For manual use- to run one at a time UNCOMMENT this line and add cutecode(s) of the species interested ####
#not_yet_exported<-c("eriogyps")

###Final step sends the finals to the model_review_staging folder
##Assumes the model_review_staging folder is in the parent directory Lines 204 +206


####Run the packaging tool for all models in your vector####
for (j in 1:length(not_yet_exported)){
  print(paste0("Starting Model Review Tool export for model ",j," of ",length(not_yet_exported)," : ",not_yet_exported[j]))
  model_species <- not_yet_exported[j]
  load(here("_data","species",model_species,"runSDM_paths_most_recent.Rdata"))
  for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])
  
  # load the specific model output rdata file
  model_rdata <- max(list.files(here("_data","species",model_species,"outputs","rdata")))
  load(here("_data","species",model_species,"outputs","rdata",paste0(model_rdata)))
  
  rootPath <- here("_data","species",model_species)
  outpath <- file.path(rootPath, "outputs","model_review_output")
  dir.create(outpath, showWarnings = FALSE)
  setwd(outpath)
  
  # select the threshold type
  threshold_Aqua <- "TenPctile"
  threshold_Terr_a <- "MTPGP"
  threshold_Terr_b <- "TenPctile"
  
  # Get the model type from the sqlite
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("select sp_code, modtype from lkpSpecies where sp_code = '", model_species, "';")
  modType <- dbGetQuery(db, statement = sql)$modtype
  
  if (modType=="A"){ # Aquatic Option 
    # load the shapefile from the latest model run
    shpPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,"_results.shp"))
    shp <- st_read(shpPath, quiet = T)  
  } else if (modType=="T"){ # Terrestrial Option 
    # load the raster from the latest model run
    rasPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,"_rf.tif"))
    #ras <- raster(rasPath)
    ras <- read_stars(rasPath)
    #st_crs(ras) = 102003  ### TODO:: need to track this backwards to find why not set correctly!
  } else {
    print("Model is not of the Terrestrial or Aquatic Type")
  }
  
  # get threshold information
  sql <- paste0("select model_run_name, algorithm, ElemCode, cutCode, cutValue, capturedPts 
                from tblModelResultsCutoffs where model_run_name = '", 
                model_run_name, "' and  algorithm = 'rf';")
  threshInfo <- dbGetQuery(db, statement = sql)
  
  # table storing input data seems to have different date stamp, need to use only
  # cutecode_date, instead of cutecode_date_time
  # TODO: if multiples, grab the most recent by date-time, not the largest count
  # mrn <- strsplit(model_run_name, "_")[[1]]
  # mrn <- paste(mrn[[1]], mrn[[2]], sep = "_")
  # sql <- paste0("select obs_count from tblModelInputs where table_code LIKE '",
  #                 mrn, "%';")
  
  sql <- paste0("select obs_count from tblModelInputs where model_run_name = '",
                model_run_name, "' and algorithm = 'rf';")
  
  inputPts <- dbGetQuery(db, statement = sql)
  inPts <- max(inputPts$obs_count)
  
  dbDisconnect(db)
  #rm(db, mrn, sql, inputPts)
  rm(db, sql, inputPts)
  
  if (modType=="A"){ # Aquatic Option -
    # delete followlines that are not above the threshold
    modelLine <- shp[which(shp[[threshold_Aqua]]==1),]  
    modelLine$cutecode <- model_species
    modelLine <- modelLine[,"cutecode"] 
    threshUsed <- threshold_Aqua
    cutval <- threshInfo[threshInfo$cutCode == threshUsed,"cutValue"]
    threshComm <- paste0("Aquatic model, using ",
                         threshUsed, " as threshold.")
  } else if (modType=="T"){ # Terrestrial Option - 
    # proportion of points captured by MTP by group
    propPts <- threshInfo[threshInfo$cutCode == "MTPGP","capturedPts"]/inPts
    # data show bimodal distribution with 0.4 at the approximate 
    # valley. Lower capture rate suggests very high site specificity which 
    # will in turn show very low prediction extent (high underpredict) when 
    # using mtp by group
    if(propPts <= 0.4){
      threshUsed <- threshold_Terr_b
      cutval <- threshInfo[threshInfo$cutCode == threshUsed,"cutValue"]
      threshComm <- paste0("Model has high site specificity (MTP by group has very low prediction rate), using ",
                           threshUsed, " as threshold.")
    } else {
      threshUsed <- threshold_Terr_a
      cutval <- threshInfo[threshInfo$cutCode == threshUsed,"cutValue"]
      threshComm <- paste0("Model has broad site specificity (MTP by group has high prediction rate), using ",
                           threshUsed, " as threshold.")
      # this is an old workaround, but perhaps still possible, so leaving in for now
      if(cutval == 0 | cutval == 1){
        threshUsed <- threshold_Terr_b
        cutval <- threshInfo[threshInfo$cutCode == threshUsed,"cutValue"]
        threshComm <- paste0("MTP by group thresholds to 0 or 1, using ",
                             threshUsed, " as threshold.")
      }  
    }
    #reclassify the raster and create feature class ----
      ras[[1]][ras[[1]] < cutval] <- NA
      #ras[[1]][ras[[1]] < cutval] <- 0
      ras[[1]][!is.na(ras[[1]])] <- 1
      #ras[[1]][!ras[[1]]==0] <- 1
      # write out the raster, right here!
      # hmm, this works but NoData cells are "nan" in arc which might be funky. Might be better way?
      # not ready for prime time yet
      #cutRasName <- file.path(rootPath, "outputs","model_predictions", paste0(model_run_name,"_", "rf", "_", threshUsed, ".tif"))
      #st_write(ras, cutRasName)
      #write_stars(ras, cutRasName)
      
      modelPoly <- st_as_sf(ras, as_points = FALSE, merge = TRUE)
      # add cutecode as attribute, remove all other columns
      modelPoly$cutecode <- paste0(model_species,"_",threshUsed)
      modelPoly <- modelPoly[,"cutecode"]
  } else {
    print("Model is not of the Terrestrial or Aquatic Type")
  }
  
  # using sf for geojson (immed. below) doesn't include crs, which esri apparently requires
  # layerNm <- paste0(model_species,"_",threshUsed,"_new.geojson")
  # st_write(modelPoly, layerNm, driver = "GeoJSON")
  # layerNm <- paste0(model_species,"_",threshUsed,".shp")
  # st_write(modelPoly, layerNm, append = FALSE)
  
  modelPoly <- st_transform(modelPoly, crs = 4326)

  #write out geojson
  layerNm <- paste0(model_species,"_",threshUsed,".geojson")
  crsString <- paste0('"crs" : {
    "type": "name",
    "properties": {
    "name": "EPSG:',
    st_crs(modelPoly)$epsg,
    '"
      }
    }')
  # crsString <- paste0('"crs" : {
  #   "type": "name",
  #                     "properties": {
  #                     "name": "EPSG:',
  #                     "4326",
  #                     '"
  #                     }
  #                     }')
  
  modelPoly_gjsn <- geojson_json(modelPoly)
  # add crs in location esri needs. Better to use a json tool?
  searchString <- "\"FeatureCollection\",\"features\""
  replaceString <- paste0("\"FeatureCollection\",", crsString, ",\"features\"")
  modelPoly_gjsn_with_ESRI_fix <- sub(searchString, replaceString, modelPoly_gjsn)
  geojson_write(modelPoly_gjsn_with_ESRI_fix, 
                file = layerNm, 
                convert_wgs84 = FALSE,
                pretty=TRUE)
  
  # use bridge to write out file gdb
  # copy over a blank gdb as the bridge can't create them by itself. I would store this in some other directory
  # templateGDB <- here("_data","other_spatial","feature","template_db_predictedhabitat-poly.gdb")
  # templateFiles <- list.files(templateGDB)
  # if (modType=="A"){ # Aquatic Option -
  #   gdbName <- "predictedhabitat-line.gdb"
  # } else if (modType=="T") { # Terrestrial Option -
  #   gdbName <- "predictedhabitat-poly.gdb"
  # }
  # dir.create(gdbName)
  # file.copy(file.path(templateGDB, templateFiles), gdbName)
  
  # if (modType=="A"){ # Aquatic Option -
  #   # write the results
  #   arc.write(paste0(gdbName,"/",model_species,"_", threshold_Aqua), modelLine, validate = TRUE)
  # } else if (modType=="T"){ # Terrestrial Option - 
  #   arc.write(paste0(gdbName,"/",model_species,"_",threshUsed), modelPoly, validate = TRUE)
  # }  
  
  # paste0(gdbName,"/",model_species,"_",threshUsed)
  # layerNm <- paste0(model_species,"_",threshUsed)
  # 
  # #st_write(modelPoly, dsn = gdbName, layer = layerNm, driver = "FileGDB")
  # library(reticulate)
  # use_python("C:/Users/Tim_Howard/AppData/Local/Esri/conda/envs/arcgispro-py3-clone1")
  # use_virtualenv(Sys.getenv("PYTHONPATH"))
  # use_virtualenv("C:/Users/Tim_Howard/AppData/Local/Esri/conda/envs/arcgispro-py3-clone1")
  # 
  # arcpy <- import("arcpy")
  # gdbName <- "predictedhabitat-poly.gdb"
  # arcpy$CreateFileGDB_management(outpath, gdbName)
  # # tried to here
  # inShp <- paste0(outpath, "/modelPoly.shp")
  # outFC <- paste0(outpath, "/", gdbName, "/", model_species, "_mtpg")
  #arcpy$CopyFeatures_management(inShp, outFC)
  
  #clean up
  #delShp <- list.files(path = outpath, pattern = "modelPoly.*")
  #file.remove(paste0(outpath,"/",delShp))
  
  # zip it
  # zfiles <- file.path(gdbName, list.files(gdbName))
  # utils::zip(paste0(gdbName,".zip"), files = zfiles)
  # 
  # #clean up
  # unlink(gdbName, recursive = TRUE)
  
  ## get range data ----
  if (modType=="A"){ # Aquatic Option - Huc10s output as part of the model
    hucPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,"_huc10.csv"))
    huc <- read.csv(hucPath, stringsAsFactors=FALSE, colClasses = "character")
    hucList <- huc$huc10
  } else if (modType=="T"){ # Terrestrial Option - get range info from the DB (as a list of HUCs) 
    dbpath <- here("_data","databases", "SDM_lookupAndTracking_NM.sqlite")
    db <- dbConnect(SQLite(),dbname=dbpath)
    SQLquery <- paste0("SELECT huc10_id from lkpRange
                     inner join lkpSpecies on lkpRange.EGT_ID = lkpSpecies.EGT_ID
                     where lkpSpecies.sp_code = '", model_species, "';")
    hucList <- dbGetQuery(db, statement = SQLquery)$huc10_id
    dbDisconnect(db)
    rm(db)
  }
  
  hucList.df <- data.frame("HUC_10" = hucList)
  write.csv(hucList.df, file = "modeling-extent.csv", row.names = FALSE)
  
  ## move and rename pdf ---
  pdfPath <- here("_data","species", model_species, "outputs","metadata")
  mdOutFiles <- list.files(path = pdfPath, pattern = ".pdf")
  # get most recent in case multiple runs
  mdOutF <- mdOutFiles[order(mdOutFiles, decreasing = TRUE)][[1]]
  shortName <- strsplit(mdOutF, split = "_")[[1]][[1]]
  file.copy(from = paste0(pdfPath,"/",mdOutF), to = paste0(shortName,".pdf"))
  
  ## create json file ----
  ## get model cycle info from the tracking db
  cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
  # get model cycle we are on
  sql <- paste0("select ID, model_cycle from ModelCycle where ",
                "model_cycle = (select max(model_cycle) from ModelCycle where EGT_ID = ", ElementNames$EGT_ID, " ) AND ", 
                "EGT_ID = ", ElementNames$EGT_ID,";")
  model_cycle <- dbGetQuery(cn, sql)
  names(model_cycle) <- c("model_cycle_id","model_cycle")
  dbDisconnect(cn)
  rm(cn)
  
  # uses jsonlite (and adds square brackets around)
  # library(jsonlite)
  # jsDat <- data.frame("modelVersion" = model_run_name,
  #                     ##ANOTHER OPTION if value is desired: 'threshold' = paste0(threshUsed, ":",signif(cutval,3))
  #                     "threshold" = threshUsed,
  #                     "iteration" = model_cycle$model_cycle,
  #                     "iterationNote" = model_comments
  #                     )
  # write_json(jsDat, "modelRunInfo2.json", pretty = TRUE)

  ## option that doesn't include square brackets
  jsDat2 <- RJSONIO::toJSON(
      list("modelVersion" = model_run_name,
        "threshold" = threshUsed,
        "iteration" = model_cycle$model_cycle,
        "iterationNote" = model_comments),
      pretty = TRUE)
  write(jsDat2, "modelRunInfo.json")
  
    
  ## Create cutecode named folder in upload staging area and copy model_review_output files there
  # use substr to get the root drive letter (F: or N:, etc)
  if (modType=="A"){
    staging_path <- file.path(substr(here(), 1, 2),"model_review_staging","upload_staging_2021","aquatic")
  }else if (modType=="T"){
    staging_path <- file.path(substr(here(), 1, 2),"model_review_staging","upload_staging_2021","terrestrial")
  }
  
  spec_stage_path <- file.path(staging_path, model_species)
  dir.create(spec_stage_path, showWarnings = FALSE)
  stagingFiles<-list.files(path=outpath)
  file.copy(stagingFiles,spec_stage_path,overwrite=TRUE)
  
  #Delete raster in temp folder
  temp_path="D:\\R_tmp"
  search=paste0("*r_tmp*",Sys.Date(),"*.gr*")
  delShp <- list.files(path = temp_path, pattern = glob2rx(search))
  try(file.remove(paste0(temp_path,"/",delShp)))
  
  ## update the mobi tracking db in two places
  cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
  # get model cycle we are on
  sql <- paste0("SELECT EGT_ID, Max(model_cycle) as max_Cycle ",
                "FROM ModelCycle INNER JOIN Workflows ON ModelCycle.ID = Workflows.model_cycle_ID ",
                "GROUP BY ModelCycle.EGT_ID ",
                "HAVING (((ModelCycle.EGT_ID)=", ElementNames$EGT_ID, "));")
  model_cycle <- dbGetQuery(cn, sql)
  
  sql <- paste0("SELECT ModelCycle.EGT_ID, ModelCycle.model_cycle, Workflows.ID ",
                "FROM ModelCycle ", 
                "INNER JOIN Workflows ON ModelCycle.ID = Workflows.model_cycle_ID ",
                "WHERE (((ModelCycle.EGT_ID)=", ElementNames$EGT_ID,") ", 
                "AND ((ModelCycle.model_cycle)=", model_cycle$max_Cycle, "));")
  workflow_id <- dbGetQuery(cn, sql)
  
  commentString <- paste(modelrun_meta_data$model_run_name, "prepped for review tool on", Sys.Date())
  sql <- paste0("UPDATE Workflows SET modeled = 1, modeled_com = '",
                commentString,
                "' WHERE ID = ", workflow_id$ID, ";")
  dbExecute(cn, sql)

  ## TODO: tables not ready for this yet 
  # sql <- paste0("INSERT INTO ReviewToolUpload ",
  #               "(EGT_ID, model_cycle_id, cutecode, model_run_name, package_date, threshold_chosen, threshold_value, threshold_comments) ",
  #               "VALUES ( '" , 
  #               ElementNames$EGT_ID, "', ",
  #               model_cycle$model_cycle_id, ", '",
  #               model_species, "', '",
  #               model_run_name, "', '",
  #               as.character(Sys.time()), "', '",
  #               threshUsed, "', ",
  #               cutval, ", '",
  #               threshComm, "');")
  # dbExecute(cn, sql)
  # 
  dbDisconnect(cn)
  rm(cn)

  print (paste0("Export complete for ",j," of ",length(not_yet_exported)," : ",not_yet_exported[j]))
}

