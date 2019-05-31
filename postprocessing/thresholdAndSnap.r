## for making thresholded tiffs for Charlie for already completed models


## this is a temporary work around for use untill we get the export polygon and raster
## export cleaned up in the primary scripts. 

## this is simply a modified version of the "exportForMapReviewTool.r" script.
##  You need to set paths on lines 17, 18, 43, 46


library(RSQLite)
library(reticulate) # for direct python calls (alternative to arcgisbinding)
#library(stars)
# load in the data from the species run
rm(list=ls())

basePath <- "N:/_TerrestrialModels"
pathToWriteTo <- "E:/thresholdedModels"

#####Scan the directories to detect species that have not been packaged for upload and package them all#####

#Splits out character vector of cutecodes for completed models *Finished*
#Scans for models that have produced the full metadata pdf (finished script 5)
finished <- unique(unlist(sapply(grep("2019",list.files(path=file.path(basePath, "_data","species"),pattern=".pdf",recursive = TRUE),value=TRUE),function(x) strsplit(x,"/")[[1]][[1]],USE.NAMES = FALSE)))

# check for species already exported
alreadyExported <-  list.files(path = pathToWriteTo)
alreadyExported <- unique(unlist(lapply(alreadyExported, function(y) strsplit(y,"_")[[1]][[1]])))

not_yet_exported <- setdiff(finished,alreadyExported)

##If there are models in the list you do not want to export, remove them by adding their cutecodes to "exclude_these"
exclude_these=c("_done") #Delete codes in this vector when they are ready to be packaged

not_yet_exported <- not_yet_exported[!not_yet_exported %in% exclude_these]
not_yet_exported ##These are the final set of models that will be packaged up
length(not_yet_exported)

####For manual use- to run one at a time UNCOMMENT this line and add cutecode(s) of the species interested ####
#not_yet_exported <- c("agatstep")

### This needs resetting to your ArcPro python cloned environment ###
use_python("C:/Users/Tim/AppData/Local/Esri/conda/envs/timsarcproclone")
arcpy <- import("arcpy")

arcpy$env$snapRaster = "N:/tim/snapRas/SnapRaster_reclass.tif"
arcpy$env$pyramid = "NONE"
arcpy$env$overwriteOutput = "True"

####Run the packaging tool for all models in your vector####
for (j in 2:length(not_yet_exported)){
  print (paste0("Starting threshold export for model ",j," of ",length(not_yet_exported)," : ",not_yet_exported[j]))
  model_species <- not_yet_exported[j]
  
  load(file.path(basePath, "_data","species",model_species,"runSDM_paths.Rdata"))
  for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])
  
  # load the specific model output rdata file
  model_rdata <- max(list.files(file.path(basePath, "_data","species",model_species,"outputs","rdata")))
  load(file.path(basePath, "_data","species",model_species,"outputs","rdata",paste0(model_rdata)))
  
  rootPath <- file.path(basePath, "_data","species", model_species)
  outpath <- file.path(rootPath, "outputs","model_predictions")
  arcpy$env$workspace = outpath
  
  # select the threshold type
  threshold_Terr_a <- "MTPEO"
  threshold_Terr_b <- "TenPctile"
  
  # get threshold information
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("select model_run_name, ElemCode, cutCode, cutValue, capturedPts 
                from tblModelResultsCutoffs where model_run_name = '", 
                model_run_name, "';")
  threshInfo <- dbGetQuery(db, statement = sql)
  
  # table storing input data seems to have different date stamp, need to use only
  # cutecode_date, instead of cutecode_date_time
  # TODO: if multiples, grab the most recent by date-time, not the largest count
  mrn <- strsplit(model_run_name, "_")[[1]]
  mrn <- paste(mrn[[1]], mrn[[2]], sep = "_")
  sql <- paste0("select obs_count from tblModelInputs where table_code LIKE '",
                  mrn, "%';")
  inputPts <- dbGetQuery(db, statement = sql)
  inPts <- max(inputPts$obs_count)
  
  dbDisconnect(db)
  rm(db, mrn, sql, inputPts)
  
  # proportion of points captured by MTP by group
  propPts <- threshInfo[threshInfo$cutCode == "MTPEO","capturedPts"]/inPts
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

  rasPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,".tif"))
  outName <- file.path(outpath, paste0(model_run_name, "_", threshUsed, ".tif"))
  snapOutName <- file.path(outpath, paste0(model_run_name, "_snapped.tif"))

  arcpy$Resample_management(rasPath, snapOutName, 30, "NEAREST")
  remap <- paste0("0 ",cutval," NoData;", cutval," 1 1")
  arcpy$Reclassify_3d(snapOutName, "VALUE", remap, outName, "NODATA")
  
  copyPath <- file.path(pathToWriteTo, paste0(model_run_name, "_", threshUsed, ".tif"))
  
  arcpy$CopyRaster_management(outName, copyPath)
  
  print (paste0("Raster chopped ",j," of ",length(not_yet_exported)," : ",not_yet_exported[j]))
}

