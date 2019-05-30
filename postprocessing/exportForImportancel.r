library(raster)
library(here)
library(RSQLite)
library(rgdal)
#library(reticulate)
library(RSQLite)
library(arcgisbinding)
library(sf)

# load in the data from the species run
rm(list=ls())

#Splits out character vector of cutecodes of complete models *Finished*
#Scans for models that have produced the full metadata pdf (finished script 5)
# finished <- unique(unlist(sapply(grep("2019",list.files(path=paste("N:/_AquaticModels","_data","species",sep="/"),pattern=".pdf",recursive = TRUE),value=TRUE),function(x) strsplit(x,"/")[[1]][[1]],USE.NAMES = FALSE)))
finished <- unique(unlist(sapply(grep("2019",list.files(path=paste("N:/molly/_AquaticModels_mmoore","_data","species",sep="/"),pattern=".pdf",recursive = TRUE),value=TRUE),function(x) strsplit(x,"/")[[1]][[1]],USE.NAMES = FALSE)))


finished <- finished[4:4]
#finished <- "alasvari"
for (j in 1:length(finished)){
  print (paste0("Starting Aquatic Model export for model ",j," of ",length(finished)," : ",finished[j]))
  model_species <- finished[j]

  #load(paste("N:/_AquaticModels","_data","species",model_species,"runSDM_paths.Rdata", sep="/"))
  load(paste("N:/molly/_AquaticModels_mmoore","_data","species",model_species,"runSDM_paths.Rdata", sep="/"))
  for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])
  
  # load the specific model output rdata file
  # model_rdata <- max(list.files(paste("N:/_AquaticModels","_data","species",model_species,"outputs","rdata",sep="/")))
  # load(paste("N:/_AquaticModels","_data","species",model_species,"outputs","rdata",paste0(model_rdata),sep="/"))
  model_rdata <- max(list.files(paste("N:/molly/_AquaticModels_mmoore","_data","species",model_species,"outputs","rdata",sep="/")))
  load(paste("N:/molly/_AquaticModels_mmoore","_data","species",model_species,"outputs","rdata",paste0(model_rdata),sep="/"))
  
    
  #rootPath <- paste("N:/_AquaticModels","_data","species",model_species,sep="/")
  rootPath <- paste("N:/molly/_AquaticModels_mmoore","_data","species",model_species,sep="/")
  outpath <- file.path("N:/_AquaticDataForCharlie", "May29")
  dir.create(outpath, showWarnings = FALSE)
  setwd(outpath)
  
  # select the threshold type
  threshold_Aqua <- "TenPctile"

  # # Get the model type from the sqlite
  #nm_db_file <- paste("N:/_AquaticModels","_data", "databases", "SDM_lookupAndTracking.sqlite", sep="/")
  nm_db_file <- paste("N:/molly/_AquaticModels_mmoore","_data", "databases", "SDM_lookupAndTracking.sqlite", sep="/")
  db <- dbConnect(SQLite(),dbname=nm_db_file)

  shpPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,"_results.shp"))
    shp <- st_read(shpPath, quiet = T)  

  wbPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,"_results_aquaPolys.shp"))
    wb <- st_read(wbPath, quiet = T)  
    
    
  # get threshold information
  sql <- paste0("select model_run_name, ElemCode, cutCode, cutValue, capturedPts from tblModelResultsCutoffs where model_run_name = '", model_run_name, "';")
  threshInfo <- dbGetQuery(db, statement = sql)
  
  # table storing input data seems to have different date stamp, need to use only
  # cutecode_date, instead of cutecode_date_time
  # TODO: if multiples, grab the most recent by date-time, not the largest count
  # mrn <- strsplit(model_run_name, "_")[[1]]
  # mrn <- paste(mrn[[1]], mrn[[2]], sep = "_")
  # sql <- paste0("select obs_count from tblModelInputs where table_code LIKE '",
  #               mrn, "%';")
  # inputPts <- dbGetQuery(db, statement = sql)
  # inPts <- max(inputPts$obs_count)
  # 
  # dbDisconnect(db)
  # rm(db, mrn, sql, inputPts)
  
  # delete followlines that are not above the threshold
  modelLine <- shp[which(shp[[threshold_Aqua]]==1),]  
  modelLine$cutecode <- model_species
  modelLine <- modelLine[,"cutecode"] 
  threshUsed <- threshold_Aqua
  cutval <- threshInfo[threshInfo$cutCode == threshUsed,"cutValue"]
  threshComm <- paste0("Aquatic model, using ", threshUsed, " as threshold.")
  
  
  if(nrow(wb[which(wb[[threshold_Aqua]]==1),])>0){
    modelPoly <- wb[which(wb[[threshold_Aqua]]==1),]  
    modelPoly$cutecode <- model_species
    modelPoly <-  modelPoly[,"cutecode"] 
    threshUsed <- threshold_Aqua
    cutval <- threshInfo[threshInfo$cutCode == threshUsed,"cutValue"]
    threshComm <- paste0("Aquatic model, using ", threshUsed, " as threshold.")    
  }
  
  # use bridge to write out file gdb
  arc.check_product()
 
  gdbName <- "aquatic_models.gdb"

  arc.write(paste0(gdbName,"/",model_species,"_", "Flowline"), modelLine)
  
  
  
  if(exists("modelPoly")){
    arc.write(paste0(gdbName,"/",model_species,"_", "Waterbody"), modelPoly)
  }
  

  print (paste0("Export complete for ",j," of ",length(finished)," : ",finished[j]))
}

