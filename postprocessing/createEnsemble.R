# originally a copy of 4b_threshold, therefore need to load data and settings for up to that point
# Purpose: using a threshold determined independantly (with expert review),
# create an ensemble raster

# assumes 4b has already been run: threshold data are downloaded from DB

### find and load model data ----
setwd(loc_model)
setwd(paste0(model_species,"/outputs"))
#load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))


db <- dbConnect(SQLite(),dbname=nm_db_file)

# first clear any results if this run has already been written to the db
sql <- paste0("SELECT * from tblModelResultsCutoffs WHERE model_run_name = '", 
              modelrun_meta_data$model_run_name, 
              "';")
threshDat <- dbGetQuery(db, statement = sql)
# clean up
dbDisconnect(db)

# for now, apply one thresh among all algorithms
# options: "eqSS", "FMeasPt01", "maxSSS", "MPVG", "MPVP", "MTP", "MTPGP", "ROC", "TenPctile"

oneThresh <- "maxSSS"

threshDat <- threshDat[threshDat$cutCode == oneThresh,]


pth <- file.path(loc_model, model_species,"outputs","model_predictions")

# reclassify the rasters based on the threshold into binary 0/1
for(algo in ensemble_algos){
  fileNm <- paste0(model_run_name,"_",algo,".tif") 
  ras <- raster(file.path(pth, fileNm))
  cutval <- threshDat[threshDat$algorithm == algo, "cutValue"]  
  m <- cbind(
    from = c(-Inf, cutval),
    to = c(cutval, Inf),
    becomes = c(0, 1)
  )
  rasrc <- reclassify(ras, m)
  outFileNm <- file.path(pth, paste0(model_run_name,"_",algo,"_",oneThresh, ".tif"))
  writeRaster(rasrc, filename=outFileNm, format="GTiff", overwrite=TRUE, datatype = "INT2U")
}

# now merge the reclassified rasters 
binStackPths <- vector("list",length(ensemble_algos))
names(binStackPths) <- ensemble_algos
# make a stack
for(algo in ensemble_algos){
  fileNm <- paste0(model_run_name,"_",algo,"_",oneThresh, ".tif")
  binStackPths[algo] <- file.path(pth, fileNm)
}

predStack <- stack(binStackPths)

#calculate the sum
fileNm <- paste0(model_run_name,"_sum_",oneThresh,".tif")
outName <- file.path(pth, fileNm)
sumOut <- calc(predStack, fun=sum, filename = outName)

