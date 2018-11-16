# File: 4_predictModelToStudyArea.r
# Purpose: create the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(randomForest)
library(RSQLite)
library(sf)
removeTmpFiles(48) # clean old (>2days) Raster temporary files

####
## two lines need your attention. The one directly below (loc_scripts)
## and about line 26 where you choose which Rdata file to use,

# get paths, other settings
# get the customized version of the predict function
# source(paste(loc_scripts, "helper/RasterPredictMod.R", sep = "/"), local = TRUE)

# load data ----
# get the rdata file
setwd(loc_model)
dir.create(paste0(model_species,"/outputs/model_predictions"), recursive = T, showWarnings = F)
setwd(paste0(model_species,"/outputs"))

# load rdata
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

##Make the raster stack
stackOrder <- names(df.full)[indVarCols]
setwd(loc_envVars)

# find matching var rasters (with folder for temporal vars)
raslist <- list.files(pattern = ".tif$", recursive = TRUE)

fullL <- list()

# attach file names to env var names
for (i in 1:length(stackOrder)) {
  rs <- raslist[grep(paste0(stackOrder[i],".tif"), raslist, ignore.case = TRUE)]
  if (length(rs) > 1) {
    # always take most recent temporal raster
    rs1 <- do.call(rbind.data.frame, strsplit(rs, "_|/"))
    rs1$nm <- rs
    rs <- rs1$nm[which.max(as.numeric(rs1[,2]))]
  }
  fullL[[i]] <- rs
}
names(fullL) <- stackOrder
rm(rs,rs1)

envStack <- stack(fullL)

# get range info from the DB (as a list of HUCs)
db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLquery <- paste0("SELECT huc10_id from lkpRange 
                   inner join lkpSpecies on lkpRange.EGT_ID = lkpSpecies.EGT_ID
                   where lkpSpecies.sp_code = '", model_species, "';")
hucList <- dbGetQuery(db, statement = SQLquery)$huc10_id
dbDisconnect(db)
rm(db)

# now get that info spatially
nm_range <- nm_HUC_file
qry <- paste("SELECT * from HUC10 where HUC10 IN ('", paste(hucList, collapse = "', '"), "')", sep = "")
hucRange <- st_read(nm_range, query = qry)

# run prediction ----
setwd(paste0(loc_model, "/", model_species,"/outputs"))
fileNm <- paste0("model_predictions/", model_run_name,".tif")

# use parallel processing if packages installed
if (all(c("snow","parallel") %in% installed.packages())) {
  try({
    beginCluster(type = "SOCK")
    outRas <- clusterR(envStack, predict, args = list(model=rf.full, type = "prob", index = 2), verbose = T)
    writeRaster(outRas, filename = fileNm, format = "GTiff", overwrite = TRUE)
  })
  try(endCluster())
  if (!exists("outRas")) {
    cat("Cluster processing failed. Falling back to single-core processing...\n")
    outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2,
                      filename=fileNm, format = "GTiff", overwrite=TRUE)
  }
# otherwise regular processing
} else {
  cat("Using single-core processing for prediction.\nInstall package 'snow' for faster cluster (multi-core) processing.\n")
  outRas <- predict(object=envStack, model=rf.full, type = "prob", index=2,
                    filename=fileNm, format = "GTiff", overwrite=TRUE)

}
