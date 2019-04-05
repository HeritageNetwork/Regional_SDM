## for exporting outputs for the map review tool
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

## NOTE: enter the species CuteCode on line 30 below

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

# for scripts 1-3, run just the following 3 lines
model_species <- "obovretu"
load(here("_data","species",model_species,"runSDM_paths.Rdata"))
for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])

# if debugging script 4 or later, also load the specific model output rdata file
model_rdata <- max(list.files(here("_data","species",model_species,"outputs","rdata")))
load(here("_data","species",model_species,"outputs","rdata",paste0(model_rdata)))



rootPath <- here("_data","species",model_species)
outpath <- file.path(rootPath, "outputs","model_review_output")
dir.create(outpath, showWarnings = FALSE)
setwd(outpath)

# select the threshold type
threshold_Aqua <- "TenPctile"
threshold_Terr <- "MTPEO"

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
  rasPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,".tif"))
  ras <- raster(rasPath) 
} else {
  print("Model is not of the Terrestrial or Aquatic Type")
}

# get threshold information
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("select model_run_name, ElemCode, cutCode, cutValue
            from tblModelResultsCutoffs where model_run_name = '", 
              model_run_name, "';")
threshInfo <- dbGetQuery(db, statement = sql)

if (modType=="A"){ # Aquatic Option -
  # delete followlines that are not above the threshold
  modelLine <- shp[which(shp[[threshold_Aqua]]==1),]  
  modelLine$cutecode <- model_species
  modelLine <- modelLine[,"cutecode"] 
} else if (modType=="T"){ # Terrestrial Option - 
  # get cutvalue for MTP by group
  cutval <- threshInfo[threshInfo$cutCode == threshold_Terr,"cutValue"]
  #reclassify the raster and create feature class ----
  breaks <- c(0,cutval,1)
  rascut <- cut(ras, breaks = breaks)
  # convert raster to polys
  modelPoly <- rasterToPolygons(rascut, fun = function(x){x==2}, dissolve = TRUE)
  # add cutecode as attribute, remove all other columns
  modelPoly$cutecode <- model_species
  modelPoly <- modelPoly[,"cutecode"]
} else {
  print("Model is not of the Terrestrial or Aquatic Type")
}

#writeOGR(obj=modelPoly, dsn=outpath, layer="modelPoly", driver="ESRI Shapefile")
#inShp <- paste0(outpath, "/modelPoly.shp")

# use bridge to write out file gdb
arc.check_product()
# copy over a blank gdb as the bridge can't create them by itself. I would store this in some other directory
templateGDB <- here("_data","other_spatial","feature","template_db_predictedhabitat-poly.gdb")
templateFiles <- list.files(templateGDB)
if (modType=="A"){ # Aquatic Option -
  gdbName <- "predictedhabitat-line.gdb"
} else if (modType=="T") { # Terrestrial Option -
  gdbName <- "predictedhabitat-poly.gdb"
}
dir.create(gdbName)
file.copy(file.path(templateGDB, templateFiles), gdbName)

if (modType=="A"){ # Aquatic Option -
  # write the results
  arc.write(paste0(gdbName,"/",model_species,"_TenPctile"), modelLine)
} else if (modType=="T"){ # Terrestrial Option - 
  arc.write(paste0(gdbName,"/",model_species,"_mtpg"), modelPoly)
}  

#use_python("C:/Users/Tim/AppData/Local/Esri/conda/envs/timsarcproclone")
#use_virtualenv(Sys.getenv("PYTHONPATH"))
#use_virtualenv("C:/Users/Tim/AppData/Local/Esri/conda/envs/timsarcproclone")

#arcpy <- import("arcpy")
#gdbName <- "predictedhabitat-poly.gdb"
#arcpy$CreateFileGDB_management(outpath, gdbName)
# tried to here
#inShp <- paste0(outpath, "/modelPoly.shp")
#outFC <- paste0(outpath, "/", gdbName, "/", model_species, "_mtpg")
#arcpy$CopyFeatures_management(inShp, outFC)

#clean up
#delShp <- list.files(path = outpath, pattern = "modelPoly.*")
#file.remove(paste0(outpath,"/",delShp))

# zip it
#library(zip)

zfiles <- file.path(gdbName, list.files(gdbName))
utils::zip(paste0(gdbName,".zip"), files = zfiles)
#zip::zip(paste0(gdbName,".zip"), files = x)

#clean up
unlink(gdbName, recursive = TRUE)
              
## get range data ----
if (modType=="A"){ # Aquatic Option - Huc10s output as part of the model
  hucPath <- file.path(rootPath, "outputs","model_predictions",paste0(model_run_name,"_huc10.csv"))
  huc <- read.csv(hucPath)  # st_read(hucPath, quiet = T) 
  hucList <- huc$huc10
} else if (modType=="T"){ # Terrestrial Option - get range info from the DB (as a list of HUCs) 
  dbpath <- "N:/_TerrestrialModels/_data/databases/SDM_lookupAndTracking.sqlite"
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

