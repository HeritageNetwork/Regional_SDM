# File: 5_createMetadata.r
# Purpose: to summarize validation data and other information about the 
# model and write it to a pdf. This pdf should accompany ALL sharing/showing
# of the SDM map.

# For knitr to work, you need MikTex installed. See http://miktex.org/

# load libraries ----
library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(randomForest)
library(knitr)
library(raster)
library(maptools)
library(sp)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(rasterVis)
library(RSQLite)
library(xtable)

### find and load model data ----
## three lines need your attention. The one directly below (loc_scripts),
## about line 35 where you choose which Rdata file to use,
## and about line 46 where you choose which record to use

# source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))

setwd(loc_RDataOut)
load(paste(modelrun_meta_data$model_run_name,".Rdata", sep=""))

# get reach data for the map
setwd(loc_outVector)

results_shape <- readOGR(loc_outVector, paste0(modelrun_meta_data$model_run_name, "_results")) # shapefile results for mapping

# get background poly data for the map
setwd(loc_otherSpatial)
studyAreaExtent <- readOGR(loc_otherSpatial,  nm_studyAreaExtent) # study area
referenceBoundaries <- readOGR(loc_otherSpatial, nm_refBoundaries) # name of state boundaries file
if (!is.null(nm_aquaArea)) aquaPolys <- readOGR(loc_otherSpatial, nm_aquaArea) # aquatic area features (lakes, large rivers, etc.)

## Get Program and Data Sources info ----
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)  
SQLquery <- paste("Select lkpModelers.ProgramName, lkpModelers.FullOrganizationName, ",
                  "lkpModelers.City, lkpModelers.State, lkpSpecies.CODE ",
                  "FROM lkpModelers ", 
                  "INNER JOIN lkpSpecies ON lkpModelers.ModelerID=lkpSpecies.ModelerID ", 
                  "WHERE lkpSpecies.CODE='", ElementNames$Code, "'; ", sep="")
sdm.modeler <- dbGetQuery(db, statement = SQLquery)
# NOTE: VA group changed here to only select VA sources
SQLquery <- paste("SELECT sp.CODE, sr.ProgramName, sr.State ",
                  "FROM lkpSpecies as sp ",
                  "INNER JOIN mapDataSourcesToSpp as mp ON mp.EstID=sp.EST_ID ",
                  "INNER JOIN lkpDataSources as sr ON mp.DataSourcesID=sr.DataSourcesID ",
                  "WHERE sr.State = 'VA' ",
                  "AND sp.CODE='", ElementNames$Code, "'; ", sep="")
sdm.dataSources <- dbGetQuery(db, statement = SQLquery)
sdm.dataSources <- sdm.dataSources[order(sdm.dataSources$ProgramName),]

SQLquery <- paste("SELECT ID, date, speciesCode, comments",
                  " FROM tblCustomModelComments ", 
                  "WHERE modelRunName ='", model_run_name, "'; ", sep="")
sdm.customComments <- dbGetQuery(db, statement = SQLquery)
# assume you want the most recently entered comments, if there are multiple entries
if(nrow(sdm.customComments) > 1) {
  sdm.customComments <- sdm.customComments[order(sdm.customComments$date, decreasing = TRUE),]
  sdm.customComments.subset <- sdm.customComments[1,]
} else {
  sdm.customComments.subset <- sdm.customComments
}

## Get threshold information ----
SQLquery <- paste("Select ElemCode, dateTime, cutCode, cutValue, capturedPts ", 
                  "FROM tblCutoffs ", 
                  "WHERE modelRunName ='", model_run_name, "'; ", sep="")
sdm.thresholds <- dbGetQuery(db, statement = SQLquery)
# filter to only most recent
#uniqueTimes <- unique(sdm.thresholds$dateTime)
#mostRecent <- uniqueTimes[order(uniqueTimes, decreasing = TRUE)][[1]]
#sdm.thresholds <- sdm.thresholds[sdm.thresholds$dateTime == mostRecent,]

# get info about thresholds
SQLquery <- paste("SELECT cutCode, cutFullName, cutDescription, cutCitationShort, cutCitationFull ", 
                  "FROM lkpThresholdTypes ", 
                  "WHERE cutCode IN (", 
                  toString(sQuote(sdm.thresholds$cutCode)),
                  ");", sep = "")
sdm.thresh.info <- dbGetQuery(db, statement = SQLquery)

sdm.thresh.merge <- merge(sdm.thresholds, sdm.thresh.info)

sdm.thresh.table <- sdm.thresh.merge[,c("cutFullName", "cutValue",
                                        "capturedPts", "cutDescription")]
names(sdm.thresh.table) <- c("Threshold", "Value", "Pct","Description")
#sdm.thresh.table$EOs <- paste(round(sdm.thresh.table$EOs/numEOs*100, 1),
#                                    "(",sdm.thresh.table$EOs, ")", sep="")
#sdm.thresh.table$Polys <- paste(round(sdm.thresh.table$Polys/numPys*100, 1),
#                         "(",sdm.thresh.table$Polys, ")", sep="")
numPts <- nrow(subset(df.full, pres == 1))
sdm.thresh.table$Pct <- paste(round(sdm.thresh.table$Pct/numPts*100, 1),
                              sep="")


# Get env. var lookup table
var_names <- names(df.full)[7:length(names(df.full))]
SQLquery <- paste("SELECT fullName, category, description ",
                  "FROM lkpEnvVarsAqua ",
                  "WHERE fullName IN (",
                  toString(sQuote(var_names)),
                  ") ORDER BY fullName;", sep = "")
sdm.var.info <- dbGetQuery(db, statement = SQLquery)
names(sdm.var.info) <- c("Variable Name", "Source", "Variable Description")
## Run knitr and create metadata ----

# writing to the same folder as a grid might cause problems.
# if errors check that first
#   more explanation: tex looks for and uses aux files, which are also used
#   by esri. If there's a non-tex aux file, knitr bails. 

# Also, might need to run this twice. First time through tex builds the reference
# list, second time through it can then number the refs in the doc.

setwd(loc_outMetadata)

knit2pdf(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))

## clean up ----
options(op)
dbDisconnect(db)