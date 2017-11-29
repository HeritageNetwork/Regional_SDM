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
library(rgdal)
library(rasterVis)
library(RSQLite)
library(xtable)

### find and load model data ----
## three lines need your attention. The one directly below (loc_scripts),
## about line 35 where you choose which Rdata file to use,
## and about line 46 where you choose which record to use

# loc_scripts <- "K:/Reg5Modeling_Project/scripts/Regional_SDM"

# source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))

# get a list of what's in the directory
# d <- dir(path = loc_RDataOut, pattern = ".Rdata",full.names=FALSE)
# d
# # which one do we want to run?
# n <- 1
# fileName <- d[[n]]
# load(paste(loc_RDataOut,fileName, sep="/"))
setwd(loc_RDataOut)
load(paste(modelrun_meta_data$model_run_name,".Rdata", sep=""))

# get background poly data for the map
referenceBoundaries <- readOGR(loc_otherSpatial, nm_refBoundaries, stringsAsFactors=FALSE) # name of state boundaries file
studyAreaExtent <- readOGR(loc_otherSpatial, nm_studyAreaExtent, stringsAsFactors=FALSE) 

r <- dir(path = loc_outRas, pattern = ".tif$",full.names=FALSE)
# r
# # which one do we want to run?
# n <- 1
fileName <- r[gsub(".tif", "", r) == model_run_name]
ras <- raster(paste(loc_outRas, fileName, sep = "/"))

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
# NOTE: use column should be populated with 1/0 for sources of data used
SQLquery <- paste("SELECT DISTINCT sp.CODE, sr.ProgramName, sr.State ",
  "FROM lkpSpecies as sp ",
  "INNER JOIN mapDataSourcesToSpp as mp ON mp.EstID=sp.EST_ID ",
  "INNER JOIN lkpDataSources as sr ON mp.DataSourcesID=sr.DataSourcesID ",
  "WHERE mp.use = 1 ",
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
SQLquery <- paste("Select ElemCode, dateTime, cutCode, cutValue, capturedEOs, capturedPolys, capturedPts ", 
                  "FROM tblCutoffs ", 
                  "WHERE modelRunName ='", model_run_name, "'; ", sep="")
sdm.thresholds <- dbGetQuery(db, statement = SQLquery)
# filter to only most recent
#uniqueTimes <- unique(sdm.thresholds$dateTime)
#mostRecent <- uniqueTimes[order(uniqueTimes, decreasing = TRUE)][[1]]
#sdm.thresholds <- sdm.thresholds[sdm.thresholds$dateTime == mostRecent,]

# get info about thresholds
SQLquery <- paste("SELECT cutCode, cutFullName, cutDescription, cutCitationShort, cutCitationFull, sortOrder ", 
                  "FROM lkpThresholdTypes ", 
                  "WHERE cutCode IN (", 
                  toString(sQuote(sdm.thresholds$cutCode)),
                  ");", sep = "")
sdm.thresh.info <- dbGetQuery(db, statement = SQLquery)

sdm.thresh.merge <- merge(sdm.thresholds, sdm.thresh.info)
#sort it
sdm.thresh.merge <- sdm.thresh.merge[order(sdm.thresh.merge$sortOrder),]
sdm.thresh.table <- sdm.thresh.merge[,c("cutFullName", "cutValue",
  "capturedEOs", "capturedPolys", "capturedPts", "cutDescription")]
names(sdm.thresh.table) <- c("Threshold", "Value", "EOs","Polys","Pts","Description")
sdm.thresh.table$EOs <- paste(round(sdm.thresh.table$EOs/numEOs*100, 1),
                                     "(",sdm.thresh.table$EOs, ")", sep="")
sdm.thresh.table$Polys <- paste(round(sdm.thresh.table$Polys/numPys*100, 1),
                              "(",sdm.thresh.table$Polys, ")", sep="")
numPts <- nrow(subset(df.full, pres == 1))
sdm.thresh.table$Pts <- paste(round(sdm.thresh.table$Pts/numPts*100, 1),
                              sep="")

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
