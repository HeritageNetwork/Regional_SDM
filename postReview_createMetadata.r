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
library(sf)
library(RColorBrewer)
library(rasterVis)
library(RSQLite)
library(xtable)
library(stringi)
library(tables)

library(tmap)
library(tmaptools)
library(OpenStreetMap)


### find and load model data ----
## three lines need your attention. The one directly below (loc_scripts),
## about line 35 where you choose which Rdata file to use,
## and about line 46 where you choose which record to use

### temp debugging
loc_model <- "G:/tim/_Regional_SDM/_data/species"
loc_envVars <- gsub("F:","G:",loc_envVars)
loc_scripts <- "G:/tim/_Regional_SDM"
nm_refBoundaries <- gsub("F:","G:",nm_refBoundaries)
nm_db_file <- gsub("F:","G:",nm_db_file)
####

setwd(loc_model)
dir.create(paste0(model_species,"/outputs/metadata"), recursive = T, showWarnings = F)
setwd(paste0(model_species,"/outputs"))
load(paste0("rdata/", modelrun_meta_data$model_run_name,".Rdata"))

# get background poly data for the map (study area, reference boundaries)
studyAreaExtent <- st_read(here("_data","species",model_species,"inputs","model_input",paste0(model_run_name, "_studyArea.gpkg")), quiet = T)
referenceBoundaries <- st_read(nm_refBoundaries, quiet = T) # name of state boundaries file

r <- dir(path = "model_predictions", pattern = ".tif$",full.names=FALSE)
fileName <- r[gsub(".tif", "", r) == model_run_name]
ras <- raster(paste0("model_predictions/", fileName))

# project to match raster, just in case
studyAreaExtent <- st_transform(studyAreaExtent, as.character(ras@crs))
referenceBoundaries <- st_transform(referenceBoundaries, as.character(ras@crs))

## Get Program and Data Sources info ----
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)  
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
# assume you want the most recently entered comments, if there are multiple entries
if(nrow(sdm.customComments) > 1) {
  sdm.customComments <- sdm.customComments[order(sdm.customComments$date, decreasing = TRUE),]
  sdm.customComments.subset <- sdm.customComments[1,]
} else {
  sdm.customComments.subset <- sdm.customComments
}

## Get threshold information ----
SQLquery <- paste("Select ElemCode, dateTime, cutCode, cutValue, capturedEOs, capturedPts ", 
                  "FROM tblModelResultsCutoffs ", 
                  "WHERE model_run_name ='", model_run_name, "'; ", sep="")
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
  "capturedEOs", "capturedPts", "cutDescription")]
names(sdm.thresh.table) <- c("Threshold", "Value", "Groups","Pts","Description")
sdm.thresh.table$Groups <- paste(round(sdm.thresh.table$Groups/numEOs*100, 1),
                                     "(",sdm.thresh.table$Groups, ")", sep="")
# sdm.thresh.table$Polys <- paste(round(sdm.thresh.table$Polys/numPys*100, 1),
#                               "(",sdm.thresh.table$Polys, ")", sep="")
numPts <- nrow(subset(df.full, pres == 1))
sdm.thresh.table$Pts <- paste(round(sdm.thresh.table$Pts/numPts*100, 1),
                              sep="")

## get grank definition ----
SQLquery <- paste0("SELECT rank, rankname FROM lkpRankDefinitions where rank = '",ElementNames$rounded_g_rank,"';", sep="")
grank_desc <- dbGetQuery(db, SQLquery)

# make a url to NatureServe Explorer
NSurl <- paste("http://explorer.natureserve.org/servlet/NatureServe?searchName=",gsub(" ", "+", ElementNames[[1]], fixed=TRUE), sep="")

## get Model Evaluation and Use data ----

## Get env. var lookup table ----
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


# model review data from Tracking DB
fn <- here("_data","databases", "mobi_tracker_connection_string_short.dsn")
cn <- dbConnect(odbc::odbc(), .connection_string = readChar(fn, file.info(fn)$size))

sql <- paste0("SELECT UserID, rating, comment
              FROM ModelReviewToolOverallFeedback
              WHERE (((ModelReviewToolOverallFeedback.cutecode)= '", ElementNames$Code, "' ));")
reviewData <- dbGetQuery(cn, sql)

# get info about revisions:
  # if on a cycle greater than 1 (but not a 'both' species with a cycle of 2), then count as revised
  # if HUCs are getting been removed, count as revised
dbDisconnect(cn)
rm(cn)

numReviewers <- nrow(reviewData)
if(numReviewers == 0){
  numReviewersPhrase = ""
  meanRating <- "-"
  minRating <- "-"
  maxRating <- "-"
  medianRating <- "-"
} else if (numReviewers == 1){
  numReviewersPhrase <- paste0(" (",numReviewers," reviewer)")
  meanRating <- reviewData$rating
  minRating <- "-"
  maxRating <- "-"
  medianRating <- "-"
  revMatrix <- data.frame(
    "rAttribute" = c("C", "Cr","A","I"),
    "rComments" = c(
      "Model was not reviewed by regional, taxonomic experts.",
      paste0("Model review indicates possible issues with this model.",numReviewersPhrase), 
      paste0("Model was reviewed by a regional, taxonomic expert.",numReviewersPhrase),
      paste0("Model reviewed by a regional, taxonomic expert and given high marks.",numReviewersPhrase)
    ))
} else {
  numReviewersPhrase <- paste0(" (",numReviewers," reviewers)")
  meanRating <- mean(reviewData$rating)
  minRating <- min(reviewData$rating)
  maxRating <- max(reviewData$rating)
  medianRating <- median(reviewData$rating)
  revMatrix <- data.frame(
    "rAttribute" = c("C", "Cr","A","I"),
    "rComments" = c(
      "Model was not reviewed by regional, taxonomic experts.",
      paste0("Model review indicates possible issues with this model.",numReviewersPhrase), 
      paste0("Model was reviewed by regional, taxonomic experts.",numReviewersPhrase),
      paste0("Model reviewed by regional, taxonomic experts and given high marks.",numReviewersPhrase)
    ))
}

revAtt <- ifelse(nrow(reviewData) == 0 , "C", 
                 ifelse(meanRating < 2.5, "Cr",
                        ifelse(meanRating < 3.5, "A", "I")))
revUpdate <- revMatrix[match(revAtt, revMatrix$rAttribute),]

## get Model Evaluation and Use data ----
SQLquery <- paste("Select spdata_dataqual, spdata_abs, spdata_eval, envvar_relevance, envvar_align, process_algo, process_sens, process_rigor, process_perform, process_review, products_mapped, products_support, products_repo, interative, spdata_dataqualNotes, spdata_absNotes, spdata_evalNotes, envvar_relevanceNotes, envvar_alignNotes, process_algoNotes, process_sensNotes, process_rigorNotes, process_performNotes, process_reviewNotes, products_mappedNotes, products_supportNotes, products_repoNotes, interativeNotes ", 
                  "FROM lkpSpeciesRubric ", 
                  "WHERE sp_code ='", model_species, "'; ", sep="")
sdm.modeluse <- dbGetQuery(db, statement = SQLquery)

sdm.modeluse$process_review <- as.character(revUpdate$rAttribute)
sdm.modeluse$process_reviewNotes <- as.character(revUpdate$rComments)


sdm.modeluse[is.na(sdm.modeluse)] <- " "
sdm.modeluse[sdm.modeluse=="I"] <- "\\cellcolor[HTML]{9AFF99} Ideal"
sdm.modeluse[sdm.modeluse=="A"] <- "\\cellcolor[HTML]{FFFFC7} Acceptable"
sdm.modeluse[sdm.modeluse=="C"] <- "\\cellcolor[HTML]{FD6864} Interpret with Caution"

# fix greater than and less than symbol in rubric table
sdm.modeluse$process_performNotes <- gsub(">=","$\\\\geq$", sdm.modeluse$process_performNotes)
sdm.modeluse$process_performNotes <- gsub("<","$<$ ", sdm.modeluse$process_performNotes)

## Run knitr and create metadata ----

# writing to the same folder as a grid might cause problems.
# if errors check that first
#   more explanation: tex looks for and uses aux files, which are also used
#   by esri. If there's a non-tex aux file, knitr bails. 

# Also, might need to run this twice. First time through tex builds the reference
# list, second time through it can then number the refs in the doc.

setwd("metadata")
# knit2pdf errors for some reason...just knit then call directly
#knit(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
#knit2pdf(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
knit2pdf(paste(loc_scripts,"postReview_MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
#call <- paste0("pdflatex -interaction=nonstopmode ",model_run_name , ".tex")
# call <- paste0("pdflatex -halt-on-error -interaction=nonstopmode ",model_run_name , ".tex") # this stops execution if there is an error. Not really necessary
#system(call)
#system(call) # 2nd run to apply citation numbers


# delete .txt, .log etc if pdf is created successfully.
fn_ext <- c(".log",".aux",".out")
if (file.exists(paste(model_run_name, ".pdf",sep=""))){
  #setInternet2(TRUE)
  #download.file(fileURL ,destfile,method="auto")
  for(i in 1:NROW(fn_ext)){
    fn <- paste(model_run_name, fn_ext[i],sep="")
    if (file.exists(fn)){ 
      file.remove(fn)
    }
  }
}


## clean up ----
dbDisconnect(db)
options(op)
