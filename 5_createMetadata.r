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
library(RODBC)

library(tmap)
library(tmaptools)
library(OpenStreetMap)


### find and load model data ----
## three lines need your attention. The one directly below (loc_scripts),
## about line 35 where you choose which Rdata file to use,
## and about line 46 where you choose which record to use

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

## update Model Evaluation and Use data ----
# get most recent data from the tracking DB for two of the rubric table fields that may vary
cn <- odbcConnect("mobi_spp_tracking")
sql <- paste0("SELECT FinalSppList.ELEMENT_GLOBAL_ID, LocalityData.pres_dat_eval_rubric, 
    LocalityData.bison_use, LocalityData.gbif_use, LocalityData.inat_use, 
    LocalityData.other_use, LocalityData.MJD_sufficient, LocalityData.MJD_only, LocalityData.status
  FROM FinalSppList INNER JOIN LocalityData ON FinalSppList.ELEMENT_GLOBAL_ID = LocalityData.EGT_ID
  WHERE (((FinalSppList.ELEMENT_GLOBAL_ID)= ", ElementNames$EGT_ID, "));")
localityData <- sqlQuery(cn, sql)

sql <- paste0("SELECT Reviewer.EGT_ID, Reviewer.response, Reviewer.date_completed
              FROM Reviewer
              WHERE (((Reviewer.EGT_ID)= ", ElementNames$EGT_ID, " ));")
reviewerData <- sqlQuery(cn, sql)
close(cn)
rm(cn)

if(localityData$status == "complete"){
  # if complete, take scoring assessment from DB
  #1 = caution, 2 = acceptible, 3 = ideal
  dataQuality <- localityData$pres_dat_eval_rubric
} else if(sum(localityData[,c("bison_use","gbif_use","inat_use","other_use")]) > 0) {
  if(localityData$MJD_only == 1){
      #this really means 'mjd_use' in the DB, not 'mjd_only'
      # if any of the BIG data are tagged for use as well as the MJD data, tag it acceptible
      dataQuality <- 2
    } else {
      # if no MJD data, tag as caution
      dataQuality <- 1
  }
} else if(!sum(localityData[,c("bison_use","gbif_use","inat_use","other_use")]) > 0) {
  if(localityData$MJD_only == 1){ 
    #this really means 'mjd_use' in the DB, not 'mjd_only'
    # if only MJD data tagged to use
    dataQuality <- 3
  } else {
    #if nothing is tagged in DB, assume a mix of records
    dataQuality <- 2
  }
}
dqMatrix <- data.frame("dataQuality" = c(1,2,3),
              "dqAttribute" = c("C","A","I"),
              "dqComments" = c("Data taken from outside sources and may or may not be vetted for accuracy or weighted for spatial representation.",
                               "Heritage Network data augmented with outside data which may or may not be vetted for accuracy or weighted for spatial representation.",
                               "Heritage Network data are vetted for accuracy and weighted for spatial representation."))
dqUpdate <- dqMatrix[match(dataQuality, dqMatrix$dataQuality),]
#push it up to DB
sql <- paste0("update lkpSpeciesRubric set spdata_dataqual = '", dqUpdate$dqAttribute, 
              "', spdata_dataqualNotes = '", dqUpdate$dqComments, 
              "' where EGT_ID = ", ElementNames$EGT_ID, " ;")
dbExecute(db, statement = sql)
## performance
prfmcMatrix <- data.frame("pAttribute" = c("C","A"),
                          "pComments" = c("Model TSS < 0.6. Mapped model output is evaluated for ecological plausibility by expert review.",
                                          "Model TSS >= 0.6. Mapped model output is evaluated for ecological plausibility by expert review."))
prfmAtt <- ifelse(tss.summ$mean<=0.6, "C", "A")
prfmUpdate <- prfmcMatrix[match(prfmAtt, prfmcMatrix$pAttribute),]
sql <- paste0("update lkpSpeciesRubric set process_perform = '", prfmUpdate$pAttribute, 
              "', process_performNotes = '", prfmUpdate$pComments, 
              "' where EGT_ID = ", ElementNames$EGT_ID, " ;")
dbExecute(db, statement = sql)
## model review
modRev <- sum(reviewerData$response)
revMatrix <- data.frame("rAttribute" = c("C","A"),
                        "rComments" = c("Model was not reviewed by regional, taxonomic experts.",
                                        "Model was reviewed by regional, taxonomic experts."))
revAtt <- ifelse(sum(reviewerData$response) > 0 , "A", "C")
revUpdate <- revMatrix[match(revAtt, revMatrix$rAttribute),]
sql <- paste0("update lkpSpeciesRubric set process_review = '", revUpdate$rAttribute, 
              "', process_reviewNotes = '", revUpdate$rComments, 
              "' where EGT_ID = ", ElementNames$EGT_ID, " ;")
dbExecute(db, statement = sql)


## get Model Evaluation and Use data ----
SQLquery <- paste("Select spdata_dataqual, spdata_abs, spdata_eval, envvar_relevance, envvar_align, process_algo, process_sens, process_rigor, process_perform, process_review, products_mapped, products_support, products_repo, interative, spdata_dataqualNotes, spdata_absNotes, spdata_evalNotes, envvar_relevanceNotes, envvar_alignNotes, process_algoNotes, process_sensNotes, process_rigorNotes, process_performNotes, process_reviewNotes, products_mappedNotes, products_supportNotes, products_repoNotes, interativeNotes ", 
                  "FROM lkpSpeciesRubric ", 
                  "WHERE sp_code ='", model_species, "'; ", sep="")
sdm.modeluse <- dbGetQuery(db, statement = SQLquery)
sdm.modeluse[is.na(sdm.modeluse)] <- " "
sdm.modeluse[sdm.modeluse=="I"] <- "\\cellcolor[HTML]{9AFF99} Ideal"
sdm.modeluse[sdm.modeluse=="A"] <- "\\cellcolor[HTML]{FFFFC7} Acceptable"
sdm.modeluse[sdm.modeluse=="C"] <- "\\cellcolor[HTML]{FD6864} Interpet with Caution"

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
knit2pdf(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
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
