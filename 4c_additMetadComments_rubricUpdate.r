# File: 4c_additMetadComments_rubricUpdate.r
# Purpose: this script is for adding an extra paragraph to the metadata pdf,
# making any specific comments you have for the specific model you
# are working with.

# the intent here is for the database to store comments from different
# modeling runs if that's what you want, or give you the ability to 
# just update the comments when you update the model. 

# this script also updates the rubric table, based on information from the model run (TSS) 
# and the tracking DB
# this portion requires an ODBC connection defined by the .dsn text file.

library(RSQLite)
library(odbc)
library(DBI)

# load Rdata ----
setwd(loc_model)
setwd(paste0(model_species,"/outputs"))
load(paste0("rdata/", modelrun_meta_data$model_run_name,".Rdata"))

for(i in 1:length(modelrun_meta_data)){
  assign(names(modelrun_meta_data)[i], modelrun_meta_data[[i]])
}

## Additional Metadata Comments: get any current documentation ----
db <- dbConnect(SQLite(),dbname=nm_db_file)

SQLquery <- paste("SELECT * ",
                  " FROM tblModelResults ",
                  "WHERE model_run_name ='", model_run_name , "'; ", sep="")
dat.in.db <- dbGetQuery(db, statement = SQLquery)
newText <- fn_args$metaData_comments
#clean up newline chars, send it to the DB
newText <- gsub("\n", " ", newText)
# update row if necessary
if (dat.in.db$metadata_comments != newText) {

  SQLquery <- paste("UPDATE tblModelResults ",
                    "SET metadata_comments = '", newText, 
                    "' WHERE model_run_name = '", 
                    model_run_name, "';", sep = "")
  dbExecute(db, SQLquery)
}
dbDisconnect(db)
## update Model Evaluation and Use data (rubric table) ----
# get most recent data from the tracking DB for two of the rubric table fields that may vary
# get tracking DB data from SQL Server tables 

fn <- here("_data","databases", "hsm_tracker_connection_string_short.dsn")
cn <- dbConnect(odbc::odbc(), .connection_string = readChar(fn, file.info(fn)$size))

sql <- paste0("SELECT ModelCycle.EGT_ID, ModelCycle.model_cycle, ",
              "Workflows.locality_data_eval_rubric, Workflows.model_reviewed ",
              "FROM ModelCycle ",
              "INNER JOIN Workflows ON ModelCycle.ID = Workflows.model_cycle_ID ",
              "WHERE ModelCycle.EGT_ID= ", ElementNames$EGT_ID, ";")

evalAndReviewStatus <- dbGetQuery(cn, sql)
modelCycleData <- evalAndReviewStatus[,c("EGT_ID","model_cycle")]
# if more than one cycle, get the most recent cycle
if(nrow(evalAndReviewStatus) > 1){
  evalAndReviewStatus <- evalAndReviewStatus[order(evalAndReviewStatus$model_cycle, decreasing = TRUE),]
  evalAndReviewStatus <- evalAndReviewStatus[1,]
}

dbDisconnect(cn)
rm(cn)

## data quality ----
dqMatrix <- data.frame("dataQuality" = c(1,2,3),
                       "dqAttribute" = c("C","A","I"),
                       "dqComments" = c("Data taken from outside sources and may or may not be vetted for accuracy or weighted for spatial representation.",
                                        "Heritage Network data augmented with outside data which may or may not be vetted for accuracy or weighted for spatial representation.",
                                        "Heritage Network (and possibly outside) data are vetted for accuracy and weighted for spatial representation."))
dqUpdate <- dqMatrix[match(evalAndReviewStatus$locality_data_eval_rubric, dqMatrix$dataQuality),]
#push it up to sqlite DB
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("update lkpSpeciesRubric set spdata_dataqual = '", dqUpdate$dqAttribute, 
              "', spdata_dataqualNotes = '", dqUpdate$dqComments, 
              "' where EGT_ID = ", ElementNames$EGT_ID, " ;")
dbExecute(db, statement = sql)

## performance ----
# get performance data
sql <- paste0("SELECT * from tblModelResultsValidationStats where model_run_name = '", 
              model_run_name, "';")
vstats <- dbGetQuery(db, statement = sql)
summaryTSS <- mean(vstats[vstats$metric == "TSS", "metric_mn"])
rm(vstats)

prfmcMatrix <- data.frame("pAttribute" = c("C","A"),
                          "pComments" = c("Model TSS < 0.6. Mapped model output is evaluated for ecological plausibility by expert review.",
                                          "Model TSS >= 0.6. Mapped model output is evaluated for ecological plausibility by expert review."))
prfmAtt <- ifelse(summaryTSS<=0.6, "C", "A")
prfmUpdate <- prfmcMatrix[match(prfmAtt, prfmcMatrix$pAttribute),]
sql <- paste0("update lkpSpeciesRubric set process_perform = '", prfmUpdate$pAttribute, 
              "', process_performNotes = '", prfmUpdate$pComments, 
              "' where EGT_ID = ", ElementNames$EGT_ID, " ;")
dbExecute(db, statement = sql)

## model review ----
revMatrix <- data.frame("rAttribute" = c("C","A"),
                        "rComments" = c("Model was not reviewed by regional, taxonomic experts.",
                                        "Model was reviewed by regional, taxonomic experts."))
revAtt <- ifelse(!is.na(evalAndReviewStatus$model_reviewed) , "A", "C")
revUpdate <- revMatrix[match(revAtt, revMatrix$rAttribute),]
sql <- paste0("update lkpSpeciesRubric set process_review = '", revUpdate$rAttribute, 
              "', process_reviewNotes = '", revUpdate$rComments, 
              "' where EGT_ID = ", ElementNames$EGT_ID, " ;")
dbExecute(db, statement = sql)

## iterative ----
iterMatrix <- data.frame("iAttribute" = c("C","A"),
                          "iComments" = c("Model not re-run with new or modified data.",
                                          "Model was re-run with new or modified data."))
nCycles <- nrow(modelCycleData)
maxCycle <- max(modelCycleData$model_cycle)
if(nCycles > 1){
  # if(nCycles == 2 & "Both" %in% modelCycleData$model_type){
  #   iterAtt <- "C"
  # } else if(TRUE %in% modelCycleData[modelCycleData$model_cycle == maxCycle, c("alternate_method","existing_model")]){
  #   iterAtt <- "C"
  # } else {
    iterAtt <- "A"
  #}
} else {
  iterAtt <- "C"
}

iterUpdate <- iterMatrix[match(iterAtt, iterMatrix$iAttribute),]
sql <- paste0("update lkpSpeciesRubric set iterative = '", iterUpdate$iAttribute, 
              "', iterativeNotes = '", iterUpdate$iComments, 
              "' where EGT_ID = ", ElementNames$EGT_ID, " ;")
dbExecute(db, statement = sql)

## clean up ----
dbDisconnect(db)
