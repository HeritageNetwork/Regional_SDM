# File: 4c_additionalMetadataComments.r
# Purpose: this script is for adding an extra paragraph to the metadata pdf,
# making any specific comments you have for the specific model you
# are working with.

# the intent here is for the database to store comments from different
# modeling runs if that's what you want, or give you the ability to 
# just update the comments when you update the model. 

library(RSQLite)

### set paths, load Rdata file ----
## three lines need your attention. The one directly below (loc_scripts),
## about line 24 where you choose which Rdata file to use,
## and about line 45 where you choose which record to use

# load Rdata
setwd(loc_modelOut)
load(paste0("rdata/", modelrun_meta_data$model_run_name,".Rdata"))

for(i in 1:length(modelrun_meta_data))
  assign(names(modelrun_meta_data)[i], modelrun_meta_data[[i]])

## get any current documentation ----
db <- dbConnect(SQLite(),dbname=nm_db_file)

SQLquery <- paste("SELECT * ",
                  " FROM tblCustomModelComments ",
                  "WHERE modelRunName ='", model_run_name , "'; ", sep="")
dat.in.db <- dbGetQuery(db, statement = SQLquery)

#view what you've got
# dat.in.db

## edit current information ----
# if you have existing record(s) and you just want to modify one and use in 
# for your current model, get a copy of the text with these calls

newText <- metaData_comments
#clean up newline chars, send it to the DB
newText <- gsub("\n", " ", newText)

# if row exists, update it - if not, insert it
if (length(dat.in.db[,1]) == 1) {

  SQLquery <- paste("UPDATE tblCustomModelComments ",
                    "SET comments = '", newText, 
                    "' , date = '", as.character(Sys.Date()),
                    "' WHERE modelRunName = '", 
                    model_run_name, "';", sep = "")
  dbExecute(db, SQLquery)
} else {
  
  ## create a new row instead ----
  newVals <- paste(as.character(Sys.Date()), ElementNames$Code, newText, model_run_name, sep = "','")
  #clean up newline chars, send it to the DB
  SQLquery <- paste("INSERT INTO tblCustomModelComments ",
                    "(date, speciesCode, comments, modelRunName) ",
                    "VALUES ('", newVals ,
                    "');", sep = "")
                    
  dbExecute(db, SQLquery)
}

# update tblModelRuns with finish time
SQLquery <- paste0("UPDATE tblModelRuns SET modelEndTime = '",as.character(Sys.time()),
                   "' WHERE modelRunName ='", model_run_name , "'; ")
dbExecute(db, SQLquery)

## clean up ----
dbDisconnect(db)