# File: 4c_additionalMetadataComments.r
# Purpose: this script is for adding an extra paragraph to the metadata pdf,
# making any specific comments you have for the specific model you
# are working with.

# the intent here is for the database to store comments from different
# modeling runs if that's what you want, or give you the ability to 
# just update the comments when you update the model. 

library(RSQLite)

# load Rdata
setwd(loc_model)
setwd(paste0(model_species,"/outputs"))
load(paste0("rdata/", modelrun_meta_data$model_run_name,".Rdata"))

for(i in 1:length(modelrun_meta_data))
  assign(names(modelrun_meta_data)[i], modelrun_meta_data[[i]])

## get any current documentation ----
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

## clean up ----
dbDisconnect(db)
