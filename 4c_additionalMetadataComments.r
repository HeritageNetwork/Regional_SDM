##
# this script is for adding an extra paragraph to the metadata pdf,
# making any specific comments you have for the specific model you
# are working with.

# the intent here is for the database to store comments from different
# modeling runs if that's what you want, or give you the ability to 
# just update the comments when you update the model. 

library(RSQLite)

## set paths, load Rdata file ----
inPath <- "K:/Reg5Modeling_Project/outputs"

# get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
# which one do we want to load?
n <- 1
fileName <- d[[n]]
load(paste(inPath,fileName, sep="/"))

dbLoc <- "K:/Reg5Modeling_Project/databases"

## get any current documentation ----
db_file <- paste(dbLoc, "SDM_lookupAndTracking.sqlite", sep = "/")
db <- dbConnect(SQLite(),dbname=db_file)  

SQLquery <- paste("SELECT ID, date, speciesCode, comments",
                  " FROM tblCustomModelComments ", 
                  "WHERE speciesCode='", ElementNames$Code, "'; ", sep="")
dat.in.db <- dbGetQuery(db, statement = SQLquery)

#view what you've got
dat.in.db


## edit current information ----
# if you have existing record(s) and you just want to modify one and use in 
# for your current model, get a copy of the text with these calls

# if you have multiple rows, which row to you want? Set the ID you want to use, 
# the ID should be visible in the dat.in.db view, above
idVal <- 1
cat(dat.in.db$comments[dat.in.db$ID == idVal])

# copy and paste it into here and edit as needed. 

newText <- "This is the spot where you can write a 
bunch of text describing what you want to describe about
this particular model or this particular species. It will 
be added as a paragraph to the metadata pdf."

#clean up newline chars, send it to the DB
newText <- gsub("\n", " ", newText)
SQLquery <- paste("UPDATE tblCustomModelComments ",
                  "SET comments = '", newText, 
                  "' , date = '", Sys.time(), 
                  "' WHERE ID = ", 
                  idVal, ";", sep = "")
dbExecute(db, SQLquery)


## create a new row instead ----
myText <- "This is the spot where you can write a 
bunch of text describing what you want to describe about
this particular model or this particular species. It will 
be added as a paragraph to the metadata pdf."

#clean up newline chars, send it to the DB
myText <- gsub("\n", " ", newText)
SQLquery <- paste("INSERT INTO tblCustomModelComments ",
                  "(date, speciesCode, comments) ",
                  "VALUES ('",
                  Sys.time(),"', '", 
                  ElementNames$Code, "', '", 
                  newText,
                  "');", sep = "")
                  
dbExecute(db, SQLquery)

## clean up ----
dbDisconnect(db)
# remove all objects before moving on to the next script
rm(list=ls())
