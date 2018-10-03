# File: preproc_getCleanSqliteDB.R
# Purpose: Use this code only to 
# create a new, clean version of the 
# SQLite db. 

# The core schema and data in this DB are 
# stored here (in git) as text so that version 
# tracking can work properly with it. Only when 
# we have significant schema changes should we 
# expect to have to apply those changes to this
# version. 

# this code requires command-line calls that use
# an sqlite tool. Download that tool here:
# http://www.sqlite.org/download.html
# you want "sqlite-tools-wind32-x86-3180000.zip"
# and after unzipping, you want sqlite3.exe
# place sqlite3.exe in your databases folder (dbLoc)

# for more info, see
# https://sqlite.org/cli.html#converting_an_entire_database_to_an_ascii_text_file

# paths ----
# Lines that require editing

# do you want to populate the DB with example data? If not change to FALSE (schema only)
example_data <- TRUE 
# path to git sqlite sub-folder
gitLoc <- "E:/git/aquatic/Regional_SDM/sqlite"

# database location and name
dbLoc <- "D:/testing_SDM/aqua3/databases"
dbName <- "SDM_lookupAndTracking.sqlite"

# create the directory
if (!dir.exists(dbLoc)) dir.create(dbLoc, recursive = T)
# NOW COPY sqlite.exe to the new directory (See details in header)

# End, lines that require editing

# create database ----
## use these three commands to create the SQLite db
## from the text file located in the repository.
## This assumes you are on windows. If on Linux, you would
## want to replace 'type' with 'cat' but otherwise it should work.
# This also assumes you have sqlite3.exe in the dbLoc folder. 

if (file.exists(paste0(dbLoc, "/sqlite3.exe"))) {
  setwd(gitLoc)
  # just schema
  toSQLitecmd <- paste('type sqlite_template_db_nodata.sql | "',
                       dbLoc, '/sqlite3.exe" "',
                       dbLoc, '/', dbName, '"', sep = "")
  shell(toSQLitecmd)
  # example data
  if (example_data) {
    toSQLitecmd <- paste('type example_data.sql | "',
                         dbLoc, '/sqlite3.exe" "',
                         dbLoc, '/', dbName, '"', sep = "")
    shell(toSQLitecmd)
  }
  message("New database '", dbLoc, '/', dbName, "' created.")
} else {
  stop("You need to copy the sqlite3.exe executable to '",dbLoc,"' to continue. See file header for details.")
}
