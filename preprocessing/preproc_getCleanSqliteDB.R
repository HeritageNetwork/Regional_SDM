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

library(here)

# this assumes a databases folder in the _data folder within the project. 
# it doesn't require any unique paths to be set

# database location and name
dbLoc <- here("_data","databases")
dbName <- "SDM_lookupAndTracking.sqlite"

# path to sqlite dump folder
gitLoc <- here("sqlite")


# create database ----
## use these three commands to create the SQLite db
## from the text file located in the repository.
## This assumes you are on windows. If on Linux, you would
## want to replace 'type' with 'cat' but otherwise it should work.
# This also assumes you have sqlite3.exe in the dbLoc folder. 

setwd(gitLoc)
toSQLitecmd <- paste('type sqliteDBDump.txt | "',
                     dbLoc, '/sqlite3" "',
                     dbLoc, '/', dbName, '"', sep = "")
shell(toSQLitecmd)

# database dump ----
## these three commands will create a new text representation 
## of the sqlite DB. We might want to do this periodically with
## new schema changes. 

# setwd(gitLoc)
# toTextcmd <- paste(dbLoc, '/sqlite3.exe \"',
#                    dbLoc, '/', dbName, '\" .dump > ',
#                    'sqliteDBDump.txt', sep = "")
# shell(toTextcmd)
