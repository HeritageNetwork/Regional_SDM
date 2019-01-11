# add EV variables to input data
# this script doesn't alter the structure of the current file
# also faster than ArcGIS

# load data, QC ----
###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 43 where you choose which random points file to use
loc_scripts <- "E:/SDM/Aquatic2/scripts/Regional_SDM"

source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))
setwd(loc_envVars)
EnvVars <- read.csv("EnvVars.csv", colClasses=c("huc12"="character")) 

# fix the number of digits in the huc12 if the leading zeros are stripped.
library(stringr)
EnvVars$huc12 <- str_pad(EnvVars$huc12,12,pad="0")
# enter the huc level you want to add (eg. 2,4,6,8,10,12) 
hucLevel <- 4
EnvVars$hucSubset <- substr(EnvVars$huc12,1,hucLevel)
# replace the old EnvVars.csv with the new one
file.rename(from="E:/SDM/Aquatic2/env_vars/EnvVars.csv",to="E:/SDM/Aquatic2/env_vars/old/EnvVars1.csv")
# write the new file as a csv
write.csv(EnvVars, "EnvVars.csv", row.names=FALSE)
# note: you should always rerun and check the correlations after adding variables.

# insert row into database with variable name
db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLQuery <- paste0("INSERT INTO lkpEnvVarsAqua ('fullName','gridName','description','category','use_A') VALUES ('hucSubset','hucSubset','higher level HUC to limit model predictions to areas where species are found','custom','1');")  
dbExecute(db, SQLQuery)
# clean up
dbDisconnect(db)
