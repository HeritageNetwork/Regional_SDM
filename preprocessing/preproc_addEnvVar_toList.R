# add EV variables to input data
# this script doesn't alter the structure of the current file
# also faster than ArcGIS

# load data, QC ----
###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 43 where you choose which random points file to use
loc_scripts <- "E:/SDM/Aquatic/scripts/Regional_SDM"

source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))
setwd(loc_envVars)
EnvVars <- read.csv("EnvVars.csv", colClasses=c("HUC12"="character")) 

# load new EV data to add to above
new_EnvVar <- read.csv("W:/Heritage/Heritage_Projects/1362 Pearlshell/SDM/csv/lu_PRISM.csv")
names(new_EnvVar)
new_EnvVar$OID <- NULL
new_EnvVar$CatAreaSqKm <- NULL
new_EnvVar$WsAreaSqKm <- NULL
new_EnvVar$CatPctFull <- NULL
new_EnvVar$WsPctFull <- NULL

# do the join (may want to chose a different join type)
EnvVars_added <- merge(EnvVars, new_EnvVar, by="COMID")

# delete some fields introduced by the join
EnvVars$OID <- NULL
EnvVars$X.1 <- NULL

# replace the old EnvVars.csv with the new one
file.rename(from="E:/SDM/Aquatic/env_vars/EnvVars.csv",to="E:/SDM/Aquatic/env_vars/old/EnvVars1.csv")
# write the new file as a csv
write.csv(EnvVars_added, "EnvVars.csv")


# note, you shoudl always rerun and check the correlations after adding variables.
