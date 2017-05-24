# varclus Variable Clustering

library(Hmisc)

# load data, QC ----
###
## two lines need your attention. The one directly below (loc_scripts)
## and about line 43 where you choose which random points file to use
loc_scripts <- "E:/SDM/Aquatic/scripts/Regional_SDM"

source(paste(loc_scripts, "0_pathsAndSettings.R", sep = "/"))
setwd(loc_envVars)
EnvVars <- read.csv("EnvVars.csv", colClasses=c("HUC12"="character")) 
# drop uneeded columns
EnvVars$X <- NULL
EnvVars$HUC12 <- NULL
EnvVars$COMID <- NULL
EnvVars$OID <- NULL
EnvVars$X.1 <- NULL
# delete EV columns where all rows are zero (the snow/ice NLCD cover issue)
# doing this manually for now, but I should come up with something a little more robust.
###  may way to look at removing variables that are the same across all reaches as well.
EnvVars$PctIce2006Cat <- NULL
EnvVars$PctIce2006Ws <- NULL

# run correlation analysis
correlationtree <- varclus(data.matrix(EnvVars), similarity="spear")

# plot the data
plot(correlationtree)


