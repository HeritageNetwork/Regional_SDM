#####
# This script can make a raster brick out of a folder of tiff files.
# A brick might speed up processing a little bit over a raster stack
# but also may create an unwieldy monstrosity.
#
# Modified from a script written by Emilie Henderson
####

#Test edit by RG

library(raster)

# set this path to the folder where the environmental rasters reside
# it can have other files, but only *.tifs that you want to be included
# in the brick
pathToTifs <- "G:/RegionalSDM/env_vars/geotiffs"

# the path to write out the brick to
pathToBrick <- "G:/RegionalSDM/env_vars/brick"

# get a list of the grids
tiflist <- list.files(path = pathToTifs, pattern = ".tif$")

# tack on the full paths and name them
gridlist<-as.list(paste(pathToTifs,tiflist,sep = "/"))
nm <- substr(tiflist,1,nchar(tiflist) - 4)
names(gridlist)<-nm

# make the brick, writing it out at the same time
envBrick <- brick(stack(gridlist),filename = paste(pathToBrick,"/brick.grd",sep = ""))
  
# ## To look at the brick, use these commands:
# plot(envBrick) #plots all the layers in the brick
# plot(envBrick,nm[1]) #plots only layer 1, for example

