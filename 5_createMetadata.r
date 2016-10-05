

library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(randomForest)
library(knitr)
library(raster)
library(sp)
library(rgdal)
library(RColorBrewer)

inPath <- "D:/RegionalSDM/outputs"
rnwPath <- "D:/RegionalSDM/scripts/Regional_SDM"
outPath <- "D:/RegionalSDM/outputs/metadata"
gridpath <- "D:/RegionalSDM/outputs/grids"
stateBoundPath <- "D:/RegionalSDM/other_spatial"

##get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
### which one do we want to run?
n <- 1
fileName <- d[[n]]
load(paste(inPath,fileName, sep="/"))


## get the grid so we can plot it
g <- dir(path = gridpath, pattern = ".tif$", full.names = FALSE)
g
# which one?
n <- 1
ras <- raster(paste(gridpath, g[[n]], sep = "/"))

bnds <- readOGR(stateBoundPath, "StateBoundariesAlbersConicEqualArea")

# pall <- colorRampPalette(brewer.pal(9,"Blues"))(30)
# image(ras, col = pall)
# plot(bnds, add = TRUE)

#spplot(ras, col.regions=pall)


##
# writing to the same folder as a grid might cause problems.
# if errors check that first
#   more explanation: tex looks for and uses aux files, which are also used
#   by esri. If there's a non-tex aux file, knitr bails. 
##
setwd(outPath)

knit2pdf(paste(rnwPath,"MetadataEval_knitr.rnw",sep="/"), output=paste(ElementNames$Code, ".tex",sep=""))



