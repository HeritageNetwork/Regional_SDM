

library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(randomForest)
library(knitr)

inPath <- "F:/_Howard/Projects/EDM/2009_metadataout/withPPlots/VernPool"
rnwPath <- "G:/RegionalSDM/scripts/Regional_SDM"
outPath <- "G:/RegionalSDM/outputs/glypmuhl/metadata"
  
##get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
### which one do we want to run?
n <- 1
fileName <- d[[n]]

load(paste(inPath,fileName, sep="/"))

setwd(outPath)

knit2pdf(paste(rnwPath,"MetadataEval_knitr.rnw",sep="/"), output=paste(abbr, ".tex",sep=""))



