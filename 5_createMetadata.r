

library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(randomForest)
library(knitr)

inPath <- "D:/RegionalSDM/outputs"
rnwPath <- "D:/RegionalSDM/scripts/Regional_SDM"
outPath <- "D:/RegionalSDM/outputs/glypmuhl"
  
##get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
### which one do we want to run?
n <- 1
fileName <- d[[n]]

load(paste(inPath,fileName, sep="/"))

setwd(outPath)

knit2pdf(paste(rnwPath,"MetadataEval_knitr.rnw",sep="/"), output=paste(ElementNames$Code, ".tex",sep=""))



