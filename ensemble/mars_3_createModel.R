# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - create initial model to remove poorest performing env vars
# - validate using leave-one-out jackknifing
# - create a final model using all presence points, stratify by EO using RA
# - build partial plots of top performing env vars for metadata output

library(RSQLite)

library(caret)
library(pdp)
library(vip)
library(earth)

# get remove least important vars with a simple model

# marsOut <- earth(
#   x = df.full[,indVarCols], 
#   y = df.full[,depVarCol]
# )
# 
# impvals <- varImp(marsOut)
# names(impvals) <- "coef"


#x <- evimp(marsOut)

# impvals <- lmProfile$fit$coefficients[order(lmProfile$fit$coefficients, decreasing = TRUE)]
# impvals <- as.data.frame(impvals[!names(impvals) == "(Intercept)"])
# names(impvals) <- "coef"

# remove the bottom of the correlated vars  #MARS trims so extremely. don't do this
# mars.corrdEVs <- corrdEVs[tolower(corrdEVs$gridName) %in% row.names(impvals),]
# if(nrow(mars.corrdEVs) > 0 ){
#   for(grp in unique(mars.corrdEVs$correlatedVarGroupings)){
#     vars <- tolower(mars.corrdEVs[mars.corrdEVs$correlatedVarGroupings == grp,"gridName"])
#     imp.sub <- impvals[rownames(impvals) %in% vars,, drop = FALSE]
#     suppressWarnings(varsToDrop <- imp.sub[!imp.sub == max(imp.sub),, drop = FALSE])
#     impvals <- impvals[!rownames(impvals) %in% rownames(varsToDrop),,drop = FALSE]
#   }
#   rm(vars, imp.sub, varsToDrop)
# }

# df.full.pt1 <- df.full[,1:(indVarCols[[1]]-1)]
# df.full.pt2 <- df.full[,names(df.full) %in% rownames(impvals)]
# mars.df.full <- cbind(df.full.pt1, df.full.pt2)
# indVarCols <- indVarCols[[1]]:length(names(mars.df.full))
# rm(df.full.pt1, df.full.pt2, impvals)

mars.df.full <- df.full

# set up for main run

subSampByGp <- function(x, ssvec, gpColName) {
  df.sub = x[FALSE,]
  for(i in 1:length(ssvec)){
    cls <- names(ssvec[i])
    if(cls == "pseu-a"){
      toDrawFrom <- x[x[,"pres"] == "0", ]
    } else {
      toDrawFrom <- x[x[,gpColName] == cls & x[,"pres"] == "1", ]
    }
    if(ssvec[[i]] > nrow(toDrawFrom)) { #lame
      ssvec[[i]] <- nrow(toDrawFrom)
    }
    draws <- sample(1:nrow(toDrawFrom), ssvec[[i]], replace = FALSE)  
    drawnSamps <- toDrawFrom[draws,]
    df.sub <- rbind(df.sub, drawnSamps)
  }
  df.sub
}

# subsample
mars.df.full.s <- subSampByGp(mars.df.full, sampSizeVec, group$colNm)

# randomly assign group ids to background points so background pts
# can be drawn into KFolds

# vector to split up represents row indices
rowInd <- seq(1, nrow(mars.df.full.s[mars.df.full.s$pres == "0",]),by=1)
# first, randomly assign based on number of pres records in each group
presCounts <- table(mars.df.full.s[,group$colNm])
presCounts <- presCounts[!names(presCounts) == "pseu-a"]
# start by assigning a base number to avoid huge skews
drawAmt <- ifelse(presCounts>5,5,presCounts)
# now get weights for distributing the rest
numToDistribute <- length(rowInd) - sum(drawAmt)
weights <- presCounts/sum(presCounts)
drawAmt <- drawAmt + round(numToDistribute*weights)
# rounding might keep this from coming out even, so fix if it didn't
if(sum(drawAmt) > length(rowInd)){
  reduceAmt <- sum(drawAmt) - length(rowInd)
  # find the biggest set and subtract
  drawAmt[drawAmt == max(drawAmt)] <- max(drawAmt) - reduceAmt
}
if(sum(drawAmt) < length(rowInd)){
  increaseAmt <- length(rowInd) - sum(drawAmt)
  # find the biggest set and add
  drawAmt[drawAmt == max(drawAmt)] <- max(drawAmt) + increaseAmt
}
# now randomly place absence rows into groups for k-folding
sampVec <- integer(0)
for(i in 1:length(drawAmt)){
  x <- sample(rowInd, drawAmt[i])
  names(x) <- rep(names(drawAmt[i]),drawAmt[i])
  rowInd <- rowInd[!rowInd %in% x]
  sampVec <- c(sampVec, x)
}
rm(rowInd, presCounts, drawAmt, numToDistribute, weights, i, x)

sampVec <- sampVec[order(sampVec)]
# this defines group assignment for each mars.df.full.s row (assumed pres=1 is first)
fullSampVec <- c(as.character(mars.df.full.s[mars.df.full.s$pres == "1",group$colNm]), names(sampVec))
mars.df.full.s$stratum <- fullSampVec


if(length(group$vals) > 50){
  kf <- 50
} else {
  kf <- length(group$vals)
}

# define the folds
folds <- groupKFold(mars.df.full.s$stratum, k = kf) 

marsfitControl <- trainControl(
  method = "LGOCV",
  index = folds,
  number = length(folds),
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = TRUE
)

# caret seems to need this
mars.df.full.s$pres <- as.character(mars.df.full.s$pres)
mars.df.full.s[mars.df.full.s$pres == "0","pres"] <- "abs"
mars.df.full.s[mars.df.full.s$pres == "1","pres"] <- "pres"
mars.df.full.s$pres <- as.factor(mars.df.full.s$pres)

marsFit1 <- train(y = mars.df.full.s[,"pres"],
                x = mars.df.full.s[,indVarCols],
                 method = "earth",
                 trControl = marsfitControl)


###
# write out input data
###

db <- dbConnect(SQLite(),dbname=nm_db_file)
# write model input data to database before any other changes made
tblModelInputs <- data.frame(table_code = baseName,
                             model_run_name = model_run_name,
                             algorithm = algo,
                             EGT_ID = ElementNames$EGT_ID, 
                             datetime = as.character(Sys.time()),
                             feat_count = length(unique(df.in$stratum)), 
                             feat_grp_count = length(unique(df.in$group_id)),
                             jckn_grp_column = group$colNm,
                             jckn_grp_type = group$JackknType,
                             mn_grp_subsamp = mean(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             min_grp_subsamp = min(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             max_grp_subsamp = max(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             tot_obs_subsamp = sum(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             tot_bkgd_subsamp = nrow(mars.df.full.s[mars.df.full.s$pres == "abs",]),
                             obs_count = nrow(df.in), 
                             bkgd_count = nrow(df.abs)
)
dbExecute(db, paste0("DELETE FROM tblModelInputs where table_code = '", baseName, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelInputs", tblModelInputs, append = TRUE)
# write validation data

marsTuneOutput <- marsFit1$results[as.numeric(rownames(marsFit1$finalModel$tuneValue)),]
mars.summ.table <- as.data.frame(cbind("model_run_name" = rep(model_run_name, 4), 
                                      "algorithm" = rep(algo, 4), 
                                      "metric" = c("AUC","Sensitivity","Specificity","TSS"),
                                      "metric_mn" = c(marsTuneOutput$ROC,
                                                      marsTuneOutput$Sens,
                                                      marsTuneOutput$Spec, 
                                                      marsTuneOutput$Sens + marsTuneOutput$Spec - 1),
                                      "metric_sd" = c(marsTuneOutput$ROCSD,
                                                      marsTuneOutput$SensSD,
                                                      marsTuneOutput$SpecSD,
                                                      NA)
))

dbExecute(db, paste0("DELETE FROM tblModelResultsValidationStats where model_run_name = '", 
                     model_run_name, 
                     "' and algorithm = '", 
                     algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelResultsValidationStats", mars.summ.table, append = TRUE)

dbDisconnect(db)
rm(db)

####
# Importance measures ----
####

varList <- names(marsFit1$trainingData)[!names(marsFit1$trainingData) %in% c(".outcome")]

mars.impvals <- varImp(marsFit1)$importance
mars.impvals$gridName <- rownames(mars.impvals)
names(mars.impvals) <- c("impVal", "gridName")

mars.impvals <- mars.impvals[order(mars.impvals$impVal, decreasing = TRUE),]

#any NAs?  replace with zero
#TODO: not sure why NAs would occur but one is in my test set. should track down this edge case
# possibly there was only one input value for the variable in the training set (no variance)
mars.impvals[is.na(mars.impvals$impVal),"impVal"] <- 0

db <- dbConnect(SQLite(),dbname=nm_db_file)
# get importance data, set up a data frame
mars.EnvVars <- data.frame(gridName = mars.impvals$gridName, impVal = mars.impvals$impVal, fullName="", stringsAsFactors = FALSE)
#set the query for the following lookup, note it builds many queries, equal to the number of vars
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName COLLATE NOCASE in ('", 
                  paste(mars.EnvVars$gridName,sep=", "),
                  "'); ", sep="")
#cycle through all select statements, put the results in the df
for(i in 1:length(mars.EnvVars$gridName)) {
  try(mars.EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2]))
}
##clean up
#dbDisconnect(db)

# ###
# # partial plot data ----
# ###

# using pdp package

#get the order for the importance charts
ord <- order(mars.EnvVars$impVal, decreasing = TRUE)
if(length(ord) > 9){
  pPlotListLen <- 9
} else {
  pPlotListLen <- length(ord)
}

# pdp allows calculating together, but that is a combinatorial 
# exercise, not what we want here. Calculate individually
numvars <- nrow(mars.EnvVars)
mars.pPlots <- vector(mode = "list", length = numvars)
for(i in 1:numvars){
  mars.pPlots[[i]] <-partial(marsFit1, pred.var = mars.EnvVars$gridName[i], 
                            plot = FALSE) 
}

mars.pPlots$fullNames <- mars.EnvVars$fullName

# write model metadata to db

# tblModelResultsVarsUsed
varImpDB <- data.frame(model_run_name = model_run_name, 
                       algorithm = algo, 
                       gridName = tolower(envvar_list), 
                       inFinalModel = 0)
varImpDB <- merge(varImpDB, mars.EnvVars[c("gridName","impVal")], by = "gridName", all.x = TRUE)
varImpDB$inFinalModel[!is.na(varImpDB$impVal)] <- 1
dbWriteTable(db, "tblModelResultsVarsUsed", varImpDB, append = TRUE)
dbDisconnect(db)

closeAllConnections()
