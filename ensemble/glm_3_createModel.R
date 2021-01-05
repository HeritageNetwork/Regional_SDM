# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - create initial model to remove poorest performing env vars
# - validate using leave-one-out jackknifing
# - create a final model using all presence points, stratify by EO using RA
# - build partial plots of top performing env vars for metadata output

library(RSQLite)
# library(ROCR)    #for ROC plots and stats
# library(vcd)     #for kappa stats
# library(abind)   #for collapsing the nested lists
# library(foreign) #for reading dbf files
# library(randomForest)
# library(iterators)
# library(doParallel)
library(caret)
library(pdp)
library(vip)

# get remove least important vars
ctrl <- rfeControl(functions = lrFuncs,
                   method = "repeatedcv",
                   repeats = 1,
                   number = 3,
                   verbose = FALSE)

lmProfile <- rfe(pres ~ ., data = df.full[,c(depVarCol,indVarCols)],
                 sizes = c(1:length(indVarCols)), #size of predictor variables, 
                 rfeControl = ctrl)


# no clear importance value using rfe, so use magnitude of effect for 
# correlated var removal

impvals <- lmProfile$fit$coefficients[order(lmProfile$fit$coefficients, decreasing = TRUE)]
impvals <- as.data.frame(impvals[!names(impvals) == "(Intercept)"])
names(impvals) <- "coef"

# remove the bottom of the correlated vars
corrdEVs <- corrdEVs[tolower(corrdEVs$gridName) %in% row.names(impvals),]
if(nrow(corrdEVs) > 0 ){
  for(grp in unique(corrdEVs$correlatedVarGroupings)){
    vars <- tolower(corrdEVs[corrdEVs$correlatedVarGroupings == grp,"gridName"])
    imp.sub <- impvals[rownames(impvals) %in% vars,, drop = FALSE]
    suppressWarnings(varsToDrop <- imp.sub[!imp.sub == max(imp.sub),, drop = FALSE])
    impvals <- impvals[!rownames(impvals) %in% rownames(varsToDrop),,drop = FALSE]
  }
  rm(vars, imp.sub, varsToDrop)
}

df.full.pt1 <- df.full[,1:(indVarCols[[1]]-1)]
df.full.pt2 <- df.full[,names(df.full) %in% rownames(impvals)]
glm.df.full <- cbind(df.full.pt1, df.full.pt2)
indVarCols <- indVarCols[[1]]:length(names(glm.df.full))
rm(df.full.pt1, df.full.pt2, impvals)

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
glm.df.full.s <- subSampByGp(glm.df.full, sampSizeVec, group$colNm)

# randomly assign group ids to background points so background pts
# can be drawn into KFolds

# vector to split up represents row indices
rowInd <- seq(1, nrow(glm.df.full.s[glm.df.full.s$pres == "0",]),by=1)
# first, randomly assign based on number of pres records in each group
presCounts <- table(glm.df.full.s[,group$colNm])
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
# this defines group assignment for each glm.df.full.s row (assumed pres=1 is first)
fullSampVec <- c(as.character(glm.df.full.s[glm.df.full.s$pres == "1",group$colNm]), names(sampVec))
glm.df.full.s$stratum <- fullSampVec


if(length(group$vals) > 50){
  kf <- 50
} else {
  kf <- length(group$vals)
}

# define the folds
folds <- groupKFold(glm.df.full.s$stratum, k = kf) 

glmfitControl <- trainControl(
  method = "LGOCV",
  index = folds,
  number = length(folds),
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = TRUE
)

# caret seems to need this
glm.df.full.s$pres <- as.character(glm.df.full.s$pres)
glm.df.full.s[glm.df.full.s$pres == "0","pres"] <- "abs"
glm.df.full.s[glm.df.full.s$pres == "1","pres"] <- "pres"
glm.df.full.s$pres <- as.factor(glm.df.full.s$pres)

glmFit1 <- train(y = glm.df.full.s[,"pres"],
                x = glm.df.full.s[,indVarCols],
                 method = "glm",
                 family = "binomial",
                 trControl = glmfitControl)


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
                             tot_bkgd_subsamp = nrow(glm.df.full.s[glm.df.full.s$pres == "abs",]),
                             obs_count = nrow(df.in), 
                             bkgd_count = nrow(df.abs)
)
dbExecute(db, paste0("DELETE FROM tblModelInputs where table_code = '", baseName, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelInputs", tblModelInputs, append = TRUE)
# write validation data

glmTuneOutput <- glmFit1$results[as.numeric(rownames(glmFit1$finalModel$tuneValue)),]
glm.summ.table <- as.data.frame(cbind("model_run_name" = rep(model_run_name, 4), 
                                      "algorithm" = rep(algo, 4), 
                                      "metric" = c("AUC","Sensitivity","Specificity","TSS"),
                                      "metric_mn" = c(glmTuneOutput$ROC,
                                                      glmTuneOutput$Sens,
                                                      glmTuneOutput$Spec, 
                                                      glmTuneOutput$Sens + glmTuneOutput$Spec - 1),
                                      "metric_sd" = c(glmTuneOutput$ROCSD,
                                                      glmTuneOutput$SensSD,
                                                      glmTuneOutput$SpecSD,
                                                      NA)
))

dbExecute(db, paste0("DELETE FROM tblModelResultsValidationStats where model_run_name = '", 
                     model_run_name, 
                     "' and algorithm = '", 
                     algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelResultsValidationStats", glm.summ.table, append = TRUE)

dbDisconnect(db)
rm(db)

####
# Importance measures ----
####

varList <- names(glmFit1$trainingData)[!names(glmFit1$trainingData) %in% c(".outcome")]

# estimate importance from vip package. glm//caret doesn't always give results
glm.impvals <- vi_firm(glmFit1, varList)
names(glm.impvals) <- c("gridName","impVal")

glm.impvals <- glm.impvals[order(glm.impvals$impVal, decreasing = TRUE),]

#TODO: not sure why NAs would occur but one is in my test set. should track down this edge case
# possibly there was only one input value for the variable in the training set (no variance)
glm.impvals[is.na(glm.impvals$impVal),"impVal"] <- 0

db <- dbConnect(SQLite(),dbname=nm_db_file)
# get importance data, set up a data frame
glm.EnvVars <- data.frame(gridName = glm.impvals$gridName, impVal = glm.impvals$impVal, fullName="", stringsAsFactors = FALSE)
#set the query for the following lookup, note it builds many queries, equal to the number of vars
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName COLLATE NOCASE in ('", 
                  paste(glm.EnvVars$gridName,sep=", "),
                  "'); ", sep="")
#cycle through all select statements, put the results in the df
for(i in 1:length(glm.EnvVars$gridName)) {
  try(glm.EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2]))
}
##clean up
#dbDisconnect(db)

# ###
# # partial plot data ----
# ###

# using pdp package

#get the order for the importance charts
ord <- order(glm.EnvVars$impVal, decreasing = TRUE)
if(length(ord) > 9){
  pPlotListLen <- 9
} else {
  pPlotListLen <- length(ord)
}

# pdp allows calculating together, but that is a combinatorial 
# exercise, not what we want here. Calculate individually
numvars <- nrow(glm.EnvVars)
glm.pPlots <- vector(mode = "list", length = numvars)
for(i in 1:numvars){
  glm.pPlots[[i]] <-partial(glmFit1, pred.var = glm.EnvVars$gridName[i], 
                            plot = FALSE) 
}

glm.pPlots$fullNames <- glm.EnvVars$fullName

# write model metadata to db

# tblModelResultsVarsUsed
varImpDB <- data.frame(model_run_name = model_run_name, 
                       algorithm = algo, 
                       gridName = tolower(envvar_list), 
                       inFinalModel = 0)
varImpDB <- merge(varImpDB, glm.EnvVars[c("gridName","impVal")], by = "gridName", all.x = TRUE)
varImpDB$inFinalModel[!is.na(varImpDB$impVal)] <- 1
dbWriteTable(db, "tblModelResultsVarsUsed", varImpDB, append = TRUE)
dbDisconnect(db)

closeAllConnections()
