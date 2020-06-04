# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - validate using leave-one-out jackknifing
# - create initial model to remove poorest performing env vars
# - create a final model using all presence points, stratify by EO using RA
# - build partial plots of top performing env vars for metadata output

library(RSQLite)
#library(ROCR)    #for ROC plots and stats
#library(vcd)     #for kappa stats
#library(abind)   #for collapsing the nested lists
#library(foreign) #for reading dbf files
#library(randomForest)
#library(iterators)
#library(doParallel)

library(caret)
library(recipes)
# for xgb
library(xgboost)

# having parallel package loaded messes up caret
#detach("package:doParallel", unload=TRUE)


###
# Remove the least important env vars ----
##


df.full.xgb <- xgb.DMatrix(as.matrix(df.full[,indVarCols]), 
                           label=as.integer(as.character(df.full$pres)))


param <- list(
  booster = "gbtree",
  max_depth = 5, 
  eta = 0.3, 
  verbose = 0, 
  nthread = 4,
  objective = "binary:logistic", 
  eval_metric = "auc")

xgb.find.envars <- xgb.train(params=param, data = df.full.xgb,
                      nrounds = 20)


xgb.impvals <- xgb.importance(model=xgb.find.envars)

#xgb already removed a bunch of env vars?
OriginalNumberOfEnvars <- nrow(xgb.impvals)

# first remove the bottom of the correlated vars
corrdEVs <- corrdEVs[tolower(corrdEVs$gridName) %in% xgb.impvals$Feature,]
if(nrow(corrdEVs) > 0 ){
  for(grp in unique(corrdEVs$correlatedVarGroupings)){
    vars <- tolower(corrdEVs[corrdEVs$correlatedVarGroupings == grp,"gridName"])
    imp.sub <- xgb.impvals[xgb.impvals$Feature %in% vars,, drop = FALSE]
    suppressWarnings(varsToDrop <- imp.sub[!imp.sub$Gain == max(imp.sub$Gain),, drop = FALSE])
    xgb.impvals <- xgb.impvals[!xgb.impvals$Feature %in% varsToDrop$Feature,,drop = FALSE]
  }
  rm(vars, imp.sub, varsToDrop)
}

# set the percentile, here choosing above 25% percentile
envarPctile <- 0.25
#envarPctile <- 0.90
y <- quantile(xgb.impvals$Gain, probs = envarPctile)
impEnvVars <- xgb.impvals[xgb.impvals$Gain > y,]
subsetNumberofEnvars <- nrow(impEnvVars)
rm(y)
# which columns are these, then flip the non-envars to TRUE
impEnvVarCols <- names(df.full) %in% impEnvVars$Feature
impEnvVarCols[1:5] <- TRUE
# subset!
df.full <- df.full[,impEnvVarCols]
# reset the indvarcols object
indVarCols <- c(6:length(names(df.full)))

rm(xgb.impvals, impEnvVars, impEnvVarCols)



outPth <- file.path(loc_model, ElementNames$Code,"outputs","ensemble")
dir.create(outPth, showWarnings = FALSE)

if(length(group$vals) > 50){
  kf <- 50
} else {
  kf <- length(group$vals)
}

# randomly assign group ids to background points so background pts
# can be drawn into KFolds

# vector to split up represents row indices
rowInd <- seq(1, nrow(df.full[df.full$pres == 0,]),by=1)
# first, randomly assign based on number of pres records in each group
presCounts <- table(df.full[,group$colNm])
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

#sampVec2 <- unlist(y, use.names = TRUE)
#sampVec <- c(sampVec, sampVec2)
sampVec <- sampVec[order(sampVec)]
# this defines group assignment for each df.full row (assumed pres=1 is first)
fullSampVec <- c(as.character(df.full[df.full$pres == 1,group$colNm]), names(sampVec))
df.full$stratum <- fullSampVec

# subsample down to a reasonable number 

# subsample function. x is data frame. ssvec is sampSizeVec (not including pseu-a)
# this will get the df down to the size indicated by sampSizeVec, 
# and possibly could be used in a recipe inside the caret model (as a downsample)
# but I can't figure that out yet. 
# use it here to downsample before modeling
# subSampByGp <- function(x, ssvec) {
#   df.sub = x[FALSE,]
#   for(i in 1:length(ssvec)){
#     cls <- names(ssvec[i])
#     toDrawFrom <- x[x$stratum == cls, ]
#     draws <- sample(1:nrow(toDrawFrom), ssvec[[i]])
#     drawnSamps <- toDrawFrom[draws,]
#     df.sub <- rbind(df.sub, drawnSamps)
#   }
#   df.sub
# }

subSampByGp <- function(x, ssvec, gpColName) {
  df.sub = x[FALSE,]
  for(i in 1:length(ssvec)){
    cls <- names(ssvec[i])
    toDrawFrom <- x[x[,gpColName] == cls, ]
    if(ssvec[[i]] > nrow(toDrawFrom)) { #lame
      ssvec[[i]] <- nrow(toDrawFrom)
    }
    draws <- sample(1:nrow(toDrawFrom), ssvec[[i]], replace = FALSE)  
    drawnSamps <- toDrawFrom[draws,]
    df.sub <- rbind(df.sub, drawnSamps)
  }
  df.sub
}

df.full.s <- subSampByGp(df.full, sampSizeVec[-grep("pseu-a", names(sampSizeVec))], group$colNm)
## TODO sample background down to a reasonable number ##### research it?
df.full.s <- rbind(df.full.s, df.full[df.full$pres == 0,])

# define the folds
folds <- groupKFold(df.full.s$stratum, k = kf) 

#library(pROC)
# xgbfitControl <- trainControl(
#   method = "LGOCV",  #leave group out cross validation
#   index = folds,
#   number = 1,
#   summaryFunction = twoClassSummary,
#   classProbs = TRUE
#   )



xgbfitControl <- trainControl(
  method = "repeatedcv",
  index = folds,
  number = 1,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

# caret seems to need this
df.full.s$pres <- as.character(df.full.s$pres)
df.full.s[df.full.s$pres == "0","pres"] <- "abs"
df.full.s[df.full.s$pres == "1","pres"] <- "pres"
df.full.s$pres <- as.factor(df.full.s$pres)

# xgbGrid <-  expand.grid(
#   nrounds = c(5, 10, 20),
#   max_depth = c(2, 5, 9), 
#   eta = 1,
#   gamma = 0,
#   colsample_bytree = 1,
#   min_child_weight = 1,
#   subsample = 1
# )
#modRecipe <- recipe(pres ~ ., data = df.full.s
#)

# run validation with caret
xgbFit1 <- train(pres ~ ., data = df.full.s[,c(depVarCol,indVarCols)],
                 method = "xgbTree",
                 trControl = xgbfitControl,
                 metric = "ROC")

# flip *back* for raw xgb
df.full.s$pres <- as.character(df.full.s$pres)
df.full.s[df.full.s$pres == "abs","pres"] <- "0"
df.full.s[df.full.s$pres == "pres","pres"] <- "1"
df.full.s$pres <- as.factor(df.full.s$pres)

df.full.s.xgb <- xgb.DMatrix(as.matrix(df.full.s[,indVarCols]), 
                           label=as.integer(as.character(df.full.s$pres)))

####
#   run the full model ----
####

xgb.full <- xgb.train(params=xgbFit1$finalModel$params, 
                             data = df.full.s.xgb,
                             nrounds = 100)


# write out input data
# connect to DB ..
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
                             tot_bkgd_subsamp = nrow(df.full.s[df.full.s$pres == "0",]),
                             obs_count = nrow(df.in), 
                             bkgd_count = nrow(df.abs)
)
dbExecute(db, paste0("DELETE FROM tblModelInputs where table_code = '", baseName, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelInputs", tblModelInputs, append = TRUE)
# write validation data

xgbTuneOutput <- xgbFit1$results[as.numeric(rownames(xgbFit1$finalModel$tuneValue)),]


xgb.summ.table <- as.data.frame(cbind("model_run_name" = rep(model_run_name, 4), 
                    "algorithm" = rep(algo, 4), 
                    "metric" = c("AUC","Sensitivity","Specificity","TSS"),
                    "metric_mn" = c(xgbTuneOutput$ROC,
                                    xgbTuneOutput$Sens,
                                    xgbTuneOutput$Spec, 
                                    xgbTuneOutput$Sens + xgbTuneOutput$Spec - 1),
                    "metric_sd" = c(xgbTuneOutput$ROCSD,
                                    xgbTuneOutput$SensSD,
                                    xgbTuneOutput$SpecSD,
                                    NA)
                    ))

dbExecute(db, paste0("DELETE FROM tblModelResultsValidationStats where model_run_name = '", model_run_name, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelResultsValidationStats", xgb.summ.table, append = TRUE)

dbDisconnect(db)
rm(db)

####
# Importance measures ----
####
xgb.impvals <- xgb.importance(model=xgb.full)

g.imp <- xgb.impvals[,c("Feature","Gain")]

db <- dbConnect(SQLite(),dbname=nm_db_file)
# get importance data, set up a data frame
xgb.EnvVars <- data.frame(gridName = g.imp$Feature, impVal = g.imp$Gain, fullName="", stringsAsFactors = FALSE)
#set the query for the following lookup, note it builds many queries, equal to the number of vars
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName COLLATE NOCASE in ('", paste(xgb.EnvVars$gridName,sep=", "),
					"'); ", sep="")
#cycle through all select statements, put the results in the df
for(i in 1:length(xgb.EnvVars$gridName)) {
  try(xgb.EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2]))
}
##clean up
#dbDisconnect(db)

# ###
# # partial plot data ----
# ###
df.full.s$pres <- as.integer(as.character(df.full.s$pres))

#get the order for the importance charts
ord <- order(xgb.EnvVars$impVal, decreasing = TRUE)[1:length(indVarCols)]
if(length(ord) > 9){
  pPlotListLen <- 9
} else {
  pPlotListLen <- length(ord)
}

# cat("... calculating partial plots \n")
# 
# ### subsample, grouped by pres/abs, to speed up partial plots
# ppPres <- df.full[df.full$pres == 1, ]
# ppAbs <- df.full[df.full$pres == 0, ]
# ppPresSamp <- min(c(nrow(ppPres), 6000)) # take all pres samples, or 6000, whichever is less
# ppPresSamp <- sample(1:nrow(ppPres), size = round(ppPresSamp), replace = FALSE)
# ppPresSamp <- ppPres[ppPresSamp,]
# ppAbsSamp <- min(c(nrow(ppAbs), 6000)) # take all abs samples, or 6000, whichever is less
# ppAbsSamp <- sample(1:nrow(ppAbs), size = round(ppAbsSamp), replace = FALSE)
# ppAbsSamp <- ppAbs[ppAbsSamp,]
# 
# ppPreddata <- rbind(ppPresSamp, ppAbsSamp)
# 
# # run partial plots in parallel
#curvars = EnvVars$gridName[1:pPlotListLen]

xgb.pPlots <- xgb.plot.shap(data = as.matrix(df.full.s[,indVarCols]), 
                     model = xgb.full, 
                     features = xgb.EnvVars$gridName[1:pPlotListLen],
                     target_class = 1,
                     plot = FALSE)

xgb.pPlots$fullNames <- xgb.EnvVars$fullName

#plot(pPlots$data[,2], pPlots$shap_contrib[,2])

# xgb.plot.shap(data = as.matrix(df.full.s[,-grep("pres",names(df.full.s))]), 
#               model = xgb.full, 
#               features = "nm_calgyp",
#               target_class = 1,
#               plot = TRUE)



# write model metadata to db

# tblModelResultsVarsUsed
varImpDB <- data.frame(model_run_name = model_run_name, 
                       algorithm = algo, 
                       gridName = tolower(envvar_list), 
                       inFinalModel = 0)
varImpDB <- merge(varImpDB, xgb.EnvVars[c("gridName","impVal")], by = "gridName", all.x = TRUE)
varImpDB$inFinalModel[!is.na(varImpDB$impVal)] <- 1
dbWriteTable(db, "tblModelResultsVarsUsed", varImpDB, append = TRUE)
dbDisconnect(db)





closeAllConnections()



# 
# 
# 
# # save the project, return to the original working directory
# dir.create(paste0(loc_model, "/", model_species,"/outputs/rdata"), recursive = TRUE, showWarnings = FALSE)
# setwd(paste0(loc_model, "/", model_species,"/outputs"))
# 
# # don't save fn args/vars
# for(i in 1:length(modelrun_meta_data)) assign(names(modelrun_meta_data)[i], modelrun_meta_data[[i]])
# ls.save <- ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("begin_step","rdata","prompt","scrpt",
#                                                              "run_steps","prompt","fn_args", names(fn_args))]
# save(list = ls.save, file = paste0("rdata/xgb_", model_run_name,".Rdata"), envir = environment())
# 

