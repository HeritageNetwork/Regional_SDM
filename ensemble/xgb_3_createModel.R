# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - validate using leave-one-out jackknifing
# - create initial model to remove poorest performing env vars
# - create a final model using all presence points, stratify by EO using RA
# - build partial plots of top performing env vars for metadata output

library(RSQLite)
library(ROCR)    #for ROC plots and stats
library(vcd)     #for kappa stats
library(abind)   #for collapsing the nested lists
library(foreign) #for reading dbf files
library(randomForest)
library(iterators)
library(doParallel)

library(caret)
library(recipes)
# for xgb
library(xgboost)


###
# Remove the least important env vars ----
##


df.full.xgb <- xgb.DMatrix(as.matrix(df.full[,indVarCols]), 
                           label=as.integer(as.character(df.full.x$pres)))


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
# for gam, take top 10%
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
numToDraw <- table(df.full[,group$colNm])
numToDraw <- numToDraw[!names(numToDraw) == "pseu-a"]

sampVec <- integer(0)
for(i in 1:length(numToDraw)){
  x <- sample(rowInd, numToDraw[i])
  names(x) <- rep(names(numToDraw[i]),numToDraw[i])
  rowInd <- rowInd[!rowInd %in% x]
  sampVec <- c(sampVec, x)
}
# assign the remainders evenly
y <- split(rowInd, cut(rowInd, length(group$vals), labels = FALSE))
names(y) <- NULL
for(i in 1:length(y)){
  names(y[[i]]) <- rep(group$vals[[i]], length(y[[i]]))
}
sampVec2 <- unlist(y, use.names = TRUE)
sampVec <- c(sampVec, sampVec2)
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
subSampByGp <- function(x, ssvec) {
  df.sub = x[FALSE,]
  for(i in 1:length(ssvec)){
    cls <- names(ssvec[i])
    toDrawFrom <- x[x$stratum == cls, ]
    draws <- sample(1:nrow(toDrawFrom), ssvec[[i]])
    drawnSamps <- toDrawFrom[draws,]
    df.sub <- rbind(df.sub, drawnSamps)
  }
  df.sub
}
df.full.s <- subSampByGp(df.full, sampSizeVec[-grep("pseu-a", names(sampSizeVec))])

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

df.full.s <- df.full.s[,c(depVarCol,indVarCols)]
str(df.full.s)
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
modRecipe <- recipe(pres ~ ., data = df.full.s
)

xgbFit1 <- train(pres ~ ., data = df.full.s,
                 method = "xgbTree", 
                 trControl = xgbfitControl,
                 metric = "ROC")

xgb.out <- xgbFit1

# test predict
# x <- sample(1:nrow(df.full), 50)
# y <- predict(xgbFit1, newdata = df.full[x,], type = "prob")
# z <- cbind(y, df.full[x,"pres"])



##
# code above is for removing least important env vars
##

# # prep for validation loop ----
# #now that entire set is cleaned up, split back out to use any of the three DFs below
# df.in2 <- subset(df.full,pres == "1")
# df.abs2 <- subset(df.full, pres == "0")
# df.in2$stratum <- factor(df.in2$stratum)
# df.abs2$stratum <- factor(df.abs2$stratum)
# df.in2$group_id <- factor(df.in2$group_id)
# df.abs2$group_id <- factor(df.abs2$group_id)
# df.in2$pres <- factor(df.in2$pres)
# df.abs2$pres <- factor(df.abs2$pres)
# 
# #reset the row names, needed for random subsetting method of df.abs2, below
# row.names(df.in2) <- 1:nrow(df.in2)
# row.names(df.abs2) <- 1:nrow(df.abs2)
# 
# #reduce the number of trees if group$vals has more than 30 entries #commented out to be parallel with aquatic
# #this is for validation
# # if(length(group$vals) > 30) {
# # 	ntrees <- 500
# # } else {
# # 	ntrees <- 750
# # }
# ntrees <- 1000
# 
# # reduce the number of validation loops if more than 50. 50 is plenty!
# # randomly draw to get the validation set.
# if(length(group$vals) > 50) {
#   group$vals <- sample(group$vals, size = 50)
#   group$vals <- factor(group$vals)
# } 
# 
# ##initialize the Results vectors for output from the jackknife runs
# trRes <- vector("list",length(group$vals))
#    names(trRes) <- group$vals[]
# evSet <- vector("list",length(group$vals))
#    names(evSet) <- group$vals[]	   
# evRes <- vector("list",length(group$vals))
#    names(evRes) <- group$vals[]
# t.f <- vector("list",length(group$vals))
#    names(t.f) <- group$vals[]
# t.ctoff <- vector("list",length(group$vals))
#    names(t.ctoff) <- group$vals[]
# v.rocr.rocplot <- vector("list",length(group$vals))
#    names(v.rocr.rocplot) <- group$vals[]
# v.rocr.auc <- vector("list",length(group$vals))
#    names(v.rocr.auc) <- group$vals[]
# v.y <- vector("list",length(group$vals))
#    names(v.y) <- group$vals[]
# v.kappa <- vector("list",length(group$vals))
#    names(v.kappa) <- group$vals[]
# v.tss <- vector("list",length(group$vals))
#    names(v.tss) <- group$vals[]
# v.OvAc <- vector("list",length(group$vals))
#    names(v.OvAc) <- group$vals[]
# t.importance <- vector("list",length(group$vals))
#    names(t.importance) <- group$vals[]
# t.rocr.pred <- vector("list",length(group$vals))
#    names(t.rocr.pred) <- group$vals[]
# v.rocr.pred <- vector("list",length(group$vals))
#    names(v.rocr.pred) <- group$vals[]
#    
# #######
# ## This is the validation loop. ----
# ## it creates a model for all-but-one group (EO, polygon, or group),
# ## tests if it can predict that group left out,
# ## then moves on to another group, cycling though all groups
# ## Validation stats in tabular form are the final product.
# #######
# 
# # calculate the number of trees to send to each core
# treeSubs <- ntrees/numCores
# 
# if(length(group$vals)>1){
#   for(i in 1:length(group$vals)){
# 		   # Create an object that stores the select command, to be used by subset.
# 		  trSelStr <- parse(text=paste(group$colNm[1]," != '", group$vals[[i]],"'",sep=""))
# 		  evSelStr <- parse(text=paste(group$colNm[1]," == '", group$vals[[i]],"'",sep=""))
# 		   # apply the subset. do.call is needed so selStr can be evaluated correctly
# 		  trSet <- do.call("subset",list(df.in2, trSelStr))
# 		  evSet[[i]] <- do.call("subset",list(df.in2, evSelStr))
# 		   # use sample to grab a random subset from the background points
# 		  BGsampSz <- nrow(evSet[[i]])
# 		  #### TEMPORARY DURING TESTING ####
#       if(BGsampSz > nrow(df.abs2)) BGsampSz <- nrow(df.abs2)/2
# 		  evSetBG <- df.abs2[sample(nrow(df.abs2), BGsampSz , replace = FALSE, prob = NULL),]
# 		   # get the other portion for the training set
# 		  TrBGsamps <- attr(evSetBG, "row.names") #get row.names as integers
# 		  trSetBG <-  df.abs2[-TrBGsamps,]  #get everything that isn't in TrBGsamps
# 		   # join em, clean up
# 		  trSet <- rbind(trSet, trSetBG)
# 		  trSet[,group$colNm] <- factor(trSet[,group$colNm])
# 		  evSet[[i]] <- rbind(evSet[[i]], evSetBG)
# 		  
# 		  ssVec <- sampSizeVec[!names(sampSizeVec) == group$vals[[i]]]
# 		  # re-calc pseudo-absence samples to match input training samples
# 		  ssVec["pseu-a"] <- sum(ssVec[!names(ssVec) %in% "pseu-a"])
# 		  
# 		  rm(trSelStr, evSelStr, trSetBG, evSetBG, TrBGsamps, BGsampSz )
# 
# 		  trRes[[i]] <- foreach(ntree = rep(treeSubs,numCores), .combine = randomForest::combine, 
# 		                        .packages = 'randomForest', .multicombine = TRUE) %dopar%
#           		    		        randomForest(trSet[,indVarCols],y=trSet[,depVarCol],
#           		                             importance=TRUE,mtry=mtry,ntree = ntree,
#           		                             strata = trSet[,group$colNm], sampsize = ssVec, replace = TRUE
# 		  )
# 
# 		  # run a randomForest predict on the validation data
# 		  evRes[[i]] <- predict(trRes[[i]], evSet[[i]], type="prob")
# 		   # use ROCR to structure the data. Get pres col of evRes (= named "1")
# 		  v.rocr.pred[[i]] <- prediction(evRes[[i]][,"1"],evSet[[i]]$pres)
# 		   # extract the auc for metadata reporting
# 		  v.rocr.auc[[i]] <- performance(v.rocr.pred[[i]], "auc")@y.values[[1]]
# 			cat("finished run", i, "of", length(group$vals), "\n")
# 	}
# 
# 	# restructure validation predictions so ROCR will average the figure
# 	v.rocr.pred.restruct <- v.rocr.pred[[1]]
# 	#send in the rest
# 	for(i in 2:length(v.rocr.pred)){
# 		v.rocr.pred.restruct@predictions[[i]] <- v.rocr.pred[[i]]@predictions[[1]]
# 		v.rocr.pred.restruct@labels[[i]] <- v.rocr.pred[[i]]@labels[[1]]
# 		v.rocr.pred.restruct@cutoffs[[i]] <- v.rocr.pred[[i]]@cutoffs[[1]]
# 		v.rocr.pred.restruct@fp[[i]] <- v.rocr.pred[[i]]@fp[[1]]
# 		v.rocr.pred.restruct@tp[[i]] <- v.rocr.pred[[i]]@tp[[1]]
# 		v.rocr.pred.restruct@tn[[i]] <- v.rocr.pred[[i]]@tn[[1]]
# 		v.rocr.pred.restruct@fn[[i]] <- v.rocr.pred[[i]]@fn[[1]]
# 		v.rocr.pred.restruct@n.pos[[i]] <- v.rocr.pred[[i]]@n.pos[[1]]
# 		v.rocr.pred.restruct@n.neg[[i]] <- v.rocr.pred[[i]]@n.neg[[1]]
# 		v.rocr.pred.restruct@n.pos.pred[[i]] <- v.rocr.pred[[i]]@n.pos.pred[[1]]
# 		v.rocr.pred.restruct@n.neg.pred[[i]] <- v.rocr.pred[[i]]@n.neg.pred[[1]]
# 	}
# 
# 	# run a ROC performance with ROCR
# 	v.rocr.rocplot.restruct <- performance(v.rocr.pred.restruct, "tpr","fpr")
# 	# send it to perf for the averaging lines that follow
# 	perf <- v.rocr.rocplot.restruct
#   rm(v.rocr.rocplot.restruct)
# 	## for infinite cutoff, assign maximal finite cutoff + mean difference
# 	## between adjacent cutoff pairs  (this code is from ROCR)
# 	if (length(perf@alpha.values)!=0) perf@alpha.values <-
# 		lapply(perf@alpha.values,
# 			function(x) { isfin <- is.finite(x);
# 				x[is.infinite(x)] <-
# 					(max(x[isfin]) +
# 						mean(abs(x[isfin][-1] -
# 						x[isfin][-length(x[isfin])])));
# 				x[is.nan(x)] <- 0.001; #added by tgh to handle vectors length 2
# 		x})
# 
# 	for (i in 1:length(perf@x.values)) {
# 		ind.bool <- (is.finite(perf@x.values[[i]]) & is.finite(perf@y.values[[i]]))
# 		if (length(perf@alpha.values) > 0)
# 			perf@alpha.values[[i]] <- perf@alpha.values[[i]][ind.bool]
# 		perf@x.values[[i]] <- perf@x.values[[i]][ind.bool]
# 		perf@y.values[[i]] <- perf@y.values[[i]][ind.bool]
# 	}
# 	perf.sampled <- perf
# 
# 	# create a list of cutoffs to interpolate off of
# 	alpha.values <- rev(seq(min(unlist(perf@alpha.values)),
# 							max(unlist(perf@alpha.values)),
# 							length=max(sapply(perf@alpha.values, length))))
# 	# interpolate by cutoff, values for y and x
# 	for (i in 1:length(perf.sampled@y.values)) {
# 		perf.sampled@x.values[[i]] <-
# 		  approxfun(perf@alpha.values[[i]],perf@x.values[[i]],
# 					rule=2, ties=mean)(alpha.values)
# 		perf.sampled@y.values[[i]] <-
# 		  approxfun(perf@alpha.values[[i]], perf@y.values[[i]],
# 					rule=2, ties=mean)(alpha.values)
# 	}
# 
# 	## compute average curve
# 	perf.avg <- perf.sampled
# 	perf.avg@x.values <- list(rowMeans( data.frame( perf.avg@x.values)))
# 	perf.avg@y.values <- list(rowMeans( data.frame( perf.avg@y.values)))
# 	perf.avg@alpha.values <- list( alpha.values )
# 
# 	for(i in 1:length(group$vals)){
# 	  ### get threshold
# 	  # get MTP: minimum training presence (minimum votes recieved [probability]
# 	  # for any training point)
# 	  allVotesPrespts <- trRes[[i]]$votes[,"1"][trRes[[i]]$y == 1]
# 	  MTP <- min(allVotesPrespts/numCores)
#     # calculations fail if MTP = 0 so if it does, fall back to maxSSS
# 	  if(MTP == 0) {
# 	    # max sensitivity plus specificity (maxSSS per Liu et al 2016)
# 	    # create the prediction object for ROCR. Get pres col from y, prediction from votes (=named "1")
# 	    pred <- prediction(trRes[[i]]$votes[,"1"],trRes[[i]]$y)
# 	    sens <- performance(pred,"sens")
# 	    spec <- performance(pred,"spec")
# 	    sss <- data.frame(cutSens = unlist(sens@x.values),sens = unlist(sens@y.values),
# 	                      cutSpec = unlist(spec@x.values), spec = unlist(spec@y.values))
# 	    sss$sss <- with(sss, sens + spec)
# 	    maxSSS <- sss[which.max(sss$sss),"cutSens"]/numCores
# 	    cutval.rf <- c(1-maxSSS, maxSSS)
# 	    names(cutval.rf) <- c("0","1")
# 	  } else {
# 	    cutval.rf <- c(1-MTP, MTP)
# 	    names(cutval.rf) <- c("0","1")
# 	  }
# 
# 	  #apply the cutoff to the validation data
# 		v.rf.pred.cut <- predict(trRes[[i]], evSet[[i]],type="response", cutoff=cutval.rf)
# 		#make the confusion matrix
# 		v.y[[i]] <- table(observed = evSet[[i]][,"pres"],
# 			predicted = v.rf.pred.cut)
# 		#add estimated accuracy measures
# 		v.y[[i]] <- cbind(v.y[[i]],
# 			"accuracy" = c(v.y[[i]][1,1]/sum(v.y[[i]][1,]), v.y[[i]][2,2]/sum(v.y[[i]][2,])))
# 		#add row, col names
# 		rownames(v.y[[i]])[rownames(v.y[[i]]) == "0"] <- "background/abs"
# 		rownames(v.y[[i]])[rownames(v.y[[i]]) == "1"] <- "known pres"
# 		colnames(v.y[[i]])[colnames(v.y[[i]]) == "0"] <- "pred. abs"
# 		colnames(v.y[[i]])[colnames(v.y[[i]]) == "1"] <- "pred. pres"
# 		print(v.y[[i]])
# 		#Generate kappa statistics for the confusion matrices
# 		v.kappa[[i]] <- Kappa(v.y[[i]][1:2,1:2])
# 		#True Skill Statistic
# 		v.tss[[i]] <- v.y[[i]][2,3] + v.y[[i]][1,3] - 1
# 		#Overall Accuracy
# 		v.OvAc[[i]] <- (v.y[[i]][[1,1]]+v.y[[i]][[2,2]])/sum(v.y[[i]][,1:2])
# 		### importance measures ###
# 		#count the number of variables
# 		n.var <- nrow(trRes[[i]]$importance)
# 		#get the importance measures (don't get GINI coeff - see Strobl et al. 2006)
# 		imp <- importance(trRes[[i]], class = NULL, scale = TRUE, type = NULL)
# 		imp <- imp[,"MeanDecreaseAccuracy"]
# 		#get number of variables used in each forest
# 		used <- varUsed(trRes[[i]])
# 		names(used) <- names(imp)
# 		t.importance[[i]] <- data.frame("meanDecreaseAcc" = imp,
# 									"timesUsed" = used )
# 	} #close loop
# 
# 	#housecleaning
# 	rm(trSet, evSet)
# 
# 	#average relevant validation/summary stats
# 	# Kappa - wieghted, then unweighted
# 	K.w <- unlist(v.kappa, recursive=TRUE)[grep("Weighted.value",
# 						names(unlist(v.kappa, recursive=TRUE)))]
# 	Kappa.w.summ <- data.frame("mean"=mean(K.w), "sd"=sd(K.w),"sem"= sd(K.w)/sqrt(length(K.w)))
# 	K.unw <- unlist(v.kappa, recursive=TRUE)[grep("Unweighted.value",
# 						names(unlist(v.kappa, recursive=TRUE)))]
# 	Kappa.unw.summ <- data.frame("mean"=mean(K.unw), "sd"=sd(K.unw),"sem"= sd(K.unw)/sqrt(length(K.unw)))
# 	#AUC - area under the curve
# 	auc <- unlist(v.rocr.auc)
# 	auc.summ <- data.frame("mean"=mean(auc), "sd"=sd(auc),"sem"= sd(auc)/sqrt(length(auc)))
# 	#TSS - True skill statistic
# 	tss <- unlist(v.tss) 
# 	tss.summ <- data.frame("mean"=mean(tss), "sd"=sd(tss),"sem"= sd(tss)/sqrt(length(tss)))
# 	#Overall Accuracy
# 	OvAc <- unlist(v.OvAc)
# 	OvAc.summ <- data.frame("mean"=mean(OvAc), "sd"=sd(OvAc),"sem"= sd(OvAc)/sqrt(length(OvAc)))
# 	#Specificity and Sensitivity
# 	v.y.flat <- abind(v.y,along=1)  #collapsed confusion matrices
# 	v.y.flat.sp <- v.y.flat[rownames(v.y.flat)=="background/abs",]
# 	v.y.flat.sp <- as.data.frame(v.y.flat.sp, row.names = 1:length(v.y.flat.sp[,1]))
# 	specif <- v.y.flat.sp[,"pred. abs"]/(v.y.flat.sp[,"pred. abs"] + v.y.flat.sp[,"pred. pres"])   #specificity
# 	specif.summ <- data.frame("mean"=mean(specif), "sd"=sd(specif),"sem"= sd(specif)/sqrt(length(specif)))
# 	v.y.flat.sn <- v.y.flat[rownames(v.y.flat)=="known pres",]
# 	v.y.flat.sn <- as.data.frame(v.y.flat.sn, row.names = 1:length(v.y.flat.sn[,1]))
# 	sensit <- v.y.flat.sn[,"pred. pres"]/(v.y.flat.sn[,"pred. pres"] + v.y.flat.sn[,"pred. abs"])    #sensitivity
# 	sensit.summ <- data.frame("mean"=mean(sensit), "sd"=sd(sensit),"sem"= sd(sensit)/sqrt(length(sensit)))
# 
# 	summ.table <- data.frame(Name=c("Weighted Kappa", "Unweighted Kappa", "AUC",
# 									"TSS", "Overall Accuracy", "Specificity",
# 									"Sensitivity"),
# 							 Mean=c(Kappa.w.summ$mean, Kappa.unw.summ$mean,auc.summ$mean,
# 									tss.summ$mean, OvAc.summ$mean, specif.summ$mean,
# 									sensit.summ$mean),
# 							 SD=c(Kappa.w.summ$sd, Kappa.unw.summ$sd,auc.summ$sd,
# 									tss.summ$sd, OvAc.summ$sd, specif.summ$sd,
# 									sensit.summ$sd),
# 							 SEM=c(Kappa.w.summ$sem, Kappa.unw.summ$sem,auc.summ$sem,
# 									tss.summ$sem, OvAc.summ$sem, specif.summ$sem,
# 									sensit.summ$sem))
# 	summ.table
# } else {
# 	cat("Only one polygon, can't do validation", "\n")
# 	cutval <- NA
# }
# 
# # increase the number of trees for the full model
# ntrees <- 2000
# treeSubs <- ntrees/numCores
# 
# ####
# #   run the full model ----
# ####
# cat("... creating full model \n")
# 
# rf.full <- foreach(ntree = rep(treeSubs,numCores), .combine = randomForest::combine, 
#                     .packages = 'randomForest', .multicombine = TRUE) %dopar% {
#                         randomForest(df.full[,indVarCols],
#                               y=df.full[,depVarCol],
#                               importance=TRUE,
#                               ntree=ntree,
#                               mtry=mtry,
#                               strata = df.full[,group$colNm],
#                               sampsize = sampSizeVec, replace = TRUE,
#                               norm.votes = TRUE)
#                               }
# 
# ####
# # Importance measures ----
# ####
# #get the importance measures (don't get GINI coeff - see Strobl et al. 2006)
# f.imp <- importance(rf.full, class = NULL, scale = TRUE, type = NULL)
# f.imp <- f.imp[,"MeanDecreaseAccuracy"]
# 
# db <- dbConnect(SQLite(),dbname=nm_db_file)  
# # get importance data, set up a data frame
# EnvVars <- data.frame(gridName = names(f.imp), impVal = f.imp, fullName="", stringsAsFactors = FALSE)
# #set the query for the following lookup, note it builds many queries, equal to the number of vars
# SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName COLLATE NOCASE in ('", paste(EnvVars$gridName,sep=", "),
# 					"'); ", sep="")
# #cycle through all select statements, put the results in the df
# for(i in 1:length(EnvVars$gridName)) {
#   try(EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2]))
# }
# ##clean up
# dbDisconnect(db)
# 
# ###
# # partial plot data ----
# ###
# #get the order for the importance charts
# ord <- order(EnvVars$impVal, decreasing = TRUE)[1:length(indVarCols)]
# if(length(ord) > 9){
#   pPlotListLen <- 9
# } else {
#   pPlotListLen <- length(ord)
# }
# 
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
# curvars = names(f.imp[ord])[1:pPlotListLen]
# pPlots <- foreach(i = iter(curvars), .packages = 'randomForest') %dopar% {
#                   do.call("partialPlot", list(x = rf.full, pred.data = ppPreddata[,indVarCols],
#                               x.var = i,
#                               which.class = 1,
#                               plot = FALSE))
# }
# 
# #fill in names
# names(pPlots) <- c(1:pPlotListLen)
# for(i in 1:length(pPlots)){
#   pPlots[[i]]$gridName <- curvars[[i]]
#   pPlots[[i]]$fname <- EnvVars$fullName[ord[i]]
# }
# rm(ppPres, ppAbs, ppPresSamp, ppAbsSamp, ppPreddata)
# 
# stopCluster(cl)
# rm(cl)
# closeAllConnections()

# save the project, return to the original working directory
dir.create(paste0(loc_model, "/", model_species,"/outputs/rdata"), recursive = TRUE, showWarnings = FALSE)
setwd(paste0(loc_model, "/", model_species,"/outputs"))

# don't save fn args/vars
for(i in 1:length(modelrun_meta_data)) assign(names(modelrun_meta_data)[i], modelrun_meta_data[[i]])
ls.save <- ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("begin_step","rdata","prompt","scrpt",
                                                             "run_steps","prompt","fn_args", names(fn_args))]
save(list = ls.save, file = paste0("rdata/xgb_", model_run_name,".Rdata"), envir = environment())

# write model metadata to db
# tblModelResults
# db <- dbConnect(SQLite(),dbname=nm_db_file)  
# tblModelResults <- data.frame(model_run_name = model_run_name, EGT_ID = ElementNames$EGT_ID, table_code = baseName,
#                               internal_comments = model_comments, metadata_comments = metaData_comments,
#                               model_comp_name = model_comp_name, modeller = modeller,
#                               model_start_time = model_start_time, model_end_time = as.character(Sys.time()),
#                               r_version = r_version, repo_head = repo_head, seed = seed)
# dbWriteTable(db, "tblModelResults", tblModelResults, append = T)
# 
# # tblModelResultsVarsUsed
# varImpDB <- data.frame(model_run_name = model_run_name, gridName = tolower(envvar_list), inFinalModel = 0)
# varImpDB <- merge(varImpDB, EnvVars[c("gridName","impVal")], by = "gridName", all.x = T)
# varImpDB$inFinalModel[!is.na(varImpDB$impVal)] <- 1
# dbWriteTable(db, "tblModelResultsVarsUsed", varImpDB, append = T)
# dbDisconnect(db)
# 
# message(paste0("Saved rdata file: '", model_run_name , "'."))
# 
# ### this isn't needed but keeping for a bit in one script in case we decide otherwise
# #serious cleanup
# # if(isNamespaceLoaded("doParallel")) unloadNamespace("doParallel")
# # if(isNamespaceLoaded("foreach")) unloadNamespace("foreach")
# # if(isNamespaceLoaded("iterators")) unloadNamespace("iterators")
# # if(isNamespaceLoaded("parallel")) unloadNamespace("parallel")
# # if(isNamespaceLoaded("RSQLite")) unloadNamespace("RSQLite")
# # if(isNamespaceLoaded("ROCR")) unloadNamespace("ROCR")
# # if(isNamespaceLoaded("vcd")) unloadNamespace("vcd")
# # if(isNamespaceLoaded("abind")) unloadNamespace("abind")
# # if(isNamespaceLoaded("foreign")) unloadNamespace("foreign")
# # if(isNamespaceLoaded("randomForest")) unloadNamespace("randomForest")
