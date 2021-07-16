# File: 5_createMetadata.r
# Purpose: to summarize validation data and other information about the 
# model and write it to a pdf. This pdf should accompany ALL sharing/showing
# of the SDM map.

# For knitr to work, you need MikTex installed. See http://miktex.org/

# load libraries ----
#library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(xtable)
library(knitr)
library(dplyr)
library(sf)
library(RColorBrewer)
library(RSQLite)
library(stringi)  #only used once, could clean up?
#library(tables)

library(raster)
library(tmap)
library(tmaptools)
library(OpenStreetMap)

library(tidyr)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)

library(DBI)

### find and load model data ----
#setwd(loc_model)
dir.create(file.path(loc_model, model_species, "outputs","metadata"), recursive = TRUE, showWarnings = FALSE)
setwd(file.path(loc_model, model_species, "outputs"))

model_run_name <- modelrun_meta_data$model_run_name

load(file.path(loc_model, model_species, "outputs","rdata", paste0(modelrun_meta_data$model_run_name,".Rdata")))

## get grank definition for header ----
db <- dbConnect(SQLite(),dbname=nm_db_file) 
SQLquery <- paste0("SELECT rank, rankname FROM lkpRankDefinitions where rank = '",ElementNames$rounded_g_rank,"';", sep="")
grank_desc <- dbGetQuery(db, SQLquery)
dbDisconnect(db)

##
## create table 1, ensemble summary ----
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from lkpAlgorithms;")
ensemble_details <- dbGetQuery(db, statement = sql)
dbDisconnect(db)

ensemble_details <- ensemble_details[ensemble_details$shortCode %in% ensemble_algos,
                                     c("fullName","shortCode","rPackage")]
names(ensemble_details) <- c("Name", "Code","R package")
# ensemble_details is used in knitr file
rm(db, sql)

##
## create table 2, summary of input data ----
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelInputs where model_run_name = '", 
              model_run_name, "';")
inputs <- dbGetQuery(db, statement = sql)
dbDisconnect(db)

# Assume groups and pres inputs are the same among algorithms
# but background inputs vary
summ.table <- data.frame(
  Sample=c("Presence locations (groups)",
        "Subsamples within groups",
        "Total presence inputs",
        paste0("Background inputs - ", inputs$algorithm)),
  Count=c(
    ifelse(inputs$jckn_grp_column[[1]] == "stratum", 
           inputs$feat_count[[1]],
           inputs$feat_grp_count[[1]]
           ),
    inputs$mn_grp_subsamp[[1]],
    inputs$tot_obs_subsamp[[1]],
    paste0(inputs$tot_bkgd_subsamp)
     ))
# summ.table is what gets used in knitr file
rm(db, sql)

##
## create table 3, summary of validation statistics ----
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelResultsValidationStats where model_run_name = '", 
              model_run_name, "';")
vstats <- dbGetQuery(db, statement = sql)
dbDisconnect(db)

metricsToGet <- c("AUC","Sensitivity","Specificity","TSS")
colsToGet <- c("algorithm","metric","metric_mn","metric_sd")
vstats <- vstats[vstats$metric %in% metricsToGet,colsToGet]
names(vstats) <- c("algorithm","metric","mean","SD")
vstats$evalOut <- paste0(round(vstats$mean,2), "(",round(vstats$SD,2), ")")
vstats.s <- vstats[,c("algorithm","metric","evalOut")]

# convert to wide format
vstats.w <- spread(vstats.s, metric, evalOut)
names(vstats.w) <- c("alg","AUC","Sens","Spec","TSS")

# vstats.w is what gets used in knitr file
rm(db, sql, metricsToGet, colsToGet, vstats.s)

##
## create data for thermometer figure ----
summaryTSS <- mean(vstats[vstats$metric == "TSS", "mean"])
rm(vstats)

##
## create table with all other modeling input settings ----
#  this is placed at the end in Appendix 2
#first get envars counts
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelResultsVarsUsed where model_run_name = '", 
              model_run_name, "';")
varsUsedStats <- dbGetQuery(db, statement = sql)
dbDisconnect(db)
varsUsedStats <- varsUsedStats[varsUsedStats$inFinalModel == 1,]
vuStats <- aggregate(varsUsedStats$inFinalModel, by = list(varsUsedStats$algorithm), sum)
names(vuStats) <- c("algo","value")

vuStats <- cbind("Name"= "number of predictors used", vuStats)
vuStatsList <- split(vuStats, f = vuStats$algo)
vuStatsList <- lapply(vuStatsList, FUN = function(x) x[,!names(x)=="algo"])

for(algo in names(vuStatsList)){
  if(algo == "me"){
    algodat <- data.frame(Name = c("linear feature type used",
                                 "product feature type used",
                                 "quadratic feature type used",
                                 "hinge feature type used"),
               value = c("yes","yes","yes","yes"))
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
  }
  if(algo == "xgb"){
    algodat <- data.frame(Name = c(
                                  "iterations",
                                  "eta",
                                  "max depth",
                                  "gamma",
                                  "colsample by tree",
                                  "min child weight",
                                  "subsample",
                                  "objective"),
                         value = c(
                                   xgb.full$niter,
                                   xgb.full$params$eta,
                                   xgb.full$params$max_depth,
                                   xgb.full$params$gamma,
                                   xgb.full$params$colsample_bytree,
                                   xgb.full$params$min_child_weight,
                                   xgb.full$params$subsample,
                                   xgb.full$params$objective
                                   ))
    #paste0(deparse(xgb.full$call), collapse = ""),
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
  }
  if(algo == "rf"){
    algodat <- data.frame(Name = c(
                                "mtry",
                                "number of trees",
                                "type of trees"
                              ),
                        value = c(
                                rf.full$mtry,
                                rf.full$ntree,
                                rf.full$type
                              )
    )
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
  }
  if(algo == "gam"){
    algodat <- data.frame(Name = c(
      "method",
      "family",
      "validation method"
    ),
    value = c(
      "gamSpline",
      "binomial (logit)",
      "Leave one out by group (LGOCV)"
    )
    )
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
  }
  if(algo == "glm"){
    algodat <- data.frame(Name = c(
      "method",
      "model type",
      "family"
    ),
    value = c(
      glmFit1$method,
      glmFit1$modelType,
      paste0(glmFit1$finalModel$family$family,":", glmFit1$finalModel$family$link)
    )
    )
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
  }
  if(algo == "mars"){
    algodat <- data.frame(Name = c(
      "method",
      "model type",
      "nprune",
      "degree"
    ),
    value = c(
      marsFit1$method,
      marsFit1$modelType,
      marsFit1$bestTune$nprune,
      marsFit1$bestTune$degree
    )
    )
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], algodat)
  }
}



attr(vuStatsList, "subheadings") <- paste0("\\textcolor{",
                                           names(vuStatsList), "Color}{", 
                                           "Algorithm = ",  names(vuStatsList),"}")

# vuStatsList is what gets used in knitr file
rm(db, sql, varsUsedStats, vuStats, algodat, algo)

##
## build ROC plot ----
#### this is all in the rnw, possibly change to ggplot, then build it here and print it there

## build lookup for line details and colors ----
lineColors <- brewer.pal(6, "Dark2")[1:length(ensemble_algos)]

figSpecs <- data.frame(algos = ensemble_algos,
                       col = lineColors,
                       lwd = c(rep(2,length(ensemble_algos))),
                       lty = c(rep(1,length(ensemble_algos))),
                       htmlCol = sub("#","",lineColors),
                       stringsAsFactors = FALSE)

# set the knitr hook to create color definitions within the rnw file
# based on the algorithm color definitions
defs <- vector("list",nrow(figSpecs))
for(i in 1:nrow(figSpecs)){
  defs[[i]] <- paste0("\\\\definecolor{",figSpecs$algos[[i]],"Color}{HTML}{",figSpecs$htmlCol[[i]],"}")
} 

knit_hooks$set(document = function(x) {
  sub('%CustomColorDefsHere', paste0(unlist(defs), "\n", collapse = ""), x)
})

##
## build importance plot ----

# get the vars used for each
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelResultsVarsUsed where model_run_name = '", 
              model_run_name, "';")
varsImp <- dbGetQuery(db, statement = sql)
# get full names
sql <- "SELECT gridName, fullName FROM lkpEnvVars"
varNms <- dbGetQuery(db, statement = sql)
dbDisconnect(db)

# remove vars not used by any algo
varsImp <- varsImp[varsImp$inFinalModel == 1,]

# merge in full name, reduce cols
varsImp.full <- merge(varsImp, varNms)
varsImp <- varsImp.full[,c("algorithm","fullName","impVal")]

# do it in long format ...
# standardize to 0-1 then sort by mean importance (using factors)
for(algo in ensemble_algos){
  algoLocs <- varsImp$algorithm == algo
  varsImp[algoLocs,"impVal"] <- varsImp[algoLocs, "impVal"]/max(varsImp[algoLocs,"impVal"])
}

#try sorting with zeros added
varsSorted <- varsImp %>%
  group_by(fullName) %>%
  summarise_at(vars(impVal), function(x) { sum(x)/length(ensemble_algos)}) %>%
  arrange(impVal)

# to factors for correct ordering in ggplot
varsSorted$fullName <- factor(varsSorted$fullName, levels = varsSorted$fullName)
varsImp$fullName <- factor(varsImp$fullName, levels = varsSorted$fullName)
varsImp <- varsImp[order(as.integer(varsImp$fullName)),]

#use same colors as ROC plot
scaleVec <- figSpecs$col
names(scaleVec) <- figSpecs$algos

# build the figure
# impPlot <- ggplot(data = varsImp) + 
#   xlab(bquote(atop("lower" %->% "greater", "importance"))) + 
#   theme(axis.title.y = element_blank(),
#         text = element_text(size=8),
#         legend.position = c(0.85,0.15)) + 
#   geom_point(aes(x = impVal, y = fullName, color = algorithm)) + 
#   geom_path(aes(x = impVal, y = fullName, color = algorithm, group = algorithm)) + 
#   scale_color_manual(values = scaleVec)

# with mean as thick grey line, need to plot it first so its on the bottom
impPlot <- ggplot(data = varsSorted) + 
  xlab(bquote(atop("lower" %->% "greater", "importance"))) + 
  geom_path(data = varsSorted, aes(x=impVal, y=fullName, group = 1),
            color="grey60", size = 1.5) + 
  geom_point(data = varsImp, 
             aes(x = impVal, y = fullName, color = algorithm)) + 
  geom_path(data = varsImp, 
            aes(x = impVal, y = fullName, color = algorithm, group = algorithm)) + 
  scale_color_manual(values = scaleVec) + 
  theme_classic() + 
  theme(axis.title.y = element_blank(),
        text = element_text(size=8),
        legend.position = c(0.85,0.15)) + 
  geom_hline(yintercept = 1:nrow(varsSorted), 
             linetype = "18", color = "grey40", size = 0.1)

##
## build partial plots ----
# find the longest of our set of algos
# this lines are for the slim chance we have less than 9 vars total. Not likely now. 

maxVars <- 0
for(algo in ensemble_algos){
  objName <- paste0(algo, ".pPlots")
  if(algo == "rf"){
    numVars <- length(get(objName))
  } else {
    numVars <- length(get(objName)$fullNames)  
  }
  if(numVars > maxVars) {
    maxVars <- numVars
    mostPplots <- objName
    mostPplotsAlgo <- algo
  }
}
# create how many?
if(maxVars < 9){
  numPPl <- maxVars
} else {
  numPPl <- 9
}
rm(maxVars, objName, numVars)

# get the order used in importance plot
pplotVars <- varsSorted[order(varsSorted$impVal, decreasing = TRUE), "fullName"]
pplotVars <- pplotVars[1:numPPl,]

# using info from importance plot, which of the top 9 envars is in the most models?
# easiest way to get the most complete legend
topVars <- varsImp[varsImp$fullName %in% pplotVars$fullName,]
varCount <- aggregate(topVars$fullName, list(topVars$fullName), length)
varCount$order <- nrow(varCount):1
maxCount <- max(varCount$x)
varCount <- varCount[order(varCount$order),]
plotForLeg <- min(grep(maxCount, varCount$x))
rm(topVars, varCount, maxCount)

# make a list to fill with grobs
grobList <- vector("list",numPPl)
names(grobList) <- 1:numPPl

# get the location of the longest set
if(mostPplotsAlgo == "rf"){
  elist <- unlist(lapply(get(mostPplots), FUN = function(x) x$fname))  
} else {
  elist <- NA
}

for (plotpi in 1:numPPl){
  evar <- pplotVars$fullName[[plotpi]]

  #get gridname
  grdName <- unique(varsImp.full[varsImp.full$fullName == evar, "gridName"])
  
  #dens data
  df.full <- rbind(df.in, df.abs)
  densdat <- data.frame(x = df.full[,grdName], pres = df.full[,"pres"])

  # pplot data
  # do rf only if there are data
  if(exists("rf.pPlots")){
    rf.elist <- unlist(lapply(rf.pPlots, FUN = function(x) x$fname))
    rfLoc <- match(evar, rf.elist)
  } else {
    rfLoc <- NA
  }
  if(exists("rf.pPlots") & !is.na(rfLoc)){
    grdFullName <- rf.pPlots[[rfLoc]]$fname
    dat <- data.frame(x = rf.pPlots[[rfLoc]]$x, y = rf.pPlots[[rfLoc]]$y)
    dat <- cbind(dat, algo = "rf")
    #standardize 0-1
    dat$y <- (dat$y - min(dat$y))/(max(dat$y)-min(dat$y))    
  } else {
    dat <- data.frame(x = numeric(), y = numeric(), algo = character())
  }
  
  # check and use xgb if there are data
  if(exists("xgb.pPlots")){
    if(grdName %in% dimnames(xgb.pPlots$data)[[2]]){
      xgbdat <- data.frame(xgb.pPlots$data)
      xgbresp <- data.frame(xgb.pPlots$shap_contrib)
      xgbdat.b <- data.frame(x = xgbdat[,grdName], y = xgbresp[,grdName], algo = "xgb")
      # all zeros means xgb dropped it in final model; skip for partial plot
      # so if all responses don't equal zero, get the data
      if(all(xgbdat.b$y != 0)){
        #standardize 0-1
        xgbdat.b$y <- (xgbdat.b$y - min(xgbdat.b$y))/(max(xgbdat.b$y)-min(xgbdat.b$y))
        #order, ascending
        xgbdat.b <- xgbdat.b[order(xgbdat.b$x),]
        #smooth it
        #dat <- rbind(dat, xgbdat.b)
        dat <- rbind(dat, data.frame(supsmu(xgbdat.b$x, xgbdat.b$y), algo = "xgb"))
        #rm(xgbdat, xgbresp, xgbdat.b)
      }
    }
  }
  
  # check and use me if there are data
  if(exists("me.pPlots")){
    melist <- unlist(lapply(me.pPlots, FUN = function(x) x$gridName))
    if(grdName %in% melist){
      grdLoc <- match(grdName, melist)
      medat <- data.frame(x = me.pPlots[[grdLoc]]$x, y = me.pPlots[[grdLoc]]$y)
      medat <- cbind(medat, algo = "me")
      #standardize 0-1
      medat$y <- (medat$y - min(medat$y))/(max(medat$y)-min(medat$y))
      dat <- rbind(dat, medat)
      rm(grdLoc)
    }
  }
  
  # check and use gam if there are data
  if(exists("gam.pPlots")){
    dfPointer <- unlist(lapply(gam.pPlots, FUN = function(x){grdName %in% names(x)}))
    if(TRUE %in% dfPointer){
      gamdat <- gam.pPlots[dfPointer][[1]]
      names(gamdat) <- c("x","y")
      gamdat <- cbind(gamdat, algo = "gam")
      #standardize 0-1
      gamdat$y <- (gamdat$y - min(gamdat$y))/(max(gamdat$y)-min(gamdat$y))
      dat <- rbind(dat, gamdat)
      rm(dfPointer)
    }
  }
  # check and use glm if there are data
  if(exists("glm.pPlots")){
    dfPointer <- unlist(lapply(glm.pPlots, FUN = function(x){grdName %in% names(x)}))
    if(TRUE %in% dfPointer){
      glmdat <- glm.pPlots[dfPointer][[1]]
      names(glmdat) <- c("x","y")
      glmdat <- cbind(glmdat, algo = "glm")
      #standardize 0-1
      glmdat$y <- (glmdat$y - min(glmdat$y))/(max(glmdat$y)-min(glmdat$y))
      dat <- rbind(dat, glmdat)
      rm(dfPointer)
    }
  }
  # check and use mars if there are data
  if(exists("mars.pPlots")){
    dfPointer <- unlist(lapply(mars.pPlots, FUN = function(x){grdName %in% names(x)}))
    if(TRUE %in% dfPointer){
      marsdat <- mars.pPlots[dfPointer][[1]]
      names(marsdat) <- c("x","y")
      marsdat <- cbind(marsdat, algo = "mars")
      #standardize 0-1
      marsdat$y <- (marsdat$y - min(marsdat$y))/(max(marsdat$y)-min(marsdat$y))
      dat <- rbind(dat, marsdat)
      rm(dfPointer)
    }
  }
  
 pplot <- ggplot(data = dat, aes(x=x, y=y, color = algo)) + 
    geom_line(size = 1) +
    xlab(evar) + 
    scale_x_continuous(limits = c(min(dat$x), max(dat$x)), 
                       expand = expansion(mult = c(0.05))) +
    theme_classic() +
    theme(axis.title.y = element_blank(), legend.position = "none",
          plot.margin = margin(t = 1, r = 5, b = 5, l = 5, unit = "pt"),
          text = element_text(size=8),
          panel.border = element_rect(colour = "black", fill=NA, size=0.25)
          ) + 
    scale_color_manual(values = scaleVec)

  # create the density plot
  densplot <- ggplot(data = densdat, aes(x = x, color = factor(pres, labels = c("background","presence")))) + 
    geom_density(size = 0.5, show.legend = FALSE) + 
    # scale_x_continuous(limits = c(min(densdat$x), max(densdat$x)),
    #                    expand = expansion(mult = c(0.05)),
    #                    breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) + 
    theme_classic() + 
    theme(axis.title.y = element_blank(), legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          plot.margin = margin(t = 2, r = 0, b = 1, l = 0, unit = "pt")) +
    scale_color_manual(values=c("grey60", "black")) 
    #theme_void()

  # now do the layout
  gdens <- ggplotGrob(densplot)
  gpplt <- ggplotGrob(pplot)
  panel_id <- gpplt$layout[gpplt$layout$name == "panel",c("t","l")]
  gpplt <- gtable_add_rows(gpplt, unit(0.25,"null"), 0)
  gpplt <- gtable_add_grob(gpplt, gdens,
                       t = 1, l = panel_id$l)
  #grid.newpage()
  #grid.draw(gpplt)
  grobList[[plotpi]] <- gpplt

  # if on loop with most lines, extract legends
  if(plotpi == plotForLeg){
    # Function to extract legend
    g_legend <- function(a.gplot){ 
      tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
      legend <- tmp$grobs[[leg]] 
      legend
    } 
    # extract them
    legPlot1 <- pplot + 
      labs(color = "Algorithm") +
      theme(legend.position = "bottom",
            legend.margin=margin(t=0, r=0, b=0, l=0, unit="pt"),
            text = element_text(size=14))
    legend1 <- g_legend(legPlot1) 
    
    legPlot2 <- densplot + 
      geom_freqpoly(binwidth = 1000) + # hack to get lines instead of squares in legend
      #scale_x_continuous() + 
      labs(color = "Density") +
      theme(legend.position = "bottom",
            legend.margin=margin(t=0, r=0, b=0, l=0, unit="pt"),
            text = element_text(size=14))
    legend2 <- g_legend(legPlot2)
  }
    
}

# set up legend grobs
legGb <- arrangeGrob(grobs=list(legend2, legend1), 
                  layout_matrix=rbind(c(1,2)))

# set up full figure
gt <- arrangeGrob(grobs=grobList, 
                  layout_matrix=rbind(c(1,2,3),
                                      c(4,5,6),
                                      c(7,8,9)),
                  left = "Relative Response")

gtl <- gtable_add_rows(gt, unit(0.25, "null"), pos = -1)
gtl <- gtable_add_grob(gtl, legGb, t = 4, l = 2, b = 4, r = 4)

#grid.newpage()
#grid.draw(gtl)


## build the map ----

# get the name of the raster we'll be using
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT raster_for_metadata_figure FROM tblModelResults ",
              "WHERE model_run_name = '", 
              model_run_name, "';")
rasName <- dbGetQuery(db, statement = sql)[[1]]
dbDisconnect(db)

ras <- raster(paste0("model_predictions/", rasName))

studyAreaExtent <- st_read(file.path(loc_model,model_species,"inputs","model_input",paste0(model_run_name, "_studyArea.gpkg")), quiet = TRUE)
referenceBoundaries <- st_read(nm_refBoundaries, quiet = TRUE) # name of state boundaries file

# project to match raster, just in case
studyAreaExtent <- st_transform(studyAreaExtent, as.character(ras@crs))
referenceBoundaries <- st_transform(referenceBoundaries, as.character(ras@crs))

# set up figure
nclr <- 5
clrs <- brewer.pal('Blues',n=nclr)

# figure out size of study area, expand if less than 889km across,
#  which is 1;5,000,000 when figure is 7 inches wide (which it is here)
bbox <- bb(studyAreaExtent)
studyAreaWidth <- bbox$xmax - bbox$xmin
studyAreaHeight <- bbox$ymax - bbox$ymin
if(studyAreaWidth < 889000){
  bbox <- bb(bbox, width = 889000, relative = FALSE)
}
if(studyAreaHeight < 889000){
  bbox <- bb(bbox, height = 889000, relative = FALSE)
}

tmap_options(max.raster = c("plot" = 300000, "view" = 100000))
tmap_mode("plot")
# get the basemap
# for basemap options see http://leaflet-extras.github.io/leaflet-providers/preview/
# for native options provided by read_osm, see ?OpenStreetMap::openmap

## this is Esri.WorldGrayCanvas
#mtype <- 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}.png?'

## this is CartoDB.Positron
mtype <- 'https://a.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png'
basetiles <- read_osm(bbox, type = mtype, ext = 1.1)
# plot it
mapFig <- qtm(basetiles) +
  tm_shape(ras) +
  tm_raster(palette = clrs, title = "modeled suitability",
      labels = c("Low Habitat Suitability", rep(" ", nclr-2), "High Habitat Suitability")) +
  tm_shape(referenceBoundaries) +
  tm_borders(col = "grey", lwd = 1) +
  tm_shape(studyAreaExtent) +
    tm_borders(col = "red", lwd = 2) +
  tm_compass(north = 0, type = "arrow", position = c("left","bottom")) +
  tm_scale_bar()



## Get Program and Data Sources info ----
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)  
SQLquery <- paste("Select lkpModelers.ProgramName, lkpModelers.FullOrganizationName, ",
                  "lkpModelers.City, lkpModelers.State, lkpSpecies.sp_code ",
                  "FROM lkpModelers ", 
                  "INNER JOIN lkpSpecies ON lkpModelers.ModelerID=lkpSpecies.ModelerID ", 
                  "WHERE lkpSpecies.sp_code='", model_species, "'; ", sep="")
sdm.modeler <- dbGetQuery(db, statement = SQLquery)
# NOTE: use column should be populated with 1/0 for sources of data used
SQLquery <- paste("SELECT sp.sp_code, sr.ProgramName, sr.State ",
                  "FROM lkpSpecies as sp ",
                  "INNER JOIN mapDataSourcesToSpp as mp ON mp.EGT_ID=sp.EGT_ID ",
                  "INNER JOIN lkpDataSources as sr ON mp.DataSourcesID=sr.DataSourcesID ",
                  # "WHERE mp.use = 1 ",
                  "AND sp.sp_code ='", model_species, "'; ", sep="")
sdm.dataSources <- dbGetQuery(db, statement = SQLquery)
sdm.dataSources <- sdm.dataSources[order(sdm.dataSources$ProgramName),]

SQLquery <- paste("SELECT model_end_time date, egt_id, metadata_comments comments",
                  " FROM tblModelResults ", 
                  "WHERE model_run_name ='", model_run_name, "'; ", sep="")
sdm.customComments <- dbGetQuery(db, statement = SQLquery)
dbDisconnect(db)
# assume you want the most recently entered comments, if there are multiple entries
if(nrow(sdm.customComments) > 1) {
  sdm.customComments <- sdm.customComments[order(sdm.customComments$date, decreasing = TRUE),]
  sdm.customComments.subset <- sdm.customComments[1,]
} else {
  sdm.customComments.subset <- sdm.customComments
}

## Get threshold table information ----
# get thresholds
db <- dbConnect(SQLite(),dbname=nm_db_file)  
SQLquery <- paste("Select ElemCode, algorithm, dateTime, cutCode, cutValue, 
                  capturedGPs, capturedPolys, capturedPts, prpCapGPs, prpCapPolys, prpCapPts ", 
                  "FROM tblModelResultsCutoffs ", 
                  "WHERE model_run_name ='", model_run_name, "'; ", sep="")
sdm.thresholds <- dbGetQuery(db, statement = SQLquery)

# get info about thresholds
SQLquery <- paste("SELECT cutCode, cutFullName, cutDescription, cutCitationShort, cutCitationFull, sortOrder ", 
                  "FROM lkpThresholdTypes ", 
                  "WHERE cutCode IN (", 
                  toString(sQuote(sdm.thresholds$cutCode)),
                  ");", sep = "")
sdm.thresh.info <- dbGetQuery(db, statement = SQLquery)
dbDisconnect(db)
rm(db)

# merge and sort
sdm.thresh.merge <- merge(sdm.thresholds, sdm.thresh.info)
sdm.thresh.merge <- sdm.thresh.merge[order(sdm.thresh.merge$sortOrder),]
# remove metrics we don't want to display
sdm.thresh.merge <- sdm.thresh.merge[!sdm.thresh.merge$cutCode %in% 
                                       c("FMeasPt01", "MPVP", "MPVG"),]
# extract descriptions of those we are using
thresh.descr <- unique(sdm.thresh.merge[,c("cutCode","cutFullName","cutDescription")])
names(thresh.descr) <- c("Code","Threshold full name","Threshold description")

# define groups (GPs or polys) based on validation
# if(group$JackknType == "polygon"){
#   sdm.thresh.merge$pctCapGPs <- paste0(round(sdm.thresh.merge$prpCapPolys * 100),"(",
#                                        round(sdm.thresh.merge$capturedPolys),")")
# } else {
#   sdm.thresh.merge$pctCapGPs <- paste0(round(sdm.thresh.merge$prpCapGPs * 100),"(",
#                                       round(sdm.thresh.merge$capturedGPs),")")
# }
sdm.thresh.merge$pctCapGPs <- paste0(round(sdm.thresh.merge$prpCapGPs * 100),"(",
                                     round(sdm.thresh.merge$capturedGPs),")")

sdm.thresh.merge$pctCapPts <- round(sdm.thresh.merge$prpCapPts * 100)
# subset and remove metrics we don't want to display
sdm.thresh.merge <- sdm.thresh.merge[,c("cutCode","algorithm","cutValue",
                                        "pctCapGPs","pctCapPts")]
names(sdm.thresh.merge) <- c("Code","algorithm","Value","Groups","Points")
sdm.thresh.list <- split(sdm.thresh.merge, f = sdm.thresh.merge$algorithm)
sdm.thresh.list <- lapply(sdm.thresh.list, FUN = function(x) x[,!names(x)=="algorithm"])

# no colored text
#attr(sdm.thresh.list, "subheadings") <- paste0("Algorithm = ", names(sdm.thresh.list))
# with colored text following lines on figures
attr(sdm.thresh.list, "subheadings") <- paste0("\\textcolor{",
                          names(sdm.thresh.list), "Color}{", 
                          "Algorithm = ", 
                          names(sdm.thresh.list),"}")

# can't get xtable's sanitize functions to work, manually escape % here. 
# attr(sdm.thresh.list, "message") <- paste0(thresh.descr$cutCode, ": ",
#                             gsub("%","\\%",thresh.descr$cutDescription, fixed = TRUE))

sdm.thresh.list.xtbl <- xtableList(sdm.thresh.list, 
                          align = "llrrr",
                          digits=c(0,0,3,0,0))

thresh.descr.xtbl <- xtable(thresh.descr, 
                            align = "lllp{3in}")


# make a url to NatureServe Explorer
NSurl <- paste("http://explorer.natureserve.org/servlet/NatureServe?searchName=",gsub(" ", "+", ElementNames[[1]], fixed=TRUE), sep="")

## get Model Evaluation and Use data ----
db <- dbConnect(SQLite(),dbname=nm_db_file) 
SQLquery <- paste("Select spdata_dataqual, spdata_abs, spdata_eval, envvar_relevance, envvar_align, process_algo, process_sens, process_rigor, process_perform, process_review, products_mapped, products_support, products_repo, iterative, spdata_dataqualNotes, spdata_absNotes, spdata_evalNotes, envvar_relevanceNotes, envvar_alignNotes, process_algoNotes, process_sensNotes, process_rigorNotes, process_performNotes, process_reviewNotes, products_mappedNotes, products_supportNotes, products_repoNotes, iterativeNotes ", 
                  "FROM lkpSpeciesRubric ", 
                  "WHERE sp_code ='", model_species, "'; ", sep="")
sdm.modeluse <- dbGetQuery(db, statement = SQLquery)
dbDisconnect(db)

sdm.modeluse[is.na(sdm.modeluse)] <- " "
sdm.modeluse[sdm.modeluse=="I"] <- "\\cellcolor[HTML]{9AFF99} Ideal"
sdm.modeluse[sdm.modeluse=="A"] <- "\\cellcolor[HTML]{FFFFC7} Acceptable"
sdm.modeluse[sdm.modeluse=="C"] <- "\\cellcolor[HTML]{FD6864} Interpret with Caution"

## Get env. var lookup table ----
db <- dbConnect(SQLite(),dbname=nm_db_file) 
SQLquery <- paste0("SELECT gridName g from tblModelResultsVarsUsed where model_run_name = '",
                   model_run_name, "' and inFinalModel = 1;")
var_names <- dbGetQuery(db, SQLquery)$g
SQLquery <- paste("SELECT fullName, description ",
                  "FROM lkpEnvVars ",
                  "WHERE gridName COLLATE NOCASE IN (",
                  toString(sQuote(var_names)),
                  ") ORDER BY fullName;", sep = "")
sdm.var.info <- dbGetQuery(db, statement = SQLquery)
names(sdm.var.info) <- c("Variable Name","Variable Description")
dbDisconnect(db)

# escape symbols for latex
ls <- c("&","%","$","#","_","{","}")
for (l in ls) {
  sdm.var.info$`Variable Name` <- gsub(l, paste0("\\",l), sdm.var.info$`Variable Name`, fixed = T)
  sdm.var.info$`Variable Description` <- gsub(l, paste0("\\",l), sdm.var.info$`Variable Description`, fixed = T)
}
# replace degree symbols
for (l in 1:length(sdm.var.info$`Variable Description`)) {
  new.desc <- stri_escape_unicode(sdm.var.info$`Variable Description`[l])
  if (grepl("\\u00b0",new.desc, fixed = T)) 
    sdm.var.info$`Variable Description`[l] <- gsub("\\u00b0", "$^\\circ$", new.desc, fixed = T)
}
# put descriptions in parboxes for multiple lines
sdm.var.info$`Variable Description` <- paste0("\\parbox{20cm}{",sdm.var.info$`Variable Description`,"}")

# fix greater than and less than symbol in rubric table
sdm.modeluse$process_performNotes <- gsub(">=","$\\\\geq$", sdm.modeluse$process_performNotes)
sdm.modeluse$process_performNotes <- gsub("<","$<$ ", sdm.modeluse$process_performNotes)


## Run knitr and create metadata ----

setwd("metadata")
# knit2pdf errors for some reason...just knit then call directly
knit2pdf(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))

# delete .txt, .log etc if pdf is created successfully.
# fn_ext <- c(".log",".aux",".out")
# if (file.exists(paste(model_run_name, ".pdf",sep=""))){
#   #setInternet2(TRUE)
#   #download.file(fileURL ,destfile,method="auto")
#   for(i in 1:NROW(fn_ext)){
#     fn <- paste(model_run_name, fn_ext[i],sep="")
#     if (file.exists(fn)){ 
#       file.remove(fn)
#     }
#   }
# }

## write up output info to tracking DB  ---
## If metadata pdf creation successful, then full model run complete,
## so this is an appropriate time to populate Tracking DB
# get tracking DB connection info
trackerDsnInfo <- here("_data","databases", "hsm_tracker_connection_string_short.dsn")


## get model cycle info from the tracking db
cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))
# get model cycle we are on
sql <- paste0("SELECT v2_Elements.ID, v2_Elements.Taxonomic_Group, V2_Elements.Location_Use_Class, ",
              "v2_Cutecodes.cutecode, ",
              "v2_ModelCycle.ID, v2_ModelCycle.model_cycle ",
              "FROM (v2_Elements INNER JOIN v2_Cutecodes ON v2_Elements.ID = v2_Cutecodes.Elements_ID) ",
              "INNER JOIN v2_ModelCycle ON v2_Elements.ID = v2_ModelCycle.Elements_ID ",
              "WHERE (((v2_Cutecodes.cutecode)= '", ElementNames$Code, "'));")

model_cycle <- dbGetQuery(cn, sql)
names(model_cycle) <- c("Elements_ID","taxonomic_group","luc","cutecode","model_cycle_ID", "model_cycle")
dbDisconnect(cn)


# get most recent cycle (last row after sorting)
model_cycle <- model_cycle[order(model_cycle$model_cycle),]
model_cycle <- model_cycle[nrow(model_cycle),]

outputsDat <- model_cycle[,c("model_cycle_ID"), drop = FALSE]
names(outputsDat) <- "model_cycle_id"
outputsDat$model_run_name <- model_run_name
outputsDat$path_to_output <- file.path(loc_model, model_species,"outputs","model_predictions")
outputsDat$modeling_machine <- model_comp_name
outputsDat$comment <- NA
outputsDat$ensemble_code <- NA

# duplicate rows based on number of algorithms
repTimes <- length(ensemble_algos)
outputsDat <- outputsDat[rep(1, repTimes),]

outputsDat$algorithm_code <- ensemble_algos
outputsDat$output_file_name <- paste0(model_run_name,"_",ensemble_algos,".tif")  
# decision: don't put up info about the mean suitabilities ensemble. Only use it for the metadata map

outputsDat <- outputsDat[,c("model_cycle_id","model_run_name","algorithm_code","output_file_name",
                            "path_to_output","modeling_machine","comment")]

# push up the data
cn <- dbConnect(odbc::odbc(), .connection_string = readChar(trackerDsnInfo, file.info(trackerDsnInfo)$size))

# are these data already up there? if so, delete and re-upload
# base it on file name
sql <- paste0("SELECT * from v2_Outputs where output_file_name IN (",
              toString(sQuote(outputsDat$output_file_name, q = FALSE)), ");")

datUpThere <- dbGetQuery(cn, sql)

if(nrow(datUpThere) > 0){
  sql <- paste0("DELETE from v2_Outputs where ID IN (",
                toString(datUpThere$ID), ");")
  dbExecute(cn, sql)
}

# now upload the rows
dbWriteTable(cn,"v2_Outputs", outputsDat, append = TRUE, row.names = FALSE)
dbDisconnect(cn)
rm(cn)

## clean up ----
#dbDisconnect(db)
options(op)
