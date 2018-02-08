# File: run_SDM.r
# Purpose: provide a function interface for running the SDM
#   process in this repository. 

# Usage: For a full, new model run, provide all paths/file names to arguments 'loc_scripts' THROUGH 'modeller'.

# If picking up from a previous run, provide the full file location to the saved rdata file (no file extension)
# holding these paths. For new runs, this file is automatically saved as "runSDM_paths" in the 
# 'loc_RDataOut' folder of the original run.

# Optional arguments for all runs include:
# 1. begin_step: specify as the prefix of the step to begin with: one of ("1","2","3","4","4b","4c","5"). Defaults to "1".
# 2. model_rdata: when beginning after step 3, you need to specify the Rdata file name (no file extension) for the model previously created. 
# Will be looked for in the 'loc_RDataOut' folder
# 3. prompt: if TRUE, the function will stop after each script, and ask if you want to continue. Defaults to FALSE.

run_SDM <- function(
  loc_scripts, 
  loc_spPoly,
  nm_db_file,
  loc_bkgPts, 
  nm_bkgPts,
  loc_envVars,
  loc_otherSpatial,
  nm_refBoundaries,
  nm_studyAreaExtent,
  loc_spPts,
  loc_RDataOut,
  loc_outRas,
  loc_outMetadata,
  model_comments = "",
  metaData_comments = "",
  modeller = NULL,
  begin_step = "1",
  model_rdata = NULL,
  add_vars = NULL,
  remove_vars = NULL,
  prompt = FALSE
) {
  
  if ((hasArg(add_vars) | hasArg(remove_vars)) & !begin_step %in% c("1","2")) 
    stop("Need to begin on step 1 or 2 if adding or removing variables.")
  
  # check if new or picked-up run
  if (begin_step != "1") {
    if (begin_step %in% c("2","3")) {
      message("Loading most recent saved runSDM settings...")
      load(paste0(loc_RDataOut, "/runSDM_paths.Rdata"))
    } else {
      if (is.null(model_rdata) | is.null(loc_RDataOut)) {
        stop("Must provide both 'loc_RDataOut' and 'model_rdata' for continuing a model run.")
      } else {
        load(paste0(loc_RDataOut, "/runSDM_paths.Rdata"))
      }
    }
    # re-write modified variables
    for (na in names(fn_args)) {
      if (eval(parse(text = paste0("hasArg(",na,")")))) fn_args[[na]] <- eval(parse(text=na))
    }
    rm(na)
    
  } else {
    fn_args <- list(
      loc_scripts = loc_scripts, 
      loc_spPoly = loc_spPoly,
      nm_db_file = nm_db_file,
      loc_bkgPts = loc_bkgPts, 
      nm_bkgPts = nm_bkgPts,
      loc_envVars = loc_envVars,
      loc_otherSpatial = loc_otherSpatial,
      nm_refBoundaries = nm_refBoundaries,
      nm_studyAreaExtent = nm_studyAreaExtent,
      loc_spPts = loc_spPts,
      loc_RDataOut = loc_RDataOut,
      loc_outRas = loc_outRas,
      loc_outMetadata = loc_outMetadata,
      model_comments = model_comments,
      metaData_comments = metaData_comments,
      add_vars = add_vars,
      remove_vars = remove_vars,
      modeller = modeller)
  }
  
  # add comments for added/excluded vars
  if (!hasArg(model_comments)) model_comments <- fn_args$model_comments
  if (!hasArg(metaData_comments)) metaData_comments <- fn_args$metaData_comments
  if (!is.null(add_vars)) {
    model_comments <- paste0("Non-standard variables (", paste(add_vars, collapse = ", "), ") were included in this model. ", model_comments)
    fn_args$model_comments <- model_comments
    metaData_comments <- paste0("Non-standard variables (", paste(add_vars, collapse = ", "), ") were included in this model. ", metaData_comments)
    fn_args$metaData_comments <- metaData_comments
  }
  if (!is.null(remove_vars)) {
    model_comments <- paste0("The standard variables (", paste(remove_vars, collapse = ", "), ") were excluded from this model. ", model_comments)
    fn_args$model_comments <- model_comments
    metaData_comments <- paste0("The standard variables (", paste(remove_vars, collapse = ", "), ") were excluded from this model.", metaData_comments)
    fn_args$metaData_comments <- metaData_comments
  }
  # save fn_args
  save(fn_args, file = paste0(loc_RDataOut, "/" , "runSDM_paths.Rdata"))
  # assign objects
  for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])
  
  # check for missing packages
  req.pack <- c("RSQLite","rgdal","sp","rgeos","raster","maptools","ROCR","vcd","abind",
                "foreign","randomForest","DBI","knitr","RColorBrewer","rasterVis","xtable")
  miss.pack <- req.pack[!req.pack %in% names(installed.packages()[,1])]
  if (length(miss.pack) > 0) {
    stop("Need to install the following package(s) before running this function: ", paste(miss.pack, collapse = ", "))
  }
  
  # steps to run
  all_steps <- c("1","2","3","4","4b","4c","5")
  step_names <- c("1_pointsInPolys_cleanBkgPts.R",
                  "2_attributePoints.R",
                  "3_createModel.R",
                  "4_predictModelToStudyArea.R",
                  "4b_thresholdModel.R",
                  "4c_additionalMetadataComments.R",
                  "5_createMetadata.R"
  )
  run_steps <- step_names[match(begin_step, all_steps) : length(all_steps)]
  
  if (!begin_step %in% c("1","2","3")) {
    if (is.null(model_rdata)) stop("Must provide .Rdata file name if starting after step 3.")
    load(paste0(loc_RDataOut, "/", model_rdata, ".Rdata"))
  }
  
  # run scripts
  for (scrpt in run_steps) {
    message(paste0("Running script ", scrpt , "..."))
    # reload variables
    for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])
    
    # modelrun_meta_data
    if (scrpt == "3_createModel.R") {
      model_start_time <- as.character(Sys.time())
      sdat <- Sys.info()
      model_comp_name <- sdat[['nodename']]
      r_version <- R.version.string
      model_run_name <- gsub(" ","_",gsub(c("-|:"),"",as.character(model_start_time)))
      if (modeller == "Your name") modeller <- sdat[['effective_user']]
      modelrun_meta_data <- list(model_run_name = model_run_name,
                                 model_start_time=model_start_time,
                                 modeller = modeller,
                                 model_comp_name=model_comp_name,
                                 r_version = r_version,
                                 model_comments = model_comments)
    }
    
    # run script
    source(paste(loc_scripts, scrpt, sep = "/"), local = TRUE)
    
    # clean up everything but loop objects
    rm(list=ls()[!ls() %in% c("scrpt","run_steps","prompt","modelrun_meta_data","fn_args")])
    
    message(paste0("Completed script ", scrpt , "..."))
    
    # ask for user input if prompt selected
    if (prompt & scrpt != "5_createMetadata.R") {
      continue <- readline(prompt = "Continue? (1=yes; 0=no):")
      while (!continue %in% c("0","1")) continue <- readline(prompt = "Try again. Continue? (1=yes; 0=no):")
      if (as.integer(continue) == 0) break("model run stopped")
    }
  }
}