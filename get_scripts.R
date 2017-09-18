if (!"git2r" %in% names(installed.packages()[,1])) stop ("Need to install 'git2r' package first.")
library(git2r)

if (exists("model_rdata")) dt <- as.Date(strsplit(model_rdata,"_")[[1]][2], format = "%Y%m%d") else (dt <- Sys.Date())
script_store <- paste0(loc_scripts, "/Regional_SDM_", dt)
if (!dir.exists(script_store)) {
  try(suppressMessages(git_repo <- git2r::clone("https://github.com/VANatHeritage/Regional_SDM.git",
                                                branch = "dev", local_path = script_store)), silent = TRUE)
  if (exists("git_repo")) {
    message("Scripts downloaded. Ready to run.")
  } else {
    dir.create(script_store)
    message(paste0("Couldn't download latest scripts. \nNew folder '",
                   script_store, "' created. \nPlace latest scripts in there."))
  }
} else {
  try({
    git_repo <- git2r::repository(script_store)
    git_pull <- git2r::pull(git_repo)
  })
  if (exists("git_pull") && (git_pull@up_to_date || git_pull@fast_forward)) {
    message("Scripts up-to-date. Ready to run.")
  } else {
    message(paste0("Couldn't download latest scripts. Make sure latest scripts are in folder '",
                   script_store, "'"))
  }
}
suppressWarnings(rm(git_repo, git_pull))
message(paste0("Set loc_scripts to '", script_store ,"'."))
