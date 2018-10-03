# Regional_SDM

This repository is for collaboration among the PA, VA, and NY Heritage programs for the development of a methodology for aquatic Species Distribution Modeling. 

You are currently reading the Readme file. Scripts (and other files as needed) are also part of this repository.

Use the wiki as a place to put together descriptions and steps for using the scripts and data developed and posted here. 

## Prepping Aquatic Species Data

Aquatic presence files should be .csv files with the following required columns:

| COMID    | huc12       | group_id | EO_ID_ST | SCOMNAME       | SNAME                     | OBSDATE    |
| -------- | ----------- | -------- | -------- | -------------- | ------------------------- | ---------- |
| 14639447 | 60102050602 | 1        | 1        | Blackside Dace | Chrosomus cumberlandensis | 2018-07-01 |
| 14639467 | 60102050602 | 2        | 2        | Blackside Dace | Chrosomus cumberlandensis | 1999-09-26 |
| 22539154 | 60102060202 | 3        | 3        | Blackside Dace | Chrosomus cumberlandensis | NA         |
| 22539156 | 60102060202 | 3        | 4        | Blackside Dace | Chrosomus cumberlandensis | NA         |

- COMD: unique ID for the presence feature
- huc12: Full 12-digit HUC the presence feature is associated to
- group_id: The group which the reach belongs to, used for partitioning data during model cross-validation
- EO_ID_ST: the EO_ID for the presence; not currently implemented into the model routine.
- SCOMNAME: common name of the species
- SNAME: scientific name of the species
- OBSDATE: date of the observation. Can be NA; not currently implemented into the model routine.

## Prepping for model runs

The prerequisites necessary to begin running models are listed below. As of 10/2018, no specific folder structure is needed, as the model run creates the needed folders during the run.

- project folder and populated project database
  - use [this script](preprocessing/preproc_getCleanSqliteDB.R) to create a project folder and database
  - make sure to populate `lkpSpecies` with data for your species, and update the `lkpEnvVars[Aqua]` tables with new variables
- background locations (shapefile)
- environmental variables (.csv file)
- several shapefiles for the metadata map:
  - reference boundaries
  - study area extent boundary
  - aquatic areas (optional)

## Running models

A complete model can be run using the function `run_SDM`. The user can access the function through the template file `user_run_SDM.R`. To begin, create a working copy of that file (outside of the repository) which you can edit for running your local models.

### Step 1: set up variables/scripts

The first section of `user_run_SDM.r` sets up key variables for the model run, and downloads the latest scripts from this repository for the run. The key variables include:

- loc_model: where all model inputs/outputs are stored. 
- project_db: The path to the project database.
- **model_species**: The code for the species, referenced in `lkpSpecies` table in your project database. A new folder will be created in your project_db with this name, if it doesn't already exist.
- **nm_presFile**: The species presences file.

Those in **bold** will change for every species, while the other variables will stay fixed for a given project. Other variables can be edited directly in the function call in Step 2.

Finish out section 1 to download scripts for the species, and load the `run_SDM` function into your R environment.

### Step 2: run the model

In general, you should not need to edit anything in this section, for a "standard" model run, but you can add specific comments to the model, for the database or metadata sheet here. 

You can also add or remove certain variables from the model, by referencing them by their names in a character vector; e.g., `remove_vars = c("bad_variable1","bad_variable2")`. 

Use `prompt = TRUE` to have the process pause after each step and ask for user input to continue.



### Step 2-alternate: pick up an existing model run

This section contains examples of how to pick up an existing model run (e.g., any run that completed the first `run_SDM`'s first step). For these runs starting after step 1 of the `run_SDM` process, you need to provide at least:

- begin_step
- model_species
- loc_model
- and if starting after step 3:
  - model_rdata

All other inputs for this picked-up run are taken from the previous run for the species, which is saved as an rdata file, but can can add more arguments if you want to alter them for the picked-up run (e.g., `nm_presFile`, `remove_vars`, `add_vars`, `metadata_comments`, `prompt`).

Model run input files are archived and timestamped and stored in ([species_code]/model_inputs). They can be reference by the `nm_presFile` argument (no file extension).

Model run output files are archived and timestamped and stored in  ([species_code]/model_outputs). They must be referenced by the `model_rdata` argument (no file extension); this is only relevant when starting after step 3.



## Testing/Editing scripts

If you are developing a new feature and want to test on the scripts, checkout the branch you're working on in git, and then set the `loc_scripts` in `user_run_SDM.R` to your main location for this repository (skip the `get_scripts.R` step).

- If you're testing changes to script 1, use the full `run_SDM` function call in `user_run_SDM` step 2.

- If you're testing changes to script 2 or 3, use the `run_SDM` function call in `user_run_SDM` step 2-alternate, with the `nm_presFile` created in a previous run specified.

- If you're testing changes to script 4 or later, use the `run_SDM` function call in `user_run_SDM` step 2-alternate, with the `model_rdata` specified.

Tips for testing/editing:

- Make use of `prompt = TRUE` to stop the function from running multiple steps

- use a small presence file (e.g. 5-10 presences)
- use a spatially subset environmental variables .csv and background locations shapefiles
- if running into an error at a specific line of code, add a `browser()` call right before it in the script to interact with the environment
  - Don't forget to remove the browser call after the error is fixed