# Regional_SDM

This repository is for collaboration among Heritage programs and NatureServe for the development of distribution models in a consistent way. 

You are currently reading the Readme file. Scripts (and other files as needed) are also part of this repository.

# To get a local copy

`git clone git@github.com:HeritageNetwork/Regional_SDM.git`

and then check out the wiki for software and customizations

# A brief overview of the setup process to run these scripts
 - Make sure there are appropriate entries in these tables in the sqlite DB (SDM_lookupAndTracking.sqlite)
   - lkpDataSources
   - lkpEnvVars
   - lkpModelers
   - lkpRange
   - lkpSpecies
   - lkpSpeciesRubric
 - Make sure there are appropriate entries in the SQL Server (HSM_Tracker) DB. 
   This only matters for metadata output and could eventually be optionally 
   shifted to the SQLite DB. 
 - Collate and/or create links to environmental variable data sets (see
   https://github.com/HeritageNetwork/Regional_SDM/wiki/User-Customizations)
 - 


License
=======
This project is licensed under the terms of the Creative Commons Attribution-ShareAlike 4.0 International Public
License.

license:cc-by-sa-4.0

See `<LICENSE.txt>` to see the full text.


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3874896.svg)](https://doi.org/10.5281/zenodo.3874896)
