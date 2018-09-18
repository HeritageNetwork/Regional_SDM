# This script installs/updates packages used in the modelling process of this repository.
# Run it occasionally (especially after updating R itself) to keep packages up-to-date.

pkg_list <- c("RSQLite","rgdal","sp","rgeos","raster","maptools",
              "ROCR","vcd","abind","foreign","randomForest",
              "snow", "DBI", "knitr","RColorBrewer","rasterVis","xtable",
              "git2r","spsurvey","classInt","tinytex", "stringi")
installed <- installed.packages()
to_inst <- pkg_list[!pkg_list %in% installed[,1]]

if (length(to_inst) > 0) {
  install.packages(to_inst)
  update.packages(oldPkgs = pkg_list[!pkg_list %in% to_inst], ask = FALSE)
} else {
  update.packages(oldPkgs = pkg_list, ask = FALSE)
}

rm(pkg_list, installed, to_inst)