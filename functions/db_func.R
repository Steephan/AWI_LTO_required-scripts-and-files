###############################################################################
#
#   MAIN function-script
#
#   by: stephan.lange@awi.de
#   last modified: 2016-04-25
#
##
####
#### for better overview this main-script was divided into 3 sub-scripts
####
#### db_helper - everything to the database except of:
#### db_filter - LEVEL1 filter/flag-functions
#### db_plotter- plotting functions
####
###
##
#
if (.Platform$OS.type == "windows") {
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_helper.R")
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_filter_I.R")
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_filter_II.R")
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_diel_to_vwc.R")
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_vwc.R")
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_plotter.R")
} else {
  source(  "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_helper.R")
  source(  "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_filter_I.R")
  source(  "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_filter_II.R")
  source(  "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_diel_to_vwc.R")
  source(  "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_vwc.R")
  source(  "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_plotter.R")
}
