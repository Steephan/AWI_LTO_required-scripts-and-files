#############################################################################
##
##   script to run all modules
##
##   (each module runs just the last year)
##
##   written by:  Stephan.Lange@awi.de
##
##   last modified: 2019-11-11
##
##   last check: 2020-02-03
##   checked by: christian.lehr@awi.de
##
#############################################################################
##
## open issues:
##
##  - statistics
##
##
#############################################################################
#
# last modifications:
#
# 2019-11-11 uncomment windows paths.... R seems to be able to read the linux paths independent from the running system
# 2019-10-07 implementation of BaSnow2019sr, BaSnow2019cs
# 2019-02-06 new path to new server space ... sparc
# 2016-09-22 rename BaMet2010 to BaMet2009
# 2016-06-17 implementation of Roth basoil lv2
# 2016-03-16 implementation of Bahole2015
#
#
##############################################################################
##
## Comments:
##
##
##
#############################################################################
rm(list = ls())
if (.Platform$OS.type == "windows") {
  p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)

  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
} else {
  p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)

  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
}
#############################################################################
require("zoo")
require("caTools")
origin <- "1970-01-01"
recent.year <- as.numeric(format(Sys.Date(), "%Y"))

##########################
# logging
logging <-1 
write(paste("last update:", Sys.Date()), paste0(p.1$w[p.1$n == "log.p"], "last_update.log"), sep = "\t")
require("sendmailR")
sender <- "soiluser@awi.de" # Replace with a valid address
recipients <- c("niko.bornemann@awi.de", "bill.cable@awi.de", "inge.gruenberg@awi.de", "frederieke.miesner@awi.de") # Replace with one or more valid addresses

if (logging == 1) {
  logfil <- file(paste0(p.1$w[p.1$n == "log.p"], "daily.log"))
  sink(logfil, append = TRUE)
  sink(logfil, append = TRUE, type = "message")
}
##########################

script.raw.path <- paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/")
script.lv0.path <- paste0(p.1$w[p.1$n == "script.p"], "r-level-1-scripts/")
script.lv1.path <- paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/")
script.lv2.path <- paste0(p.1$w[p.1$n == "script.p"], "r-level-2-scripts/Bayelva/")
cat("##################################################################################\n")
cat("#      This is an automatic output of the update of the database, which runs     #\n")
cat("#      daily at 4AM. If you see an error below this, please contact              #\n")
cat("#      stephan.lange@awi.de                                                      #\n")
cat("##################################################################################\n")
cat("#      logfile runtime      ", paste0(Sys.Date()), "                             #\n")
cat("##################################################################################\n")
cat("#      step 1: update new data to database                                       #\n")
cat("##################################################################################\n")

try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/Ba_copy_online_to_raw.R", sep = "")))

cat("##################################################################################\n")
cat("#      step 2: raw to level0                                                     #\n")
cat("##################################################################################\n")

run.year <- recent.year #
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaSnow2013.R", sep = "")))
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaSoil2009_temp.R", sep = "")))
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaSoil2009_tdr.R", sep = "")))
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaMet2009.R", sep = "")))
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaHole2009.R", sep = "")))
#try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaHole2015.R", sep = "")))
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaHole2021.R", sep = "")))
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaSnow2019.R", sep = "")))
try(source(paste(script.raw.path, "Bayelva/RAW_to_LV0_BaSoil2017.R", sep = "")))

cat("##################################################################################\n")
cat("#      step 3: level0 to O2A                                                     #\n")
cat("##################################################################################\n")

station <- 'BaSoil2009' ;day.shift <- 9
try(source(paste(script.raw.path, "O2A/LV0_to_O2A_dataflow.R", sep = "")))
station <- 'BaMet2009'  ;day.shift <- 9
try(source(paste(script.raw.path, "O2A/LV0_to_O2A_dataflow.R", sep = "")))
station <- 'BaHole2021' ;day.shift <- 9
try(source(paste(script.raw.path, "O2A/LV0_to_O2A_dataflow.R", sep = "")))



cat("##################################################################################\n")
cat("#      step 3: level0 to level1                                                  #\n")
cat("##################################################################################\n")

#run.year <- 2010:2019 # recent.year
station <- 'BaSnow2013'# ; years <- 2013:recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))
station <- 'BaSoil2009' #; years <- 2009:recent.year #; run.year <- recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))
station <- 'BaMet2009' # ; years <- 2009:recent.year #; run.year <- recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))
station <- 'BaHole2009' #; years <- 2009:recent.year #; run.year <- recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))
station <- 'BaHole2021' #; years <- 2015:recent.year #; run.year <- recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))
station <- 'BaSnow2019sr' #; years <- 2019:recent.year #; run.year <- recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))
station <- 'BaSnow2019cs' #; years <- 2019:recent.year #; run.year <- recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))
station <- 'BaSoil2017' #; years <- 2017:recent.year #; run.year <- recent.year
try(source(paste(script.lv0.path, "LV0_to_LV1_BaAll.R", sep = "")))

cat("##################################################################################\n")
cat("#      step 4: level1 to level2                                                  #\n")
cat("##################################################################################\n")

cat("#\n# nothing here at the moment\n#\n")


cat("##################################################################################\n")
cat("#      step 5: level1-graphs to wiki                                             #\n")
cat("##################################################################################\n")

try(source(paste(script.lv1.path, "LV1_plots_BaSnow2013.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_BaSoil2009_tdr.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_BaSoil2009_temp.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_BaMet2009.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_BaHole2009.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_BaHole2021.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_BaSnow2019cs.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_BaSoil2017.R", sep = "")))

# cat("##################################################################################\n")
# cat("#      step 6: level2-graphs to wiki                                             #\n")
# cat("##################################################################################\n")
#
#try(source(paste(script.lv2.path,"LV2_BaMet2009.R", sep = "")))
# cat("##################################################################################\n")
# cat("#      step 7: level2-update dataflow                                            #\n")
# cat("##################################################################################\n")
# station <- 'BaSoil2009' ; years <- 2009:recent.year ; run.year <- recent.year
# day.shift: how many days are updated for the dataflow product? default is 1,
# ==> use larger numbers to fill gaps in http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:bayelva:met:control
#day.shift <- 1
#try(source(paste(script.lv2.path, "LV1_to_dataflow.R", sep = "")))

# cat("##################################################################################\n")
# cat("#      step 7: level1-update PANGAEA                                             #\n")
# cat("##################################################################################\n")
#
#try(source(paste(script.lv2.path,"LV1_to_PANGAEA.R", sep = "")))

# cat("##################################################################################\n")
# cat("#      step 8: weekly statistic to SL and NB                                     #\n")
# cat("##################################################################################\n")

if (logging == 1) {
  sink(type = "message")
  sink()
}
body <- geterrmessage()
#############################################################################
#
#  Attention!!!
#
#
#
#
#############################################################################
if (nchar(body) > 0) {#
  sapply(recipients, function(x) sendmail(sender,
                                         to = x,
                                         subject = "Database-Error, Bayelva has some problems",
                                         msg = body,
                                         control = list(smtpServer = "smtp.awi.de", port = 587)))
}
