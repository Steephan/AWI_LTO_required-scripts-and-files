##########################################################################################################################
##
##   script to run Samoylov modules
##
##   (each module runs just the last years)
##
##   by: Stephan.Lange@awi.de -2200
##
##########################################################################################################################
#############################################################################
#
# last modifications:
#
# 2016-03-16 implementation of SaHole2006
# 2020-08-03  CL add some stations and update the style of the script
#
#
# still to do:
# - all other stations :-)
# -
#
#
#
#
#############################################################################
rm(list = ls())
if (.Platform$OS.type == "windows") {
  path <- read.table("N:/sparc/LTO/R_database/database_R/settings/sa_path_windoof.txt", sep = "\t", header = T)
  maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/sa_maintance.txt", sep = "\t", header = T)
  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
} else {
  path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
#############################################################################
require("zoo")
require("caTools")
origin <- "1970-01-01"
recent.year <- as.numeric(format(Sys.Date(), "%Y"))

# choose year
run.year <- recent.year

script.raw.path <- paste0(path$w[path$n == "script.p"], "database_R/Sa_01_Raw_Lvl0/")
script.lv0.path <- paste0(path$w[path$n == "script.p"], "database_R/Sa_02_Lvl0_Lvl1/")
script.lv1.path <- paste0(path$w[path$n == "script.p"], "database_R/Sa_03_Lvl1/")
cat("##################################################################################\n")
cat("#      step 1: update new data to database                                       #\n")
cat("##################################################################################\n")

#try(source(paste0(path$w[path$n == "script.p"], "database_R/Ba_copy_online_to_raw.R", sep = "")))

cat("##################################################################################\n")
cat("#      step 2: raw to level0                                                     #\n")
cat("##################################################################################\n")

try(source(paste(script.raw.path, "RAW_to_LV0_SaSoil2002.R", sep = "")))
try(source(paste(script.raw.path, "RAW_to_LV0_SaSoil2012.R", sep = "")))


cat("##################################################################################\n")
cat("#      step 3: level0 to level1                                                  #\n")
cat("##################################################################################\n")

station <- 'SaSoil2002'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaSoil2012'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaMet2002'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaSnow2012'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaSnow2016'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaHole2006'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaHole2010'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaHole2018'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SdHole2009'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))
station <- 'SaPond2014'
try(source(paste(script.lv0.path, "LV0_to_LV1_SaAll.R", sep = "")))


cat("##################################################################################\n")
cat("#      step 4: level1-graphs to wiki                                             #\n")
cat("##################################################################################\n")

try(source(paste(script.lv1.path, "LV1_plots_SaSoil2002.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaSoil2012.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaMet2002.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaSnow2012.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaSnow2016.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaHole2006.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaHole2010.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaHole2018.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SdHole2009.R", sep = "")))
try(source(paste(script.lv1.path, "LV1_plots_SaPond2014.R", sep = "")))

cat("##################################################################################\n")
cat("#      step 5: weekly statistic to SL and NB                                     #\n")
cat("##################################################################################\n")
# sink(type = "message")
# sink()
#
# body <- geterrmessage()
#############################################################################
#
#  Attention!!!
#
#
#
#
#############################################################################
# if(nchar(body)>0){#
#   sapply(recipients, function(x) sendmail(sender, to = x, subject = "Database-Error, Samoylov RAW 2 Level1", msg = body,
#                                          control = list(smtpServer = "smtp.awi.de")))
# }

