###############################################################
#
#  Create yearlyDataPath_III_auto.csv
#  automaticly by available datasets
#
#  2021-04-01 SL: new station BaHole2021
#  2021-03-23 SL: new git structure pathes
#  2020-01-13 CL: define option "Sd" for i == 3 in loop
#  2019-02-04 SL: new path to sparc
#
#
################################################################


stations <- c("Samoylov","Bayelva","Sardakh","TVC","Kurungnakh")
datas <- list()
datas[[1]] <- c("SaMet1998","SaSoil1998",
              "SaSoil2002","SaSoil2012","SaMet2002",
              "SaHole2006","SaHole2010", "SaHole2018",
              "SaPond2006","SaPond2014","SaSnow2012","SaSnow2016")
datas[[2]] <- c("BaMet1998","BaMet2009","BaSoil1998","BaSoil2009","BaSoil2017",
              "BaHole2009","BaHole2015","BaHole2021","BaSnow2013","BaSnow2019sr",
              "BaSnow2019cs","BaEddy2007")
datas[[3]] <- c("SdHole2009")
datas[[4]] <- c("TVCSoil2016","TVCHole12015","TVCHole22015")
datas[[5]] <- c("KuQ12013","KuLucky22014","KuLucky12013","KuLucky2013")

#running.system<-1
# 1 - windows
# 2 - linux AWI WIKI
# 3 - linux AWI trendsetter
# 4 - linux Stefan
# 5 - linux Jan
# (6 - linux)

origin <- "1970-01-01"

#### import data ####
for (kl in 1:3) {#1:5
  ## read paths and allowed variables

  if (kl == 1) {
    pre.sys <- "N:/sparc"
    name.sys <- "_auto"
    lv1 <- "_lv1"
  } else if (kl == 2) {
    pre.sys <- "/sparc"
    name.sys <- "_AWI"
    lv1 <- "_lv1"
  } else if (kl == 3) {
    pre.sys <- "/sparc"
    name.sys <- "_AWI_noflag"
    lv1 <- "_lv1_noflag"
  } else if (kl == 4) {
    pre.sys <- "/media/sparc"
    name.sys <- "_JN"
    lv1 <- "_lv1"
  } else if (kl == 5) {
    pre.sys <- "N:/sparc"
    name.sys <- "_auto_noflag"
    lv1 <- "_lv1_noflag"
  }

  sink(paste0("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath", name.sys, ".csv"))
  cat("station,dataset,year,path,db.path,endung\n")
  for (i in 1:5) { # stations 1-5 
    for (j in datas[[i]]) {
       hui <- list.files(paste0("N:/sparc/data/LTO/level1/", j, "/00_full_dataset/"), pattern = "noflag.dat")
      for (k in 1:length(hui)) {
        lv0.data <- read.table(paste0("N:/sparc/data/LTO/level1/", j, "/00_full_dataset/", hui[k]), sep = ",", dec = ".", header = T)[1, ]
        year <- as.numeric(format(as.POSIXct(lv0.data[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y'))

        
        if (i == 1) {wo <- "Sa"} else if (i == 2) {wo <- "Ba"} else if (i == 3) {wo <- "Sd"} else if (i == 4) {wo <- "TVC"} else if (i == 5) {wo <- "Ku"}
        cat(paste(stations[i], j, year, paste0(pre.sys, "/data/LTO/level1/", j, "/00_full_dataset/", j, "_", year, lv1, ".dat"),
                  paste0(pre.sys, "/LTO/R_database/Time_series_preprocessing/sparcflags/shiny-server/LV1_", wo, "_flagger_update.R"), "1 \n", sep = ","), append = T)
      }
    }
  }
  sink()
}

# sink() # final sink() to close the last sink. Not clear why this is necessary.
