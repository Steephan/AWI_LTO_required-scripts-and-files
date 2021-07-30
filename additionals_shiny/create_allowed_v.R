###############################################################
#
#  Create "allowed variables" for available Stations at DB
#
#
#  2021-04-01 SL: new station BaHole2021
#
#
#
#
#
#
################################################################


stations <- c("BaMet1998","BaMet2009","BaSoil1998","BaSoil2009","BaSoil2017",
            "BaHole2009","BaHole2015","BaHole2021","BaSnow2013",
            "BaSnow2019sr","BaSnow2019cs","BaEddy2007",

            "KuQ12013","KuLucky22014","KuLucky12013","KuLucky2013",

            "SdHole2009",

            "SaMet1998","SaSoil1998",
            "SaSoil2002","SaSoil2012","SaMet2002","SaPrec2019",
            "SaHole2006","SaHole2010","SaHole2018",
            "SaPond2006","SaPond2014","SaSnow2012","SaSnow2016",

            "TVCSoil2016","TVCHole12015","TVCHole22015","TVCeccc")

sink("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv")
cat("dataset, variable\n")
for (i in stations) {
  hui <- list.files(paste0("N:/sparc/data/LTO/level1/", i, "/00_full_dataset/"), pattern = "noflag.dat")[1]
 # zack<-read.table(paste0("N:/geo5/SoilData/data/level1/",i,"/00_full_dataset/",hui),sep=",",dec=".",header=T)
  cnames <- colnames(read.table(paste0("N:/sparc/data/LTO/level1/", i,"/00_full_dataset/", hui), sep = ",", dec = ".", header = T))
  #cnames <- colnames(read.table(paste0("/sparc/data/LTO/level1/", i,"/00_full_dataset/", hui), sep = ",", dec = ".", header = T))
  # remove columns which are not needed for level 1
  tmp <- c(which(grepl('Tpan', cnames) |
                   grepl('batt_U', cnames) |
                   grepl('Ubat', cnames) |
                   grepl('Bat_V', cnames) |
                   grepl('TableFlag',cnames) |
                   grepl('dist',cnames) |
                   grepl('raw', cnames) |
                   grepl('UTC', cnames) |
                   grepl('albedo', cnames) |
                   grepl('Tsen', cnames) |
                   grepl('mV',cnames) |
                   grepl('QA',cnames) |
                   grepl('SQ',cnames) |
                   grepl('sq', cnames)))
  if (length(tmp) > 0) {
    cnames <- cnames[-tmp]
  }
  for (j in 1:length(cnames)) {

    cat(paste0(i, ", ", cnames[j], "\n"), append = T)
  }
  #cat(paste0(i,", ",cnames[j],"\n"))
}
sink()

#file.show("N:/sparc/LTO/R_database/flagger_sa/allowedVariables.csv")

# run sink() k times to close the last sink. Not clear why this is necessary.
# i <- sink.number() gives the number of open diversions
# solution from https://stackoverflow.com/questions/18730491/sink-does-not-release-file

k <- sink.number()
while (k > 0) {
    sink()
    k <- k - 1
}


