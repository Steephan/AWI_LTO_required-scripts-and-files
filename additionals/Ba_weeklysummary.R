#############################################################################
##
##   Weekly statistics of Bayelva
##   
##   ( weekly run after DB-update, monday 7am )
##   cronjob:
##
##   by: Stephan.Lange@awi.de ,stefan.eberlein@awi.de
##   modified: 2017/01/09
##    
##
#############################################################################

#
rm(list=ls())
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
  source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}else{
  path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8") 
  maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
  
  source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}
#
#


# Workflow: Load data sets, analyse data quality, format for Wiki integration

today <- Sys.Date()# it should run once a week on monday morning at 7am
week  <- strftime(today,format="%V") 
week.before<-as.num(week)-1
year  <- as.POSIXlt(today)$year + 1900

von.00<-as.POSIXct(strptime(paste("00:00","1",as.num(week)-1,"2017"),format="%H:%M %u %U %Y"), tz = "UTC")
#von.00<-strftime(strptime(paste("00:00","1",as.num(week)-1,"2017"),format="%H:%M %u %U %Y"),format="%Y-%m-%d %H:%M:%S", tz = "UTC") 
bis.00<-as.POSIXct(strptime(paste("23:00","7",week,"2017"),format="%H:%M %u %U %Y"),tz = "UTC")
bis.30<-as.POSIXct(strptime(paste("23:30","7",week,"2017"),format="%H:%M %u %U %Y"),tz = "UTC")

####################################################################################################
####  001                         Load data from last week                                      ####
####################################################################################################
bahole15pfad <- paste0(path$w[path$n=="LV1.p"],"BaHole2015/temperature/BaHole2015_temperature_",
                       year,".dat")
bamet09pfad  <- paste0(path$w[path$n=="LV1.p"],"BaMet2009/00_full_dataset/BaMet2009_",
                       year, "_lv1.dat")
basnow13pfad <- paste0(path$w[path$n=="LV1.p"],"BaSnow2013/BaSnow2013_",
                       year, "_lv1.dat")
basoil09pfad <- paste0(path$w[path$n=="LV1.p"],"BaSoil2009/00_full_dataset/BaSoil2009_full_",
                       year, "_lv1.dat")
#################
# at this moment it is under maintenance
# --------------------------------------------------------------------------------
bahole15        <- read.csv(bahole15pfad)#"../testdata/BaHole2015_temperature_2016.dat")
bahole15$UTC    <- as.POSIXct(bahole15$UTC, tz = 'UTC')
bahole15        <- bahole15[bahole15$UTC >= von.00 & bahole15$UTC <= bis.30,]
bahole15.flags  <- bahole15[c(1, seq(from = 3, to = ncol(bahole15), by = 2))]
bahole15.data   <- bahole15[c(1, seq(from = 2, to = ncol(bahole15), by = 2))]

bamet09         <- read.csv(bamet09pfad)
bamet09$UTC     <- as.POSIXct(bamet09$UTC, tz = "UTC")
bamet09         <- bamet09[bamet09$UTC >= von.00 & bamet09$UTC <= bis.30,]
bamet09.flags   <- bamet09[c(1, seq(from = 3, to = ncol(bamet09), by = 2))]
bamet09.data    <- bamet09[c(1, seq(from = 2, to = ncol(bamet09), by = 2))]

basnow13        <- read.csv(basnow13pfad)
basnow13$UTC    <- as.POSIXct(basnow13$UTC, tz = "UTC")
basnow13        <- basnow13[basnow13$UTC >= von.00 & basnow13$UTC  <= bis.30,]
basnow13.flags  <- basnow13[c(1, seq(from = 3, to = ncol(basnow13), by = 2))]
basnow13.data   <- basnow13[c(1, seq(from = 2, to = ncol(basnow13), by = 2))]

basoil09        <- read.csv(basoil09pfad)
basoil09$UTC    <- as.POSIXct(basoil09$UTC, tz = 'UTC')
basoil09        <- basoil09[basoil09$UTC >= von.00 & basoil09$UTC  <= bis.30,]
basoil09.flags  <- basoil09[c(1, seq(from = 3, to = ncol(basoil09), by = 2))]
basoil09.data   <- basoil09[c(1, seq(from = 2, to = ncol(basoil09), by = 2))]


bahole15.flaghist <- createFlaghist(bahole15.flags)# still out of order
bamet09.flaghist  <- createFlaghist(bamet09.flags)
basnow13.flaghist <- createFlaghist(basnow13.flags)
basoil09.flaghist <- createFlaghist(basoil09.flags)


####################################################################################################
#### 002                               NA Statistics                                            ####
####################################################################################################

bahole15.NAstats <- naStats(bahole15.data)[-1 ,]
bamet09.NAstats  <- naStats(bamet09.data)[-1 ,]
basnow13.NAstats <- naStats(basnow13.data)[-1 ,]
basoil09.NAstats <- naStats(basoil09.data)[-1 ,]

####################################################################################################
#### 003                               GAPS                                                     ####
####################################################################################################
#
bahole15.longestGaps <- within(longestGaps(bahole15.data, 10), days <- round(gaps/24, 2))
bamet09.longestGaps  <- within(longestGaps(bamet09.data, 10), days <- round(gaps/48, 2)) ## Account for halfhour-measurements
basnow13.longestGaps <- within(longestGaps(basnow13.data, 10), days <- round(gaps/48, 2))
basoil09.longestGaps <- within(longestGaps(basoil09.data, 10), days <- round(gaps/24, 2))

# find continuous gaps per sensor
#unsplit(lapply(bahole15.data, function(x){longestGaps(data.frame(bahole15.data$UTC, x))}), names(bahole15.data))
# lapply(bamet09.data, function(x){longestGaps(data.frame(bamet09.data$UTC, x))})
# lapply(basnow13.data, function(x){longestGaps(data.frame(basnow13.data$UTC, x))})

bahole15.sameRows <- sameRows(bahole15.data)
bamet09.sameRows  <- sameRows(bamet09.data)
basnow13.sameRows <- sameRows(bamet09.data)
basoil09.sameRows <- sameRows(basoil09.data)

####################################################################################################
#### 003                               NA Plots                                                 ####
####################################################################################################

## Uncomment to write plots
plotNAdistribution(bahole15.data, "NA Distribution in BaHole2015", paste0(path$w[path$n=="script.p"],"wikiplots/BaHole2015WeeklyNAplot.png"))
plotNAdistribution( bamet09.data, "NA Distribution in BaMet2009",  paste0(path$w[path$n=="script.p"],"wikiplots/BaMet2009WeeklyNAplot.png"))
plotNAdistribution(basnow13.data, "NA Distribution in BaSnow2013", paste0(path$w[path$n=="script.p"],"wikiplots/BaSnow2013WeeklyNAplot.png"))
plotNAdistribution(basoil09.data, "NA Distribution in BaSoil2009", paste0(path$w[path$n=="script.p"],"wikiplots/BaSoil2009WeeklyNAplot.png"))

####################################################################################################
#### 004                               plots to wiki                                            ####
####################################################################################################
#### http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:bayelva:cplots       ####
####
#### this wrote directly the wikipage above
####
cplotsTxtOutput <- paste(
  " <columns 100% 40% ->",
  paste0("====== Gap control plots for week ",week," ====== "),
  paste0("| ",von.00," 00:00:00 ^ to | ",bis.30," | "),
  " ",
  "please contact --- [[stephan.lange@awi.de|Stephan Lange]] ",
  " ",
  paste0("last run: ",paste(Sys.time()),"\n\n"),
  " <fc #ff6b6b>(Mr. Moustache is currently in maintenance)</fc> ",
  " <newcolumn 50%>",
  " {{page>observatory:data:analysis:bayelva:navi_table&nodate&noeditbtn&nouser}}",
  " </columns>",
  
  
  #  "==== Gap control plots ====",
  " <columns 100% 25%% 25%% 25%% ->",
  "=== BaHole2015 ===",
  "{{http://sparcwiki.awi-potsdam.de/soildatadoc/scripts/wikiplots/BaHole2015WeeklyNAplot.png?direct&420|}}\n",
  " <newcolumn 25%%>",
  "=== BaMet2009 ===",
  "{{http://sparcwiki.awi-potsdam.de/soildatadoc/scripts/wikiplots/BaMet2009WeeklyNAplot.png?direct&420|}}\n",
  " <newcolumn 25%%>",
  "=== BaSoil2009 ===",
  "{{http://sparcwiki.awi-potsdam.de/soildatadoc/scripts/wikiplots/BaSoil2009WeeklyNAplot.png?direct&420|}}\n",
  " <newcolumn 25%%>",
  "=== BaSnow2013 ===",
  "{{http://sparcwiki.awi-potsdam.de/soildatadoc/scripts/wikiplots/BaSnow2013WeeklyNAplot.png?direct&420|}}\n",
  " </columns>",
  sep = "\n")

cplotsTxtPath <- paste0(path$w[path$n=="wiki.p"],"observatory/data/analysis/bayelva/cplots.txt")
## Uncomment for actual write, append?
cat(cplotsTxtOutput,
    file = cplotsTxtPath,
    append = F)

####################################################################################################
#### 005                               stats to wiki                                            ####
####################################################################################################
###  http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:bayelva:stats
####
#### this wrote directly the wikipage above
####
statsTxtDataOut <- paste(
  " <columns 100% 40% ->",
  paste0("====== statistics for week ",week," ====== "),
  paste0("| ",von.00," 00:00:00 ^ to | ",bis.30," | "),
  " ",
  "please contact --- [[stephan.lange@awi.de|Stephan Lange]] ",
  " ",
  paste0("last run: ",paste(Sys.time()),"\n\n"),
  " <fc #ff6b6b>(Mr. Moustache is currently in maintenance)</fc> ",
  " <newcolumn 50%>",
  " {{page>observatory:data:analysis:bayelva:navi_table&nodate&noeditbtn&nouser}}",
  " </columns>",
  
  # "==== Flag/NA statistics ====",
  # paste0("From: ", bahole15$UTC[1], " - To: ", bahole15$UTC[length(bahole15$UTC)]),
  " <columns 100% 25% 25% 25% ->",
  "=== BaHole2015 ===",
  "== Flags ==",
  "no values at the moment",
  #convertToDokuwikiTable(bahole15.flaghist),
  "== NA statistics ==",
  "no values at the moment",
  #convertToDokuwikiTable(bahole15.NAstats),
  "== Longest Gaps ==",
  "no values at the moment",
  if(bahole15.longestGaps$gaps[1]!=0){
    paste(
      convertToDokuwikiTable(bahole15.longestGaps))}else{
        convertToDokuwikiTable(NULL)
      },
  "== Identical rows ==",
  "no values at the moment",
  #convertToDokuwikiTable(bahole15.sameRows),
  "",
  " <newcolumn 25%>",
  "=== BaMet2009 ===",
  "== Flags ==",
  convertToDokuwikiTable(bamet09.flaghist),
  "== NA statistics ==",
  convertToDokuwikiTable(bamet09.NAstats),
  "== Longest Gaps ==",
  if(bamet09.longestGaps$gaps[1]!=0){
    paste(
      convertToDokuwikiTable(bamet09.longestGaps))}else{
        convertToDokuwikiTable(NULL)
      },
  "== Identical rows ==",
  convertToDokuwikiTable(bamet09.sameRows),
  "",
  " <newcolumn 25%>",
  "=== BaSoil2009 ===",
  "== Flags ==",
  convertToDokuwikiTable(basoil09.flaghist),
  "== NA statistics ==",
  convertToDokuwikiTable(basoil09.NAstats),
   "== Longest Gaps ==",
  if(basoil09.longestGaps$gaps[1]!=0){
    paste(
  convertToDokuwikiTable(basoil09.longestGaps))}else{
    convertToDokuwikiTable(NULL)
  },
  "== Identical rows ==",
  convertToDokuwikiTable(basoil09.sameRows),
  "",
  " <newcolumn 25%>",
  "=== BaSnow2013 ===",
  "== Flags ==",
  convertToDokuwikiTable(basnow13.flaghist),
  "== NA statistics ==",
  convertToDokuwikiTable(basnow13.NAstats),
  "== Longest Gaps ==",
  if(basnow13.longestGaps$gaps[1]!=0){
    paste(
      convertToDokuwikiTable(basnow13.longestGaps))}else{
        convertToDokuwikiTable(NULL)
      },
  
  "== Identical rows ==",
  convertToDokuwikiTable(basnow13.sameRows),
  " </columns>",
  sep = "\n")

## Modify input?
statsTxtPath  <- paste0(path$w[path$n=="wiki.p"],"observatory/data/analysis/bayelva/stats.txt")
statsTxtInput <- readChar(statsTxtPath, file.info(statsTxtPath)$size)
statsTxtInput <- unlist(strsplit(statsTxtInput, "(?<=[\\s])", perl = T))
#statsTxtOutput <- paste0(statsTxtInput[1:grep("from", test.split)])



## Uncomment for actual write, append?
cat(statsTxtDataOut,
    file = statsTxtPath,
    append = F)






