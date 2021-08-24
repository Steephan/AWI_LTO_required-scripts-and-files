############
#
#  The Renamer
#
#  Be carefull !!!!
#  
#  Make a backup !!!!
#
############


#############################################################################

# to run this script seperat, you have to uncomment the next 10 lines!
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
#############################################################################




stations.01<-list.files(paste0(p.1$w[p.1$n=="LV1.p"]))

for(k in 1:length(stations.01)){
  files.01  <-list.files(paste0(p.1$w[p.1$n=="LV1.p"],stations.01[k],"/00_full_dataset/"),pattern="*noflag.dat")
  if(length(files.01)>=1){
    for(i in 1:length(files.01)){
      from.path<-paste0(p.1$w[p.1$n=="LV1.p"],stations.01[k],"/00_full_dataset/")
      #cat(files.01[i],"\n")
      #cat(gsub("noflag","final", files.01[i]),"\n")
      file.copy(paste0(from.path,files.01[i]), paste0(from.path,gsub("noflag","final", files.01[i])))
      file.remove(paste0(from.path,files.01[i]))
    }}}
