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
rm(list=ls())
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)
  source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}else{
  path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
  source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}
#############################################################################
#############################################################################



# 
from.path <-paste0(path$w[path$n=="RAW.p"],"SaSoil2010/02_Data_TDR/vor_2015/")
to.path   <-paste0(path$w[path$n=="RAW.p"],"SaSoil2010/02_Data_TDR/vor_2015_v2/")
files.01  <-list.files(from.path,pattern="*.dat")

for(i in 1:length(files.01)){
  
  
  dada.t<-read.table(paste(from.path,files.01[i],sep=""),sep=",",dec=".",header=F,skip=4, fill = TRUE,na="NAN")[,1:2]

  von<-paste0(substr(lapply(dada.t[1,1],as.character),1,4),substr(lapply(dada.t[1,1],as.character),6,7),substr(lapply(dada.t[1,1],as.character),9,10))
  bis<-paste0(substr(lapply(tail(dada.t)[1,1],as.character),1,4),substr(lapply(tail(dada.t)[1,1],as.character),6,7),substr(lapply(tail(dada.t)[1,1],as.character),9,10))
  cat(von,"  ",bis,"  ",length(dada.t[,1]),"  ",files.01[i],"\n")
  file.copy(paste0(from.path,files.01[i]), paste0(to.path,bis,"-",von,"_CR1000_SaSoil2010_sab_TDR.dat"))
  
}
