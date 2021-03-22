#############################################################################
##
##   copy and archive new onlinefiles to raw
##   
##   
##
##   by: Stephan.Lange@awi.de -2201
##   last modified: 2016/05/06
##
#############################################################################

#############################################################################
# 
# still to do:
# -see at individual stations
# 
#
#############################################################################
# # this part is necessary to run this script seperate 
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
#   
#   source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# } else {
#   p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
#   
#   source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# }
#############################################################################

mon         <-as.numeric(format(Sys.Date(),"%m"))
jah         <-as.numeric(format(Sys.Date(),"%Y"))
options(scipen=100,stringsAsFactors=F) # for non-exponential display of numeric values

#############################################################################
#   BaSnow2013
#############################################################################
#
#  daily files; copy every new files and the last exiting file (it wasn`t complete)
#
#  das kann eigentlich so bleiben
#  eventuell eine Abfrage ob neue Daten da sind ... wenn nicht, dann eine Nachricht
#  

onl.snow    <-list.files(paste0(p.1$w[p.1$n=="BaSn.onl.p"],jah,"/"),pattern=".dat", full.names = TRUE)
raw.snow    <-list.files(paste0(p.1$w[p.1$n=="BaSn.raw.p"],jah,"/"),pattern=".dat", full.names = TRUE)
how.many    <-length(onl.snow)-length(raw.snow)

for(i in 1:(how.many+1)){# plus 1 to update the last file
   file.copy(onl.snow[length(onl.snow)+1-i],paste0(p.1$w[p.1$n=="BaSn.raw.p"],jah,"/"), overwrite = TRUE, recursive = TRUE)
}
cat("#\n# copy/save",how.many+1,"BaSnow2013 file(s)\n#")


#############################################################################
#   BaSoil2009
#############################################################################
#
# to do: gilt auch fuer BaMet2015-files
# aktuell werden die Daten im online-ordner 1:1 kopiert .. mehr nicht
#
# ziel ist eine regelmaessige Sicherung/Archivierung (alle 1-3 Monate), mit anschliessendem Loeschen der Ursprungs-datei
# das passiert momentan alles noch manuell und unregelmaessig
#
# wichtig: es darf wirklich nur geloescht werden wenn vorher auch gesichert wurde!!!
# also muss auch solch eine Abfrage vorher geschehen!
#
files.soil    <-list.files(paste0(p.1$w[p.1$n=="BaS.onl.p"]),pattern=".dat", full.names = TRUE)
schon.da.soil <-length(list.files(paste0(p.1$w[p.1$n=="BaS.raw3.p"]),pattern=paste(format(Sys.Date(),"%Y%m")), full.names = TRUE))

if(mon==3&schon.da.soil==0|mon==6&schon.da.soil==0|mon==9&schon.da.soil==0|mon==12&schon.da.soil==0){
  # alle 3 Monate wird eine Sicherungskopie mit Datum in Raw gespeichert
  file.copy(files.soil[1],paste0(p.1$w[p.1$n=="BaS.raw1.p"],format(Sys.Date(),"%Y%m%d"),"BaSoil2009Data_TDROnline.dat"))
  file.copy(files.soil[2],paste0(p.1$w[p.1$n=="BaS.raw2.p"],format(Sys.Date(),"%Y%m%d"),"BaSoil2009TDR_WaveOnline.dat"))
  file.copy(files.soil[3],paste0(p.1$w[p.1$n=="BaS.raw3.p"],format(Sys.Date(),"%Y%m%d"),"BaSoil2009TempOnline.dat"))
  cat("\n#\n# copy/save 3 BaSoil2009 files\n#\n")
}else{cat("\n#\n# Basoil files not saved \n#")}


#############################################################################
#   BaMet2015
#############################################################################
#
# wie oben bei BaSoil2009
#
files.met      <-list.files(paste0(p.1$w[p.1$n=="BaMet.onl.p"]),pattern=".dat", full.names = TRUE)
schon.da.met   <-length(list.files(paste0(p.1$w[p.1$n=="BaMet.raw.p"],"Bk2/"),pattern=paste(format(Sys.Date(),"%Y%m")), full.names = TRUE))

if(mon==3&schon.da.met==0|mon==6&schon.da.met==0|mon==9&schon.da.met==0|mon==12&schon.da.met==0){
  # alle 3 Monate wird eine Sicherungskopie mit Datum in Raw gespeichert
  file.copy(files.met[3],paste0(paste0(p.1$w[p.1$n=="BaMet.raw.p"],"bk2/"),format(Sys.Date(),"%Y%m%d"),"_BaMet2010_bk2Online.dat"))
  cat("\n#\n# copy/save 1 BaMet2015 file\n#")
}else{cat("\n#\n# BaMet file not saved \n#\n")}

#############################################################################
#   BaHole2015
#############################################################################
# RBR temperature chain (RUSKIN)
# hier werden nur die realtime-daten nach Potsdam kopiert ... davon ist keine Archivierung noetig
# da monatlich ein manueller download der richtigen Datenbank erfolgen muss ... der dann auch die realtimedaten enthaelt
# die realtime-daten ueberbrueken quasi den monatlichen "Leerlauf" der echten Daten
#
# hier waere eine funktion gut die checkt ob:
# 1. daten im realtime-file vorhanden sind (wenn der Spitzbergenrechner gerebootet wurde laeuft das system nicht automatisch an)
# ... da muesste eine meldung kommen, "bitte Ruskin starten"
# 2. wenn etwa 1 Monat vergangen ist, dann soll jemand den manuellen download machen
# ... ab da muesste dann taeglich eine erinnerung kommen

# realtime.files<-dir(paste0(p.1$w[p.1$n=="BaHo.onl.p"],"/realtime_rsk"), pattern = glob2rx("*.rsk"))
# start.tach<-as.numeric(substr(as.character(realtime.files[length(realtime.files)]),7,14))
# start.zeit<-as.numeric(substr(as.character(realtime.files[length(realtime.files)]),16,17))
# db <- dbConnect(SQLite(), dbname=paste0(paste0(p.1$w[p.1$n=="BaHo.onl.p"],"/realtime_rsk/"),realtime.files[length(realtime.files)]))
# last.entry<-paste(strptime(paste0(start.tach,start.zeit),format='%Y%m%d%H')+(length(dbReadTable(db, "data")[,1])*60*60))
# cat("\n#\n# BaHole2015 last value at",last.entry,"\n#\n")

















