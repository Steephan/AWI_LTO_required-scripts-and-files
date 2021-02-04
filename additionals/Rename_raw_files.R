# Rename_raw_files
#
# Be very carefulllllllllll !!!!!!!!!!!
#
#  Erste werden alle Daten Kopiert 
#  Dann nach einer Kontrolle, kann das alte Zeug von Hand geloescht werden
#  (Dies ginge auch automatisch, ist aber explizit nicht so gewollt !!!!)
#
# BaSoil2009, SdHole2009, SaSoil2010, SaMet2010

station  <- "SaSoil2002" # station Folder
filename <- "tdrwave" # name of file to characterise the containing data
folder   <- "03_TDR_wave/08_Originals/temp" # folder within staton folder containing data
wo       <- paste0("n:/sparc/data/LTO/raw/",station,"/",folder,"/")
was      <- list.files(wo ,pattern = ".dat")
#
# values for designer contributing the file format
# 1 == Campbell  z.B. SaSoil2002
# 2 == RBR       z.B. BaHole2009
# 3 == RBR II    z.B. SaHole2006

designer = 1

# copy the old files in other folder

mit.kopieren = 1

origin <- "1970-01-01"
for(i in was){ # Schleife ueber alle dat-files im wo-Ordner
  # nur Einlesen der Datumsspalte
  # ohne Header und ohne die ersten 4 Zeilen (evtl. muss das individuell angepasst werden)
  if(designer==1){    
    spalte.eins     <- read.table(paste0(wo,i),sep=",",dec=".",header=F,skip=4)[1]
    zeile.eins      <- read.table(paste0(wo,i),sep=",",dec=".",header=F,skip=0,fill=T)[1,1:8]
    
    spalte.eins[,1] <- format(as.POSIXct(spalte.eins[,1],format='%Y-%m-%d %H:%M:%S',
                                         origin=origin, tz = "UTC"),format='%Y%m%d%H%M')
    von             <- spalte.eins[1,]             # Zeitreihenbeginn
    anzahl          <- length(spalte.eins[,1])     # Zeitreihenlaenge
    bis             <- spalte.eins[anzahl,]        # Zeitreihenende  
  }else if(designer==2){
    spalte.eins    <- read.table(paste0(wo,i),sep="\t",dec=".",header=F,skip=2,fill=T)
    
    zeile.raw      <- readLines(paste0(wo,i))[1]
    start<-gregexpr(pattern ='#',zeile.raw)[[1]][1]+1
    zeile.eins<-matrix(nrow=1,ncol=4,1)
    zeile.eins[3]<-substr(zeile.raw,1,6)
    zeile.eins[4]<-substr(zeile.raw,start,start+5)
    #  19.11.2016 15:00:00
    
    spalte.eins[,1] <- format(as.POSIXct(spalte.eins[,2],format='%d.%m.%Y %H:%M:%S',
                                         origin=origin, tz = "UTC"),format='%Y%m%d%H%M')
    
    von             <- spalte.eins[1,1]             # Zeitreihenbeginn
    anzahl          <- length(spalte.eins[,1])     # Zeitreihenlaenge
    bis             <- spalte.eins[anzahl,1]        # Zeitreihenende
}else if(designer==3){
  spalte.eins    <- read.table(paste0(wo,i),sep="",dec=".",header=F,skip=110,fill=T)[,1:2]
  
  #zeile.raw      <- readLines(paste0(wo,i))[1]
  #start<-gregexpr(pattern ='#',zeile.raw)[[1]][1]+1
  #zeile.eins<-matrix(nrow=1,ncol=4,1)
  #zeile.eins[3]<-substr(zeile.raw,1,6)
  #zeile.eins[4]<-substr(zeile.raw,start,start+5)
  #  19.11.2016 15:00:00
  
  # spalte.eins[,1] <- format(as.POSIXct(spalte.eins[,2],format='%Y/%m/%d %H:%M:%S',
  #                                      origin=origin, tz = "UTC"),format='%Y%m%d%H%M')
  spalte.eins[,1] <- format(as.POSIXct(paste(spalte.eins[,1],spalte.eins[,2]),format='%Y/%m/%d %H:%M:%S',
                                       origin=origin, tz = "UTC"),format='%Y%m%d%H%M')
  
  von             <- spalte.eins[1,1]             # Zeitreihenbeginn
  anzahl          <- length(spalte.eins[,1])     # Zeitreihenlaenge
  bis             <- spalte.eins[anzahl,1]        # Zeitreihenende
}  
#  wann            <- format(file.info(paste0(wo,i))$mtime,format='%Y%m%d%H%M') # Erstellungszeit (CEST)
  #############################################################################################
  #
  #  teste immer erst ohne die beiden naechsten Zeilen
  #
  #############################################################################################
   if(mit.kopieren==1){ 
     if(designer==1){ 
   neues.file<-paste0(wo,bis,"_",von,"_",station,"_",filename,"_",zeile.eins[1,3],"_",zeile.eins[1,7],"_",zeile.eins[1,4],".dat")

   file.copy(paste0(wo,i),                                       ## alter Name "_",wann,
              neues.file, copy.date = T)
     }else if(designer==2){
       neues.file<-paste0(wo,bis,"_",von,"_",station,"_",zeile.eins[1,3],"_",zeile.eins[1,4],".dat")
       
       file.copy(paste0(wo,i),                                       ## alter Name "_",wann,
                 neues.file, copy.date = T)
     }else if(designer==3){
       neues.file<-paste0(wo,bis,"_",von,"_",station,"_",filename,".dat")
       
       file.copy(paste0(wo,i),                                       ## alter Name "_",wann,
                 neues.file, copy.date = T)
     }
   
}
  #############################################################################################
  if(designer==1){
  cat(  paste0("kopiert: ",bis,"_",von,"_",station,"_",filename,"_",zeile.eins[1,3],"_",zeile.eins[1,7],"_",zeile.eins[1,4],".dat","\n"))
  }else if(designer==2){
  cat(  paste0("kopiert: ",bis,"_",von,"_",station,"_",zeile.eins[1,3],"_",zeile.eins[1,4],".dat","\n"))
  }else if(designer==3){
  cat(  paste0("kopiert: ",bis,"_",von,"_",station,"_",filename,".dat","\n"))
  }
}  
#Sys.setFileTime(wo, tz="UTC")




































