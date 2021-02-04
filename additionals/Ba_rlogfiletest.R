#############################################################################
##
##   log-test-script
##   output goes into wiki
##   ( daily run after DB-update )
##
##   by: Stephan.Lange@awi.de
##   modified: 2016/05/26
##    
##
#############################################################################

if (.Platform$OS.type == "windows") {
  path<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  p.1<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  p.1maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  
  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
}else{
  path<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8") 
  maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  p.1<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8") 
  p.1maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  
  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}

logg=1




if(logg==1){
  logfil <- file(paste0(path$w[path$n=="wiki.p"],"observatory/data/analysis/bayelva/met/control.txt"))
  sink(logfil, append=TRUE)
  sink(logfil, append=TRUE, type="message")
}

cat("<columns 100% 50% ->\n")
cat("====== Welcome to Bayelva! ====== \n")
cat("last check: ",paste(Sys.time()),"\n\n")
cat("===== Choose dataset on the right ===== \n")
cat("|@Tomato:If you see any <fc #9E241F>red</fc> box, please contact --- [[stephan.lange@awi.de|Stephan Lange]]| \n") 
cat("| | \n") 
cat("|@Tomato:<fc Gold>(Mr. Moustache is currently in maintenance)</fc>| \n \n") 


aktuell <- round(as.numeric(Sys.time()),-2)# the last full or half hour
timediff<-60*60*24*1.5 # 1.5 Tage
jahr    <- as.numeric(format(Sys.Date(),"%Y"))# this year
# read all processed data
snow  <-read.table(paste0(path$w[path$n=="LV1.p"],"BaSnow2013/00_full_dataset/BaSnow2013_",jahr,"_lv1.dat"),sep=",",dec=".",header=T, fill = TRUE)
bahole<-read.table(paste0(path$w[path$n=="LV1.p"],"BaHole2009/00_full_dataset/BaHole2009_",jahr,"_lv1.dat"),sep=",",dec=".",header=T, fill = TRUE) 
bamet <-read.table(paste0(path$w[path$n=="LV1.p"], "BaMet2009/00_full_dataset/BaMet2009_" ,jahr,"_lv1.dat")  ,sep=",",dec=".",header=T, fill = TRUE) 
tsoil <-read.table(paste0(path$w[path$n=="LV1.p"],"BaSoil2009/00_full_dataset/BaSoil2009_",jahr,"_lv1.dat"),sep=",",dec=".",header=T,na.strings="NA")[1:17]
tdr   <-read.table(paste0(path$w[path$n=="LV1.p"],"BaSoil2009/00_full_dataset/BaSoil2009_",jahr,"_lv1.dat"),sep=",",dec=".",header=T,na.strings="NA")[-(2:17)]
# convert all dates to numeric 
snow[,1]  <-as.numeric(as.POSIXct(snow[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M'))
bahole[,1]<-as.numeric(as.POSIXct(bahole[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M'))
bamet[,1] <-as.numeric(as.POSIXct(bamet[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M'))
tsoil[,1] <-as.numeric(as.POSIXct(tsoil[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M'))
tdr[,1]   <-as.numeric(as.POSIXct(tdr[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M'))
# shrink dataset to length off existing data (remove all empty lines)
snow   <- snow[complete.cases(snow),]
if((snow[length(snow[,1]),1])<=(aktuell-timediff)) snowtime=1 else snowtime=0
if(length(snow[(length(snow[,1])-23):length(snow[,1]),3]==0)==24) snowflag=0 else snowflag=1

bahole <- bahole[complete.cases(bahole[,-10]),]  
if(length(bahole[,1])==0){baholetime=1;baholeflag=1}else{
if((bahole[length(bahole[,1]),1])<=(aktuell-(timediff*20))) baholetime=1 else baholetime=0
if(length(bahole[(length(bahole[,1])-23):length(bahole[,1]),c(seq(3,21,by=2))]==0)==240) baholeflag=0 else baholeflag=1
}
bamet  <- bamet[complete.cases(bamet[,1:2]),]  
if((bamet[length(bamet[,1]),1])<=(aktuell-timediff)) bamettime=1 else bamettime=0
if(sum(bamet[(length(bamet[,1])-23):length(bamet[,1]),c(9,11,23)])==0) climflag=0 else climflag=1       # hum temp pr
if(sum(bamet[(length(bamet[,1])-23):length(bamet[,1]),c(13,15,17,19)])==0) windflag=0 else windflag=1   # wind
if(sum(bamet[(length(bamet[,1])-23):length(bamet[,1]),c(seq(51,61,by=2))])==0) radflag=0 else radflag=1 # radiation (without albedo)
if(sum(bamet[(length(bamet[,1])-23):length(bamet[,1]),63])==0) albflag=0 else albflag=1                 # albedo 

tsoil  <- tsoil[complete.cases(tsoil),]  
if((tsoil[length(tsoil[,1]),1])<=(aktuell-timediff)) tsoiltime=1 else tsoiltime=0

tdr    <- tdr[complete.cases(tdr),]  
if((tdr[length(tdr[,1]),1])<=(aktuell-timediff)) tdrtime=1 else tdrtime=0





cat("<newcolumn 50%>\n")
cat("^ \ \ daily \ \ update")
###########################################################################################################
##
##  temp hum prec
##
if(bamettime==1){cat("|@Tomato:[[observatory:data:analysis:bayelva:met:temperature|Climate (T / H / P)]]")
}else if(climflag==1){  cat("|@Gold:[[observatory:data:analysis:bayelva:met:temperature|Climate (T / H / P)]]")
}else{  cat("|@lightgreen:[[observatory:data:analysis:bayelva:met:temperature|Climate (T / H / P)]]")}

###########################################################################################################
##
##  snow temp
##
if(bamettime==1){cat("|@Tomato:[[observatory:data:analysis:bayelva:met:snowtemp|Snow T]]")
}else{  cat("|@lightgreen:[[observatory:data:analysis:bayelva:met:snowtemp|Snow T]]")}

###########################################################################################################
##
##  soil temp
##
if(tsoiltime==1)       {  cat("|@Tomato:[[observatory:data:analysis:bayelva:met:surftemp|Surface T]]")
}else                  {  cat("|@lightgreen:[[observatory:data:analysis:bayelva:met:surftemp|Surface T]]")}

###########################################################################################################
##
##  Mrmoustache
##
if(baholetime==1)      {  cat("|@Tomato:[[observatory:data:analysis:bayelva:soil:borehole|Mr. Moustache]] |\n")
}else if(baholeflag==1){  cat("|@Gold:[[observatory:data:analysis:bayelva:soil:borehole|Mr. Moustache]] |\n")
}else                  {  cat("|@lightgreen:[[observatory:data:analysis:bayelva:soil:borehole|Mr. Moustache]] |\n")}

cat("^ ::: ")
###########################################################################################################
##
##  Radiation
##
if(bamettime==1)    {  cat("|@Tomato:[[observatory:data:analysis:bayelva:met:rad|Radiation]]")
}else if(radflag==1){  cat("|@Gold:[[observatory:data:analysis:bayelva:met:rad|Radiation]]")
}else               {  cat("|@lightgreen:[[observatory:data:analysis:bayelva:met:rad|Radiation]]")}

###########################################################################################################
##
##  snow depth
##
if(snowtime==1)      {  cat("|@Tomato: [[observatory:data:analysis:bayelva:met:snow|Snow depth]]")
}else if(snowflag==1){  cat("|@Gold: [[observatory:data:analysis:bayelva:met:snow|Snow depth]]")
}else                {  cat("|@lightgreen: [[observatory:data:analysis:bayelva:met:snow|Snow depth]]")}

###########################################################################################################
##
##  soil temp
##
if(tsoiltime==1)       {  cat("|@Tomato:[[observatory:data:analysis:bayelva:soil:soiltemp|Soil T]]")
}else                  {  cat("|@lightgreen:[[observatory:data:analysis:bayelva:soil:soiltemp|Soil T]]")}

###########################################################################################################
##
##  ... 
##
cat("|@lightgreen:[[observatory:data:analysis:bayelva:soil:SH_temp|Soil T vs Snow depth]]  |\n")
cat("^ ::: ")
###########################################################################################################
##
##  wind 
##
if(bamettime==1)     {  cat("|@Tomato: [[observatory:data:analysis:bayelva:met:wind|Wind]]")
}else if(windflag==1){  cat("|@Gold: [[observatory:data:analysis:bayelva:met:wind|Wind]]")
}else                {  cat("|@lightgreen: [[observatory:data:analysis:bayelva:met:wind|Wind]]")}

cat("|@lightgreen:[[observatory:data:analysis:bayelva:met:snowtdr|Snow TDR]]")

###########################################################################################################
##
##  soil conductivity / vwc
##
if(tdrtime==1)       {  cat("|@Tomato:[[observatory:data:analysis:bayelva:soil:tdrcond|Soil TDR (cond / vwc)]]|@lightgreen:  |\n")
}else                {  cat("|@lightgreen:[[observatory:data:analysis:bayelva:soil:tdrcond|Soil TDR (cond / vwc)]]|@lightgreen:  |\n")}
cat("^ ::: ")

if(albflag==1)       {  cat("|@Gold:[[observatory:data:analysis:bayelva:met:albedo|Albedo]]|@lightgreen:")
}else                {  cat("|@lightgreen:[[observatory:data:analysis:bayelva:met:albedo|Albedo]]|@lightgreen:")}


#cat("|@lightgreen:[[observatory:data:analysis:bayelva:met:albedo|Albedo]]|@lightgreen:")
###########################################################################################################
##
##  soil ice
##
if(tdrtime==1)       {  cat("|@Tomato:[[observatory:data:analysis:bayelva:soil:icecond|Soil TDR (ice / diel)]]")
}else                {  cat("|@lightgreen:[[observatory:data:analysis:bayelva:soil:icecond|Soil TDR (ice / diel)]]")}

#cat("|@lightgreen:[[observatory:data:analysis:bayelva:soil:icecond|Soil Ice / dielectric]]")

cat("|@lightgreen:   |\n")



cat("^ \ \ old \ \ series^ ^ ^ ^ ^\n")
cat("^::: |@lightgreen: |@lightgreen: |@lightgreen:[[observatory:data:analysis:bayelva:soil:temp2d|Soil (2D) '98-'12]] |@lightgreen:[[observatory:data:analysis:bayelva:soil:heatflux|Soil heat flux '98-'12]]  |\n")
cat("^::: |@lightgreen: |@lightgreen: |@lightgreen:Soil TDR ([[observatory:data:analysis:bayelva:soil:cond2d|cond]] / [[observatory:data:analysis:bayelva:soil:vwc2d|vwc]] / [[observatory:data:analysis:bayelva:soil:ice2d|ice]] / [[observatory:data:analysis:bayelva:soil:diel2d|diel]] )|@lightgreen: more??|\n")
cat("^ ^ ^ ^ ^ ^\n")
cat("^ weekly \ \ update |@lightgreen: [[observatory:data:analysis:bayelva:cplots|control plots last week]] |@lightgreen: [[observatory:data:analysis:bayelva:stats|statistics   (mean / NA`s)]] |@lightgreen: [[observatory:data:analysis:bayelva:log|log-files]] | @lightgreen:[[observatory:data:analysis:bayelva:met:control|controling]] |\n\n")
cat("</columns>\n\n\n")
cat("{{url>https://dashboard.awi.de/?dashboard=6190 noscroll, noborder,height=2400px}}\n\n")
if(logg==1){
  sink(type="message")
  sink()
}

