###..........................................................................
##
##   SdHole2009          Level1 wikiplots -----
##
##   
##
##   by: Stephan.Lange@awi.de and Niko.Bornemann@awi.de
##
##   last modified: ----
##     2021-05-12 SL adapted to git, runner app and new path structure
##
###..........................................................................
# to run this script separately, you have to uncomment the next 10 lines!
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
###..........................................................................


options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
origin="1970-01-01"
aktuell <-as.numeric(format(Sys.Date(),"%Y"))
#run.year = c(2009:aktuell)
#run.year = 2018
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(270)
#mon.cols<-colorRampPalette(c("dodgerblue4","gold3","firebrick3"))(12)
mon.cols<-colorRampPalette(c("gray60","olivedrab1","yellow1","tomato1","darkorange1","saddlebrown","dodgerblue3","snow3","gray60"))(13)[c(13,2:25)]
mon.cols<-c("slategray2","skyblue1","lightgreen","olivedrab2","olivedrab4","orangered","red","red4","darkgoldenrod1","tan4","gray40","steelblue4")

p.width=420*3.5;p.height=280*3.5
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)

## loop over years ----
for (jahro in run.year){
## load level1 data ----
  db.bahole.lvl1<-read.table(paste(p.1$w[p.1$n=="LV1.p"],"SdHole2009/00_full_dataset/SdHole2009_",jahro,"_lv1.dat",sep=""),sep=",",dec=".",header=T, fill = TRUE)
  db.bahole.lvl2<-db.bahole.lvl1
  for(val in 2:24){# set data to NA if flag is not 0
  db.bahole.lvl2[which(as.numeric(db.bahole.lvl2[,(val*2)+1])>=1),(val*2)]<-NA
  }
  db.bahole.extra<-db.bahole.lvl2[,c(1,c(2:24)*2)]
  db.bahole.extra$monate<-format(as.Date(db.bahole.extra[,1]),format="%m")
#   j.min <-apply(db.bahole.extra[,2:25],2,FUN=min, na.rm=TRUE)
#   j.max <-apply(db.bahole.extra[,2:25],2,FUN=max, na.rm=TRUE)
#   j.mean<-apply(db.bahole.extra[,2:25],2,FUN=mean, na.rm=TRUE)
  stats.db<-aggregate(db.bahole.extra[,2:24], by=list(db.bahole.extra$monate), FUN=mean, na.rm=TRUE)[2:24]

  y.values<-c(0,-1*(c(1:19,21,23,25,27)-.25))
  y.values<-c(-1*(c(0.4,0.8,1.2,1.6,2,2.5,3,4,5,7,8,11,13,15,20,30,40,50,60,70,80,90,100)))
  
  
  ### plot trompete -----
  png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SdHole2009_trompete_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0))
  plot(c(stats.db[1,]),y.values,type="n",xlim=c(-20,8),ylim=c(-27,5),
       xlab="", ylab = "",xaxt="n", yaxt="n",cex.axis=3)
  for(ll in seq(-30,0,5)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in seq(-20,15,2.5)){lines(c(pp,pp),c(-30,1),col="gray80")} # vertical lines
  for(qqq in 1:12){lines(c(stats.db[qqq,1:23]),y.values,col=mon.cols[qqq],lwd=2)}
#   lines(j.min,y.values,col="purple",lwd=3)
#   lines(j.max,y.values,col="purple",lwd=3)
#   lines(j.mean,y.values,col="purple",lwd=3)
  text(seq(-20,15,2.5),rep(2,13),labels=seq(-20,15,2.5), las=2,cex=4)
  axis(2, at=seq(-30,30,5),labels=seq(-30,30,5), las=2,cex.axis=4)
  legend(7,-16,months,col=mon.cols,lty=1,cex=2,lwd=4)
  text(5,-23,jahro, las=2,cex=6)
  dev.off()


  ###..........................................................................
  ## plot temperatures ----
  xxlim = c(as.numeric(strptime(paste0("13.01.",jahro),format="%d.%m.%Y")), as.numeric(strptime(paste0("20.12.",jahro),format="%d.%m.%Y")))
  lischt<-c(db.bahole.lvl1$UTC[format(strptime(db.bahole.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%d %H:%M")=="01 00:00"],db.bahole.lvl1$UTC[length(db.bahole.lvl1$UTC)])
  tair_gut  <- which(db.bahole.lvl1$Ts_0_fl==0)

  
  png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SdHole2009_Ts_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,5,1,1),omi=c(0,0,0,0))
  plot(as.numeric(strptime(db.bahole.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.bahole.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
  #plot_maintenance(jahr)
  for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
  #diefe<-c(1,5,10,15,25,35,55,75,90)
  diefe<-c(1,c(1:18,20,22,24,26)*10)

  for(qq in rev(2:24)){
    points(as.numeric(strptime(db.bahole.lvl1[((db.bahole.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
           db.bahole.lvl1[((db.bahole.lvl1[,qq*2+1])==0),qq*2],col=soil.cols[diefe[qq]],pch=20)
  }
  axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
  #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
  dev.off()


  cat("#\n# level1 SdHole2009 ",jahro," plot done!\n#\n")
}
