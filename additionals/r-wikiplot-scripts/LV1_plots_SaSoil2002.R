###...........................................................................
##
#     wikiplots    SaSoil2002       --------------------
##   
##   see results here
##   http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:samoylov:soil:tsoil:sasoil2002
##
##   by: Stephan.Lange@awi.de
##   modified: 2021-05-07
##
##   changes:
##   2021-05-07 SL adapted to refresh app and git
##   
###...........................................................................
## to run this script separately, you have to uncomment the next 10 lines!
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
###...........................................................................


options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
origin="1970-01-01"

# run.year<-2002:2021
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols.1<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(71)
soil.cols.2<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(275)
#mon.cols<-colorRampPalette(c("dodgerblue4","gold3","firebrick3"))(12)
#mon.cols<-colorRampPalette(c("gray60","olivedrab1","yellow1","tomato1","darkorange1","saddlebrown","dodgerblue3","snow3","gray60"))(13)[c(13,2:11)]
mon.cols<-c("slategray2","skyblue1","lightgreen","olivedrab2","olivedrab4","orangered","red","red4","darkgoldenrod1","tan4","gray40","steelblue4")

p.width=420*3.5;p.height=280*3.5
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)

zack=1
for (jahro in as.numeric(run.year)){
  if(zack==1){
    ###...........................................................................
    ##
    ##  00.1 load data ----
    ##
    ###...........................................................................
    
    
    db.Ts.lvl1 <-read.table(paste0(p.1$w[p.1$n=="LV1.p"],"SaSoil2002/00_full_dataset/SaSoil2002_",jahro,"_lv1.dat"),sep=",",dec=".",header=T, fill = TRUE)
    
    #  db.TDR.lvl1<-read.table(paste0(p.1$w[p.1$n=="LV1.p"],"SaSoil2002/00_full_dataset/SaSoil2002_TDR_",jahro,"_lv1.dat"),sep=",",dec=".",header=T,na.strings="NA")
    
    ## yearly LEVEL 1 plots
    ###...........................................................................
    
    xxlim = c(as.numeric(strptime(paste0("13.01.",jahro),format="%d.%m.%Y")), as.numeric(strptime(paste0("20.12.",jahro),format="%d.%m.%Y")))
    lischt<-c(db.Ts.lvl1$UTC[format(strptime(db.Ts.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%d %H:%M")=="01 00:00"],db.Ts.lvl1$UTC[length(db.Ts.lvl1$UTC)])
    tair_gut  <- which(db.Ts.lvl1$Ts_center_1_fl==0)
  }# Load data
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  01.1 Ts center ----
    ##
    ###...........................................................................
    
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_temp_center_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,8,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(1,5,10,20,30,40))
    for(qq in 3:8){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-2]],pch=20)
    }
    axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 01.1 Ts center
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  01.2 Ts slope ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_temp_slope_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(3,7,16,22,32,42))
    for(qq in 9:14){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-8]],pch=20)
    }
    axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 01.2 Ts slope
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  01.3 Ts rim ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_temp_rim_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(2,6,11,16,21,27,33,38,51,61,71))
    for(qq in 15:25){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-14]],pch=20)
    }
    axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 01.3 Ts rim
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  01.41 Ts ice wedge ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_temp_icewedge_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(41,61,91,121,151,181,211,241,271))
    for(qq in 26:34){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.2[diefe[qq-25]],pch=20)
    }
    axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 01.41 Ts ice wedge
  
  if(zack==1){
    
    ##
    ##
    ###...........................................................................
    ##
    ##  01.42 Ts ice wedge trompete ----
    ##
    ###...........................................................................
    
    db.Ts.lvl2<-db.Ts.lvl1
    for(val in 26:34){#
      db.Ts.lvl2[which(as.numeric(db.Ts.lvl2[,(val*2)+1])>=1),(val*2)]<-NA     }
    db.basoil.extra<-db.Ts.lvl2[,c(1,c(26:34)*2)]
    db.basoil.extra$monate<-format(as.Date(db.basoil.extra[,1]),format="%m")
    
    stats.db<-aggregate(db.basoil.extra[,2:10], by=list(db.basoil.extra$monate), FUN=mean, na.rm=TRUE)[2:10]
    y.values<-c(41,61,91,121,151,181,211,241,271)*(-0.01)
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_icewedge_trompete_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,8,1,1),omi=c(0,0,0,0))
    plot(c(stats.db[1,]),y.values,type="n",xlim=c(-30,10),ylim=c(-3,1),
         xlab="", ylab = "",xaxt="n", yaxt="n",cex.axis=3)
    for(ll in seq(-10,0,1)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in seq(-30,10,2.5)){lines(c(pp,pp),c(-10,0.2),col="gray80")} # vertical lines
    for(qqq in 1:12){lines(c(stats.db[qqq,1:9]),y.values,col=mon.cols[qqq],lwd=2)}
    
    text(seq(-30,10,2.5),rep(0.8,17),labels=seq(-30,10,2.5), las=2,cex=4)
    axis(2, at=seq(-30,30,1),labels=seq(-30,30,1), las=2,cex.axis=4)
    legend(7,-1.5,months,col=mon.cols,lty=1,cex=2,lwd=4)
    text(7,-0.5,jahro, las=2,cex=6)
    dev.off()#  ice wedge trompete
    
  }# 01.42 Ts ice wedge trompete
  
  if(zack==1){
    ###...........................................................................
    ##
    ##  02 snowdepth ----
    ##
    ###...........................................................................
    
    sh_zero    <- which(as.numeric(db.Ts.lvl1$Dsn_fl) == 0)
    sh_flags   <- which(as.numeric(db.Ts.lvl1$Dsn_fl) > 0)
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_Dsn_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Dsn, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(0,1.0), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(0,1.4,0.250)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,2.55),col="gray80")} # vertical lines
    
    points(as.numeric(strptime(db.Ts.lvl1$UTC[sh_zero],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Dsn[sh_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7,
           col="palegreen3")
    axis(2, at=seq(0,1.0,0.25),labels=seq(0,1.0,0.25), las=2,cex.axis=3)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(1.0,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0.1,jahro, las=2,cex=6)
    dev.off() # ;rm(pr_zero,pr_zero_zero,sh_zero,pr_flags,sh_flags)
  }# 02 snowdepth
  
  if(zack==1){
    ###...........................................................................
    ##
    ##  03 heatflux ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_hf_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-40,80), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(-40,80,10)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-40,80),col="gray80")} # vertical lines
    
    points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,5])==0),1],format="%Y-%m-%d %H:%M")),
           db.Ts.lvl1[((db.Ts.lvl1[,5])==0),4],col="chartreuse",pch=20)
    points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,3])==0),1],format="%Y-%m-%d %H:%M")),
           db.Ts.lvl1[((db.Ts.lvl1[,3])==0),2],col="purple",pch=20)
    
    axis(2, at=seq(-40,80,10),labels=seq(-40,80,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(80,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,-30,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
    
  }# 03 heatflux ...
  
  if(zack==1){
    ###...........................................................................
    ##
    ##  04.1 Dielectricity center ----
    ##
    ###...........................................................................
    y.max<-90
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_E2_center_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,8,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(0,y.max), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(0,y.max,5)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,y.max),col="gray80")} # vertical lines
    diefe<-(c(5,10,20,30,40))
    for(qq in 37:41){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-36]],pch=20)
      # points(as.numeric(strptime(db.Ts.lvl1[,1],format="%Y-%m-%d %H:%M")),
      #        db.Ts.lvl1[,qq*2],col=soil.cols.1[diefe[qq-76]],pch=20)
    }
    axis(2, at=seq(0,y.max,5),labels=seq(0,y.max,5), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(y.max-2,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[2],format="%Y-%m-%d %H:%M"))+2000000,y.max-15,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 04.1 Dielectricity center
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  04.2 Dielectricity slope ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_E2_slope_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_center_1[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(0,y.max), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(0,y.max,5)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,y.max),col="gray80")} # vertical lines
    diefe<-(c(3,7,16,22,32,42))
    for(qq in 42:46){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-41]],pch=20)
      # points(as.numeric(strptime(db.Ts.lvl1[,1],format="%Y-%m-%d %H:%M")),
      #        db.Ts.lvl1[,qq*2],col=soil.cols.1[diefe[qq-81]],pch=20)
      
    }
    axis(2, at=seq(0,y.max,5),labels=seq(0,y.max,5), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(y.max-2,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[2],format="%Y-%m-%d %H:%M"))+2000000,y.max-15,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 04.2 Dielectricity slope
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  04.3 Dielectricity rim ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_E2_rim_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(0,y.max), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(0,y.max,5)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,y.max),col="gray80")} # vertical lines
    diefe<-(c(6,11,16,21,27,33,38,51,61,71))
    for(qq in 47:56){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-46]],pch=20)
      # points(as.numeric(strptime(db.Ts.lvl1[,1],format="%Y-%m-%d %H:%M")),
      #        db.Ts.lvl1[,qq*2],col=soil.cols.1[diefe[qq-86]],pch=20)
      
    }
    axis(2, at=seq(0,y.max,5),labels=seq(0,y.max,5), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(y.max-2,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[2],format="%Y-%m-%d %H:%M"))+2000000,y.max-15,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
    ##  
  }# 04.3 Dielectricity rim
  
  if(zack==1){
    
    ###...........................................................................
    ##
    ##  05.1 VWC center ----
    ##
    ###...........................................................................
    
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_vwc_center_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,8,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-0.03,1), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(0,1,.05)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(5,10,20,30,40))
    for(qq in 77:81){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-76]],pch=20)
      # points(as.numeric(strptime(db.Ts.lvl1[,1],format="%Y-%m-%d %H:%M")),
      #        db.Ts.lvl1[,qq*2],col=soil.cols.1[diefe[qq-76]],pch=20)
    }
    axis(2, at=seq(0,1,.05),labels=seq(0,1,.05), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(1,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 05.1 VWC center
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  05.2 VWC slope ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_vwc_slope_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_center_1[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-0.03,1), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(0,1,.05)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(3,7,16,22,32,42))
    for(qq in 82:86){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-81]],pch=20)
      # points(as.numeric(strptime(db.Ts.lvl1[,1],format="%Y-%m-%d %H:%M")),
      #        db.Ts.lvl1[,qq*2],col=soil.cols.1[diefe[qq-81]],pch=20)
      
    }
    axis(2, at=seq(0,1,.05),labels=seq(0,1,.05), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(1,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 05.2 VWC slope
  
  if(zack==1){
    ##
    ##
    ###...........................................................................
    ##
    ##  05.3 VWC rim ----
    ##
    ###...........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2002_vwc_rim_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-0.03,1), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(0,1,.05)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(6,11,16,21,27,33,38,51,61,71))
    for(qq in 87:96){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-86]],pch=20)
      # points(as.numeric(strptime(db.Ts.lvl1[,1],format="%Y-%m-%d %H:%M")),
      #        db.Ts.lvl1[,qq*2],col=soil.cols.1[diefe[qq-86]],pch=20)
      
    }
    axis(2, at=seq(0,1,.05),labels=seq(0,1,.05), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(1,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
    ##
  }# 05.3 VWC rim
  
  
  
  cat("#\n# level1 SaSoil2002 ",jahro," plot done!\n#\n")
}
