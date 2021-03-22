#############################################################################
##
##   SaSoil2010          Level1
##
##
##
##   by: Stephan.Lange@awi.de
##   modified: 2016/09/06
##
##
#############################################################################
##  to run this script seperat, you have to uncomment the next 10 lines!
rm(list=ls())
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)
  source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}else{
  path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/sa_maintance.txt",sep="\t",header=T)

  source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}
#############################################################################


options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
origin="1970-01-01"

aktuell<-as.numeric(format(Sys.Date(),"%Y"))

add.flagged.data<-1

#jahr=c(2010:aktuell)
jahr=c(2015:2016)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols.1<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(71)
soil.cols.2<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(275)
#mon.cols<-colorRampPalette(c("dodgerblue4","gold3","firebrick3"))(12)
#mon.cols<-colorRampPalette(c("gray60","olivedrab1","yellow1","tomato1","darkorange1","saddlebrown","dodgerblue3","snow3","gray60"))(13)[c(13,2:11)]
mon.cols<-c("slategray2","skyblue1","lightgreen","olivedrab2","olivedrab4","orangered","red","red4","darkgoldenrod1","tan4","gray40","steelblue4")

p.width=420*3.5;p.height=280*3.5
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)


for (jahro in jahr){

  db.Ts.lvl1 <-read.table(paste0(path$w[path$n=="LV1.p"],"SaSoil2010/00_full_dataset/SaSoil2010_",jahro,"_lv1.dat"),sep=",",dec=".",header=T, fill = TRUE)
  #db.TDR.lvl1<-read.table(paste0(path$w[path$n=="LV1.p"],"SaSoil2010/00_full_dataset/SaSoil2010_TDR_",jahro,"_lv1.dat"),sep=",",dec=".",header=T,na.strings="NA")

  ## yearly LEVEL 1 plots
  #  =========================================================================================================

  xxlim = c(as.numeric(strptime(paste0("13.01.",jahro),format="%d.%m.%Y")), as.numeric(strptime(paste0("20.12.",jahro),format="%d.%m.%Y")))
  lischt<-c(db.Ts.lvl1$UTC[format(strptime(db.Ts.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%d %H:%M")=="01 00:00"],db.Ts.lvl1$UTC[length(db.Ts.lvl1$UTC)])
  tair_gut  <- which(db.Ts.lvl1$Ts_center_1_fl==0)
##
##
#############################################################################
##
##  center
##
#############################################################################


png(paste(path$w[path$n=="plot.p"],jahro,"/SaSoil2010_temp_center_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,5,1,1),omi=c(0,0,0,0))
plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahro)
for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
diefe<-(c(1,5,10,20,30,40))
for(qq in rev(4:9)){
  points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")), # 3 col without soilT (Tpan,G_center,G_rim)
         db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-3]],pch=20)
}
axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
dev.off()
##
##
#############################################################################
##
##  slope
##
#############################################################################

png(paste(path$w[path$n=="plot.p"],jahro,"/SaSoil2010_temp_slope_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,5,1,1),omi=c(0,0,0,0))
plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahro)
for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
diefe<-(c(3,7,16,22,32,42))
for(qq in rev(10:15)){
  points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")), # 9 col without soilTslope (Tpan,G_center,G_rim)
         db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-9]],pch=20)
}
axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
dev.off()
##
##
#############################################################################
##
##  rim
##
#############################################################################

png(paste(path$w[path$n=="plot.p"],jahro,"/SaSoil2010_temp_rim_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,5,1,1),omi=c(0,0,0,0))
plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahro)
for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
diefe<-(c(3,7,16,22,32,42))
for(qq in rev(16:26)){
  points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")), # 15 col without soilTslope (Tpan,G_center,G_rim)
         db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-15]],pch=20)
}
axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
dev.off()




##
##
#############################################################################
##
##  ice wedge
##
#############################################################################

png(paste(path$w[path$n=="plot.p"],jahro,"/SaSoil2010_temp_icewedge_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,5,1,1),omi=c(0,0,0,0))
plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahro)
for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
diefe<-(c(41,61,91,121,151,181,211,241,271))
for(qq in rev(27:35)){
  points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,(qq*2)+1])==0),1],format="%Y-%m-%d %H:%M")), # 26 col without soilT (Tpan,G_center,G_rim)
         db.Ts.lvl1[((db.Ts.lvl1[,(qq*2)+1])==0),(qq)*2],col=soil.cols.2[diefe[qq-26]],pch=20)
}
axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
dev.off()


##
##
#############################################################################
##
##  ice wedge trompete
##
#############################################################################

db.Ts.lvl2<-db.Ts.lvl1
for(val in 27:35){#
  db.Ts.lvl2[which(as.numeric(db.Ts.lvl2[,(val*2)+1])>=1),(val*2)]<-NA     }
  db.basoil.extra<-db.Ts.lvl2[,c(1,c(27:35)*2)]
  db.basoil.extra$monate<-format(as.Date(db.basoil.extra[,1]),format="%m")

  stats.db<-aggregate(db.basoil.extra[,2:10], by=list(db.basoil.extra$monate), FUN=mean, na.rm=TRUE)[2:10]


  y.values<-c(41,61,91,121,151,181,211,241,271)*(-0.01)
  png(paste(path$w[path$n=="plot.p"],jahro,"/SaSoil2010_icewedge_trompete_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
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
  dev.off()

  #############################################################################
  ##
  ##  snowdepth
  ##
  #############################################################################

  sh_zero    <- which(as.numeric(db.Ts.lvl1$Dsn_fl) == 0)
  sh_flags   <- which(as.numeric(db.Ts.lvl1$Dsn_fl) > 0)

  png(paste(path$w[path$n=="plot.p"],jahro,"/SaSoil2010_Dsn_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,5,1,1),omi=c(0,0,0,0))
  plot(as.numeric(strptime(db.Ts.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Dsn, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(0,1.0), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
  plot_maintenance(jahro)
  for(ll in seq(0,1.4,0.250)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,2.55),col="gray80")} # vertical lines

  points(as.numeric(strptime(db.Ts.lvl1$UTC[sh_zero],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Dsn[sh_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7,
         col="palegreen3")
  if(add.flagged.data==1){
  points(as.numeric(strptime(db.Ts.lvl1$UTC[sh_flags],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Dsn[sh_flags], pch = 20, cex.lab = 1.5, cex.axis=1.7,
         col="red")}
  axis(2, at=seq(0,1.0,0.25),labels=seq(0,1.0,0.25), las=2,cex.axis=3)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(1.0,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0.1,jahro, las=2,cex=6)
  dev.off() # ;rm(pr_zero,pr_zero_zero,sh_zero,pr_flags,sh_flags)
  # Snowheight







  cat("#\n# level1 SaSoil2010 ",jahro," plot done!\n#\n")
}
