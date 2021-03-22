#############################################################################
##
##   SaPond2014          Level1
##
##   no extrems,
##
##   by: Stephan.Lange@awi.de
##   modified: 2016/09/07
##    
##
#############################################################################
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
  source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}else{
  path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8") 
  maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
  source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
}
#############################################################################



options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
origin="1970-01-01"

jahr=c(2006:2014)

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(70)
s.w.cols <-c(colorRampPalette(c("skyblue","slateblue4"))(4),"#CDCD00",colorRampPalette(c("sandybrown","peachpuff4"))(3))

mon.cols<-c("slategray2","skyblue1","lightgreen","olivedrab2","olivedrab4","orangered","red","red4","darkgoldenrod1","tan4","gray40","steelblue4")
#col2hex(s.w.cols)#library(gplots)
p.width=420*3.5;p.height=280*3.5
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)


for (jahro in jahr){
  
  db.sapond.lvl1<-read.table(paste0(path$w[path$n=="LV1.p"],"SaPond2006/00_full_dataset/SaPond2006_",jahro,"_lv1.dat"),sep=",",dec=".",header=T, fill = TRUE) 


  xxlim = c(as.numeric(strptime(paste0("13.01.",jahro),format="%d.%m.%Y")), as.numeric(strptime(paste0("20.12.",jahro),format="%d.%m.%Y")))
  lischt<-c(db.sapond.lvl1$UTC[format(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%d %H:%M")=="01 00:00"],db.sapond.lvl1$UTC[length(db.sapond.lvl1$UTC)])
  
  
  
  #  water temperatures
  # -----------------------------------------------------
  
  png(paste(path$w[path$n=="plot.p"],jahro,"/SaPond2006_Tw_center_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
  plot(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Ts_0, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
  for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
  ##87CEEB:40cm|@#719DCB:30cm|@#5C6CAB:20cm|@#473C8B:10cm
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_5,col="#473C8B",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_10,col="#5C6CAB",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_15,col="#719DCB",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_fx_cen_67,col="#87CEEB",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_fx_cen_86,col="#8FBC8F",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_fx_cen_101,col="#556B2F",pch=20)
  
  
  axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
  dev.off()
  #  water temperatures
  # -----------------------------------------------------
  
  png(paste(path$w[path$n=="plot.p"],jahro,"/SaPond2006_Tw_edge_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
  plot(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Ts_0, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
  for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
  #@#719DCB:30cm|@#5C6CAB:20cm|@#473C8B:10cm
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_fx_edg_0,col="#473C8B",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_fx_edg_20,col="#5C6CAB",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Tw_fx_edg_33,col="#719DCB",pch=20)

  axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
  dev.off()
  #  soil temperatures
  # -----------------------------------------------------
  
  png(paste(path$w[path$n=="plot.p"],jahro,"/SaPond2006_temperature_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
  plot(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Ts_0, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
  for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
  #F4A460:10cm|@#BF8D62:20cm|@#8B7765:30cm|   
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Ts_0,col="#F4A460",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Ts_16,col="#BF8D62",pch=20)
  points(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$Ts_33,col="#8B7765",pch=20)
  
  
  axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
  dev.off()
  
  #  snow depth
  # -----------------------------------------------------
  
  png(paste0(path$w[path$n=="plot.p"],jahro,"/SaPond2006_Dsn_",jahro,".png"),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0))
  plot(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$distcor, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(0,1.8), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
  for(ll in seq(0,1.8,0.2)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-0.1,1.82),col="gray80")} # vertical lines
  lines(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),
        0.73-db.sapond.lvl1$distcor, pch = 20, cex.lab = 1.5,    col="snow4")      
  axis(2, at=seq(0,1.8,0.2),labels=seq(0,1.8,0.2), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(1.7,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0.1,jahro, las=2,cex=6)
  
  
  dev.off() #
  
  
  #  water table
  # -----------------------------------------------------
  
  png(paste0(path$w[path$n=="plot.p"],jahro,"/SaPond2006_WT_",jahro,".png"),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0))
  plot(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.sapond.lvl1$WT, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(0,1.8), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
  for(ll in seq(0,1.8,0.2)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-0.1,1.82),col="gray80")} # vertical lines
  lines(as.numeric(strptime(db.sapond.lvl1$UTC,format="%Y-%m-%d %H:%M")),
        0.73-db.sapond.lvl1$WT, pch = 20, cex.lab = 1.5,    col="snow4")      
  axis(2, at=seq(0,1.8,0.2),labels=seq(0,1.8,0.2), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(1.7,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0.1,jahro, las=2,cex=6)
  
  
  dev.off() #
  
  
  
  
  
  
  
  
  #  radiation Netto
  # -----------------------------------------------------
  
  trad_gut1  <- which(db.sapond.lvl1$NetRad_1_fl==0)
  trad_gut2  <- which(db.sapond.lvl1$NetRad_2_fl==0)
  
  png(paste(path$w[path$n=="plot.p"],jahro,"/SaPond2006_rad_net1_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
  plot(as.numeric(strptime(db.sapond.lvl1$UTC[trad_gut1],format="%Y-%m-%d %H:%M")),db.sapond.lvl1$NetRad_1[trad_gut1], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(-200,720), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
  for(ll in seq(-200,600,100)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-210,720),col="gray80")} # vertical lines
  points(as.numeric(strptime(db.sapond.lvl1[trad_gut1,1],format="%Y-%m-%d %H:%M")),
         db.sapond.lvl1$NetRad_1[trad_gut1],col="#8B1C62",pch=20)
  
  axis(2, at=seq(-200,600,100),labels=seq(-200,600,100), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(700,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,600,jahro, las=2,cex=6)
  dev.off()
 
   png(paste(path$w[path$n=="plot.p"],jahro,"/SaPond2006_rad_net2_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
  plot(as.numeric(strptime(db.sapond.lvl1$UTC[trad_gut2],format="%Y-%m-%d %H:%M")),db.sapond.lvl1$NetRad_2[trad_gut2], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
       xlim=xxlim, ylim=c(-200,720), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
  for(ll in seq(-200,600,100)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-210,720),col="gray80")} # vertical lines
  points(as.numeric(strptime(db.sapond.lvl1[trad_gut2,1],format="%Y-%m-%d %H:%M")),
         db.sapond.lvl1$NetRad_2[trad_gut2],col="#8B1C62",pch=20)
  
  axis(2, at=seq(-200,600,100),labels=seq(-200,600,100), las=2,cex.axis=4)
  axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
  text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(700,12),labels=Months, las=2,cex=4)
  text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,600,jahro, las=2,cex=6)
  dev.off()
  
  
  cat("#\n# level1 SaPond2006 ",jahro," plot done!\n#\n")
}
