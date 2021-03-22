#############################################################################
##
##   BaHole2006          Level1
##
##  
##
##   by: Stephan.Lange@awi.de
##   modified: 2019/05/16
##    changed file directories (PSc)
##
#############################################################################
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/sparc/LTO/R_database//database_R/settings/sa_maintance.txt",sep="\t",header=T)
  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
}else{
  path<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/sparc/LTO/R_database/database_R/settings/sa_maintance.txt",sep="\t",header=T)
  
  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
#############################################################################
library("png")
library("R.matlab")

setwd("C:/Users/stlange/Desktop/wind/")
options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
origin="1970-01-01"
awi<-readPNG("C:/Users/stlange/Desktop/wind/AWI_WortBildmarke_Farbe_RGB.png")
sparc<-readPNG("C:/Users/stlange/Desktop/wind/sparc_ohnetext_fett.png")
jahr=c(2006:2017)
#jahr=c(2013)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(270)
#mon.cols<-colorRampPalette(c("dodgerblue4","gold3","firebrick3"))(12)
mon.cols<-colorRampPalette(c("gray60","olivedrab1","yellow1","tomato1","darkorange1","saddlebrown","dodgerblue3","snow3","gray60"))(13)[c(13,2:11)]
mon.cols<-c("slategray2","skyblue1","lightgreen","olivedrab2","olivedrab4","orangered","red","red4","darkgoldenrod1","tan4","gray40","steelblue4")

depth<-readMat("N:/geo6/tvc/lake and soil simulations/SAM_CG3RF85/SAM_CG3RF85_settings.mat")$GRID[[4]][[1]]# GRID general cT_GRID
kk<-c(63,seq(100,700,50),  727,747,767,787,807,  816,823,825,827,829)
#kk<-depth[kk]
#utility function for embedding png images at specified fractional sizes in R plots
#places the logo centred on a specified fraction of the the usr space, 
#and sizes appropriately (respects aspect ratio)
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*1.5*size/2), x+(size/2), y+(AR*1.5*size/2), interpolate=TRUE)
}





p.width=420*3.5;p.height=280*3.5
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)

month.db<-matrix(nrow=24,ncol=144,-999)
colnames(month.db)<-c(paste0(rep(2006:2017,each=12),"w",rep(1:12)))
for (jahro in jahr){
  cat("loading", jahro, "\n")
  #db.bahole.lvl1<-read.table(paste(path$w[path$n=="LV1.p"],"SaHole2006/00_full_dataset/SaHole2006_",jahro,"_lv1.dat",sep=""),sep=",",dec=".",header=T, fill = TRUE)
  g<-data.frame(t(readMat(paste0("N:/geo6/tvc/lake and soil simulations/SAM_CG3RF85/SAM_CG3RF85_output",jahro,".mat"))$OUT[[2]]))# cryoGRID
  #date<-readMat(paste0("N:/geo6/tvc/lake and soil simulations/SAM_CG3RF85/SAM_CG3RF85_output",jahro,".mat"))$OUT[[3]]
  #g$mon<-rep(seq())
  start.date <-as.POSIXct(paste(jahro,"-01-01 00:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  end.date   <-as.POSIXct(paste(jahro,"-",12,"-",31," 18:00:00",sep=""),format='%Y-%m-%d %H:%M:%S', tz = "UTC")
  
  g$mon<-as.numeric(format(as.POSIXct(seq(start.date,end.date,length.out=(nrow(g))),format='%Y-%m-%d %H:%M:%S', tz = "UTC"),format='%m'))
#  db.bahole.lvl2<-db.bahole.lvl1
  # for(val in 1:24){# set data to NA if flag is not 0
  #   db.bahole.lvl2[which(as.numeric(db.bahole.lvl2[,(val*2)+1])>=1),(val*2)]<-NA     }
  #   db.bahole.extra<-db.bahole.lvl2[,c(1,c(1:24)*2)]
  #   db.bahole.extra$monate<-format(as.Date(db.bahole.extra[,1]),format="%m")

  # stats.db<-aggregate(db.bahole.extra[,2:25], by=list(db.bahole.extra$monate), FUN=mean, na.rm=TRUE)[2:25]
  # month.db[,(12*(jahro-2006)+1):(12*(jahro-2006)+12)]<-t(stats.db)
  stats.db<-aggregate(g[,kk], by=list(g$mon), FUN=mean, na.rm=TRUE)[2:25]
  month.db[,(12*(jahro-2006)+1):(12*(jahro-2006)+12)]<-t(stats.db)
}
# fill empty columns

month.db[,34]<-rowMeans(cbind(month.db[,34-12],month.db[,34+12]))
month.db[,35]<-rowMeans(cbind(month.db[,35-12],month.db[,35+12]))
month.db[,36]<-rowMeans(cbind(month.db[,36-12],month.db[,36+12]))
month.db[,37]<-rowMeans(cbind(month.db[,37-12],month.db[,37+12]))
month.db[,38]<-rowMeans(cbind(month.db[,38-12],month.db[,38+12]))
month.db[,39]<-rowMeans(cbind(month.db[,39-12],month.db[,39+12]))

min.2006<-apply(month.db[,8:20],1,min)
mean.2006<-apply(month.db[,8:20],1,mean)
max.2006<-apply(month.db[,8:20],1,max)

for(i in 2006:2017){
  cat(mean(na.omit(month.db[20,(((i-2006)*12)+1):(((i-2006)*12)+12)])),"\n")
}



Jahre<-rep(2006:2017,each=12)
Monate<-rep(1:12,12)
for(erna in 8:141){# startet bei 8 : 141
  cat("printing", erna, "\n")
  y.values<-c(0,-1*(c(1:19,21,23,25,27)-.25))
  #png(paste(path$w[path$n=="plot.p"],jahro,"/SaHole2006_trompete_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  #png(paste("C:/Users/stlange/Desktop/wind/cryo/cryo2006_trompete_",jahro,"_",1000+erna,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  png(paste("M:Desktop/cryo/cryo2006_trompete_",jahro,"_",1000+erna,".png",sep=""),width=p.width,height=p.height,pointsize=8)
  
  par(mar=c(1,8,1,1),omi=c(0,0,0,0))
  plot(c(month.db[,1]),y.values,type="n",xlim=c(-16,8),ylim=c(-26,3),
       xlab="", ylab = "",xaxt="n", yaxt="n",cex.axis=3)
  for(ll in seq(-30,0,5)){abline(h=ll,col="gray80")} # horizontal lines
  for(pp in seq(-15,15,2.5)){lines(c(pp,pp),c(-30,1),col="gray80")} # vertical lines

  if(erna%in%c(8)){
    for(qqq in 1:erna){lines(c(month.db[,qqq]),y.values,col="gray80",lwd=2)}
  }else if(erna%in%c(9:18)){
    for(qqq in 1:erna){lines(c(month.db[,qqq]),y.values,col="gray80",lwd=2)}
    polygon(c(apply(month.db[,8:erna],1,min),rev(apply(month.db[,8:erna],1,max))),c(y.values,rev(y.values)),
            col=adjustcolor("salmon2", .1),border = "transparent")
  }else{
    polygon(c(min.2006,rev(max.2006)),c(y.values,rev(y.values)),
            col=adjustcolor("salmon2", .1),border = "transparent")
    lines(min.2006,y.values,col=adjustcolor("salmon2", .3),lwd=3)
    lines(mean.2006,y.values,col=adjustcolor("salmon2", .3),lwd=3)
    lines(max.2006,y.values,col=adjustcolor("salmon2", .3),lwd=3)
    for(qqq in (erna-14):erna){lines(c(month.db[,qqq]),y.values,col="gray80",lwd=2)}

  }
if(erna>8){
  lines(c(month.db[,erna-2]),y.values,col="gray75",lwd=3)
  lines(c(month.db[,erna-1]),y.values,col="gray70",lwd=3)
}
lines(c(month.db[,erna]),  y.values,col="gray95",lwd=6)
lines(c(month.db[,erna]),  y.values,col="gray60",lwd=4)
leg.col<-c(rep("gray80",12))
leg.col[Monate[erna]]<-"gray60"
leg.lwd<-c(rep(4,12))
leg.lwd[Monate[erna]]<-6
leg.cex<-c(rep(2,12))
leg.cex[Monate[erna]]<-3.5
text(seq(-15,15,2.5),rep(2,13),labels=seq(-15,15,2.5), las=2,cex=4)
axis(2, at=seq(-30,30,5),labels=seq(-30,30,5), las=2,cex.axis=4)
legend(7,-16,months,col=leg.col,lty=1,cex=2,lwd=leg.lwd)
if(erna%in%c(13:16)){text(2,-23,"2006",col=adjustcolor("salmon2", (erna-12)/4), las=2,cex=6)}
if(erna>16){text(2,-23,"2006",col="salmon2", las=2,cex=6)}
text(5,-23,Jahre[erna], las=2,cex=6)
text(-0.5,-25.8,"SAM_CG3RF85 27m", las=2,cex=6)
#add.image( 6,-25.8, awi, adj.x=0, adj.y=0)
logoing_func(awi, x=0.915, y=0.05, size=0.15)
#logoing_func(sparc, x=0.075, y=0.11, size=0.12)
#rasterImage(awi,1,2,-3,-1, interpolate=TRUE)
dev.off()

  }
  # 
#  cat("#\n# level1 SaHole2006 ",jahro," plot done!\n#\n")
  
  
  
  
  
  
  
  