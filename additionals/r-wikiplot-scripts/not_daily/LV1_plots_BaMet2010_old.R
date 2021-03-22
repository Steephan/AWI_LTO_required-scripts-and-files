
############################################################################
#  to run this script seperat, you have to uncomment the next 10 lines!
# rm(list=ls())
# if (.Platform$OS.type == "windows") {
#   path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/path_windoof.txt",sep="\t",header=T)
#   maint<-read.table("N:/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
#   source("N:/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
# }else{
#   path<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
#   maint<-read.table("/geo5/SoilData/doc/scripts/database_R/settings/maintance.txt",sep="\t",header=T)
# 
#   source("/geo5/SoilData/doc/scripts/database_R/settings/db_func.R")
# }
# ###########################################################################


options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
origin="1970-01-01"
years  <- c(2016)#2009:2016
#years  <- c(2013)

zack=1
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


p.width=420*3.5;p.height=280*3.5
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)


for (jahr in years){
  if(zack==1){   
  db.bamet     <-read.table(paste(path$w[path$n=="BaMet.lv1.p"],"00_full_dataset/BaMet2010_",jahr,"_lv1_noflag.dat",sep=""),sep=",",dec=".",header=T, fill = TRUE) 
  db.bamet.lvl1<-read.table(paste(path$w[path$n=="BaMet.lv1.p"],"00_full_dataset/BaMet2010_",jahr,"_lv1.dat",sep=""),sep=",",dec=".",header=T, fill = TRUE) 
  


xxlim = c(as.numeric(strptime(paste0("13.01.",jahr),format="%d.%m.%Y")), as.numeric(strptime(paste0("20.12.",jahr),format="%d.%m.%Y")))

# plotting analysis
# -----------------
# some boundaries
ylim_sw <- plot_bounderies(db.bamet.lvl1$SwOut,db.bamet.lvl1$SwIn)      # get plotting bounderies shortwave
ylim_lw <- plot_bounderies(db.bamet.lvl1$LwOut_co,db.bamet.lvl1$LwIn_co)  # get plotting bounderies longwave
lischt<-c(db.bamet.lvl1$UTC[format(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%d %H:%M")=="01 00:00"],db.bamet.lvl1$UTC[length(db.bamet.lvl1$UTC)])

}# Load data

if(zack==1){ 
#  albedo (from file vs. calculated (Out/In))
# -----------------------------------------------------
albedo_good <- which(db.bamet.lvl1$Albedo_fl == 0)
albedo_bad  <- which(db.bamet.lvl1$Albedo_fl >  1)
ohne.albedo <- which(db.bamet.lvl1$Albedo_fl == 1)

png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_albedo_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Albedo, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-5,5), xlab="Date", ylab = "[W / m]",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(-4,4,1)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-5,5),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[ohne.albedo],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwIn[ohne.albedo]/db.bamet.lvl1$SwOut[ohne.albedo], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       xlim=xxlim, ylim=c(-5,5),col="plum")             # albedo = out/in
points(as.numeric(strptime(db.bamet.lvl1$UTC[albedo_good],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Albedo[albedo_good], pch = 20, cex.lab = 1.5,
       xlim=xxlim, ylim=c(-5,5),col="purple4")             # 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[albedo_bad],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Albedo[albedo_bad], pch = 20, cex.lab = 1.5,
#        xlim=xxlim, ylim=c(-5,5),col="red") 
axis(2, at=c(4,2,0,-2,-4),labels=c(4,2,0,-2,-4), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
 text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(5,12),labels=Months, las=2,cex=4)
 text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,-4.7,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}

# legend("bottom",cex=0.8,title="",lty=1,lwd=3,col=c("purple4","plum"),y.intersp=0.8,
#        box.col = "white",inset=0.05,seg.len = 0.8,c("    albedo","plus income/outgoing"),bg="white")
dev.off()  ;rm(albedo_bad,ohne.albedo)

}# Albedo

if(zack==1){ 
#  radiation Netto
# -----------------------------------------------------
net_zero  <- which(db.bamet.lvl1$SwOut_fl == 0)
net_flags  <- which(db.bamet.lvl1$SwOut_fl > 0)



png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_rad_net_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwOut, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-200,720), xlab=" ", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(-200,600,100)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-210,720),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[net_zero],format="%Y-%m-%d %H:%M")),
       db.bamet.lvl1$SwOut[net_zero]-db.bamet.lvl1$SwIn[net_zero]+db.bamet.lvl1$LwOut_co[net_zero]-db.bamet.lvl1$LwIn_co[net_zero],
       pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="maroon4")           
# points(as.numeric(strptime(db.rad98$UTC[sw_out],format="%Y-%m-%d %H:%M")),db.rad98$SwIn[sw_out], pch = 20, cex.lab = 1.5,
#        col="khaki3")  
# points(as.numeric(strptime(db.bamet.lvl1$UTC[net_flags],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$rad_net[net_flags], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="red")            
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_bad_out],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwIn[sw_bad_out], pch = 20, cex.lab = 1.5,
#        col="red") 
axis(2, at=seq(-200,600,100),labels=seq(-200,600,100), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(700,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,600,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,350,"reindeer", las=2,srt=60,cex=4,col="gray80")}

# legend("topleft",cex=0.8,title="",lty=1,lwd=3,col=c("mediumpurple3","coral3"),y.intersp=0.8,
#        box.col = "white",inset=0.05,seg.len = 0.8,c("    in","  out"),bg="white")
dev.off()  
}# Radiation netto

if(zack==1){ 
#  radiation Global
# -----------------------------------------------------
png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_rad_gl_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwOut, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-5,720), xlab=" ", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(0,600,100)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-210,720),col="gray80")} # vertical lines
           
points(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwOut, pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="lightsalmon4")             
axis(2, at=seq(0,600,100),labels=seq(0,600,100), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(700,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,600,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,350,"reindeer", las=2,srt=60,cex=4,col="gray80")}

# legend("topleft",cex=0.8,title="",lty=1,lwd=3,col=c("mediumpurple3","coral3"),y.intersp=0.8,
#        box.col = "white",inset=0.05,seg.len = 0.8,c("    in","  out"),bg="white")
dev.off()  
}# Radiation global

if(zack==1){ 
#  radiation shortwave
# -----------------------------------------------------
  sw_in  <- which(db.bamet.lvl1$SwOut_fl == 0)
  sw_out <- which(db.bamet.lvl1$SwIn_fl == 0)
  sw_bad_in  <- which(db.bamet.lvl1$SwOut_fl > 0)
  sw_bad_out <- which(db.bamet.lvl1$SwIn_fl > 0)


png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_rad_sw_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwOut, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-5,720), xlab="Date", ylab = "[W / m]",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(0,600,100)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-20,720),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_in],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwOut[sw_in], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="mediumpurple3")            
points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_out],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwIn[sw_out], pch = 20, cex.lab = 1.5,
       col="khaki3")  
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_bad_in],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwOut[sw_bad_in], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="red")            
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_bad_out],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$SwIn[sw_bad_out], pch = 20, cex.lab = 1.5,
#        col="red") 
axis(2, at=seq(0,600,100),labels=seq(0,600,100), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(700,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,600,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,350,"reindeer", las=2,srt=60,cex=4,col="gray80")}

# legend("topleft",cex=0.8,title="",lty=1,lwd=3,col=c("mediumpurple3","coral3"),y.intersp=0.8,
#        box.col = "white",inset=0.05,seg.len = 0.8,c("    in","  out"),bg="white")
dev.off()  
}# Radiation shortwave

if(zack==1){ 

#  radiation longwave
# ----------------------------------------------------- 
  lw_in  <- which(db.bamet.lvl1$LwOut_co_fl == 0)
  lw_out <- which(db.bamet.lvl1$LwIn_co_fl == 0)
  lw_bad_in   <- which(db.bamet.lvl1$LwOut_co_fl > 0)
  lw_bad_out  <- which(db.bamet.lvl1$LwIn_co_fl > 0)

png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_rad_lw_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$LwOut, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(100,500), xlab="Date", ylab = "[W / m]",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(100,500,100)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-20,720),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_in],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$LwOut_co[lw_in], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="mediumpurple3")            
points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_out],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$LwIn_co[lw_out], pch = 20, cex.lab = 1.5,
       col="khaki3") 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_bad_in],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$LwOut_co[lw_bad_in], pch = 20, cex.lab = 1.5,
#        col="red")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_bad_out],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$LwIn_co[lw_bad_out], pch = 20, cex.lab = 1.5,
#        col="red")
axis(2, at=seq(100,500,100),labels=seq(100,500,100), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(500,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,450,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,300,"reindeer", las=2,srt=60,cex=4,col="gray80")}

# legend("topleft",cex=0.8,title="",lty=1,lwd=3,col=c("mediumpurple3","coral3"),y.intersp=0.8,
#        box.col = "white",inset=0.05,seg.len = 0.8,c("    in","  out"),bg="white")
dev.off()  
}# Radiation longwave

if(zack==1){ 
#  air temperature
# -----------------------------------------------------
#col2hex(soil.cols)#library(gplots)
rr<-length(aggregate(db.bamet$Tair_200~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,2])
murr<-matrix(ncol=4,nrow=rr,1)
murr[,1]<-aggregate(db.bamet$Tair_200~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,1]
murr[,2]<-aggregate(db.bamet$Tair_200~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,2]
# murr[,3]<-aggregate(db.bamet.lvl1$Tair_200~format(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = min)[,2]
# murr[,4]<-aggregate(db.bamet.lvl1$Tair_200~format(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = max)[,2]


air_zero   <- which(as.numeric(db.bamet.lvl1$Tair_200_fl) == 0) 
air_flags   <- which(as.numeric(db.bamet.lvl1$Tair_200_fl) > 0) 

png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_airt_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_200, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-30,25), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[air_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_200[air_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="lightgoldenrod3")  
points(as.numeric(strptime(murr[,1],format="%Y-%m-%d"))+43200,murr[,2], pch = 20, cex.lab = 1.5, cex.axis=1.7, cex=2.5,
       col="darkorange3")  
# points(as.numeric(strptime(db.bamet.lvl1$UTC[air_flags],format="%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$Tair_200[air_flags], pch = 20, cex.lab = 1.5,       col="red")         
axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}

dev.off() ;rm(air_zero,air_flags,rr,murr)
}# Air temperature

if(zack==1){ 
# #  snowheight
# # -----------------------------------------------------
# 
# pr_zero   <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec > 0) 
# pr_zero_zero   <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec == 0) 
# sh_zero   <- which(as.numeric(db.bamet.lvl1$snowh_fl) == 0) 
# pr_flags   <- which(as.numeric(db.bamet.lvl1$prec_fl) > 0) 
# sh_flags   <- which(as.numeric(db.bamet.lvl1$snowh_fl) > 0)  
# 
# png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_sh_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
# par(mar=c(1,5,1,1),omi=c(0,0,0,0)) 
# plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$snowh, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
#      xlim=xxlim, ylim=c(0,2.3), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
# plot_maintenance(jahr)
# for(ll in seq(0,1.5,0.250)){abline(h=ll,col="gray80")} # horizontal lines
# for(ll in seq(1.7,2.2,0.1)){abline(h=ll,col="gray80")} # horizontal lines
# for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,1.5),col="gray80")} # vertical lines
# for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(1.7,2.3),col="gray80")} # vertical lines
# 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[pr_zero_zero],format="%Y-%m-%d %H:%M")),2.2-(db.bamet.lvl1$prec[pr_zero_zero]/100),
#        pch = 20, cex.lab = 1.5, cex.axis=1.7, col="gray80")
# for(qq in pr_zero){
#   lines(c(as.numeric(strptime(db.bamet.lvl1$UTC[qq],format="%Y-%m-%d %H:%M")),
#           as.numeric(strptime(db.bamet.lvl1$UTC[qq],format="%Y-%m-%d %H:%M"))),
#         c(2.2,2.2-(db.bamet.lvl1$prec[qq]/100)),lwd=2, cex.lab = 1.5, cex.axis=1.7, col="darkblue")}
# 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[pr_flags],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$prec[pr_flags], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="red")  
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sh_zero],format="%Y-%m-%d %H:%M")),1.45-db.bamet.lvl1$snowh[sh_zero], pch = 20, cex.lab = 1.5,
#        col="green3")      
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sh_flags],format="%Y-%m-%d %H:%M")),1.45-db.bamet.lvl1$snowh[sh_flags], pch = 20, cex.lab = 1.5,
#        col="red")   
# axis(2, at=seq(0,1.5,0.25),labels=seq(0,1.5,0.25), las=2,cex.axis=4)
# axis(2, at=seq(1.7,2.2,0.1),labels=rev(seq(0,100,20)), las=2,cex.axis=4)
# #mtext("snowheihgt[m]                        precipitation[mm]",side=2,cex=2)
# axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
# text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(2.3,12),labels=Months, las=2,cex=4)
# text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0.1,jahr, las=2,cex=6)
# if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,1,"reindeer", las=2,srt=60,cex=4,col="gray80")}
# 
# # legend("center",cex=0.8,title="",lty=1,lwd=3,col=c("darkblue","green3","red"),y.intersp=0.8,
# #        box.col = "white",inset=0.05,seg.len = 0.8,c("precipitation","snowheight","flagged"),bg="white")
# dev.off() # ;rm(pr_zero,pr_zero_zero,sh_zero,pr_flags,sh_flags)
}# Snowheigth (off)

if(zack==1){ 
#  precipitation
# -----------------------------------------------------

if(jahr>2009){
pr<-length(aggregate(db.bamet$prec~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,2])
precr<-matrix(ncol=4,nrow=pr,1)
precr[,1]<-aggregate(db.bamet$prec~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,1]
precr[,2]<-aggregate(db.bamet$prec~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = sum)[,2]

pr_zero        <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec > 0) 
pr_zero_zero   <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec == 0) 
pr_flags       <- which(as.numeric(db.bamet.lvl1$prec_fl) > 0) 

png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_prec_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$prec, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(0,2.3), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(0,1.5,0.250)){abline(h=ll,col="gray80")} # horizontal lines
for(ll in seq(1.7,2.2,0.1)){abline(h=ll,col="gray80")} # horizontal lines
text(as.numeric(strptime(lischt[8],format="%Y-%m-%d %H:%M")),2,"hourly", las=2,srt=20,cex=5,col="gray80")
text(as.numeric(strptime(lischt[8],format="%Y-%m-%d %H:%M")),1,"daily sum", las=2,srt=20,cex=5,col="gray80")
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,1.5),col="gray80")} # vertical lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(1.7,2.3),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[pr_zero_zero],format="%Y-%m-%d %H:%M")),2.2-(db.bamet.lvl1$prec[pr_zero_zero]/40),
       pch = 20, cex.lab = 1.5, cex.axis=1.7, col="gray80")
for(qq in pr_zero){# real values in upper part
  lines(c(as.numeric(strptime(db.bamet.lvl1$UTC[qq],format="%Y-%m-%d %H:%M")),
          as.numeric(strptime(db.bamet.lvl1$UTC[qq],format="%Y-%m-%d %H:%M"))),
        c(2.2,2.2-(db.bamet.lvl1$prec[qq]/40)),lwd=2, cex.lab = 1.5, cex.axis=1.7, col="turquoise4")}
for(dd in 1:length(precr[,1])){# real values in upper part
lines(c(as.numeric(strptime(precr[dd,1],format="%Y-%m-%d"))+43200,
        as.numeric(strptime(precr[dd,1],format="%Y-%m-%d"))+43200),
      c(1.5,(1.5-as.numeric(precr[dd,2])/20)),lwd=3, cex.lab = 1.5, cex.axis=1.7, col="steelblue4")}

axis(2, at=seq(0,1.5,0.25) ,labels=rev(seq(0,30,5)), las=2,cex.axis=4)
axis(2, at=seq(1.7,2.2,0.1),labels=rev(seq(0,20,4)), las=2,cex.axis=4)

axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(2.3,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,0.1,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,1,"reindeer", las=2,srt=60,cex=4,col="gray80")}

# legend("center",cex=0.8,title="",lty=1,lwd=3,col=c("darkblue","green3","red"),y.intersp=0.8,
#        box.col = "white",inset=0.05,seg.len = 0.8,c("precipitation","snowheight","flagged"),bg="white")
dev.off()  ;rm(pr_zero,pr_zero_zero,pr_flags)
}
}# Precipitation

if(zack==1){ 
#  windspeed and direction
# -----------------------------------------------------

 w_v_zero    <- which(as.numeric(db.bamet.lvl1$wind_v_200_fl) == 0 ) 
 w_d_zero    <- which(as.numeric(db.bamet.lvl1$wind_deg_200_fl) == 0) 
 w_v_flags   <- which(as.numeric(db.bamet.lvl1$wind_v_200_fl) > 0) 
 w_d_flags   <- which(as.numeric(db.bamet.lvl1$wind_deg_200_fl) > 0)  
 day <- paste0(substr(as.character(db.bamet.lvl1$UTC), 6,7),substr(as.character(db.bamet.lvl1$UTC), 9,10))

 wind.mean<-aggregate(db.bamet.lvl1$wind_v_200 ~ day,FUN = mean,na.action=NULL)
 day.mitte<-db.bamet.lvl1$UTC[substr(as.character(db.bamet.lvl1$UTC), 12,16)=="12:00"]
 weg<-data.frame(day.mitte,wind.mean); weg<-weg[complete.cases(weg[]),]
png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_wind_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0))  
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$wind_v_200, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(0,32), xlab="Date", ylab = "speed[m/s]                        direction[deg]",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(0,24,2)){abline(h=ll,col="gray80")} # horizontal lines
for(ll in seq(26,32,1)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,24.3),col="gray80")} # vertical lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(25.7,32),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[w_d_zero],format="%Y-%m-%d %H:%M")),(db.bamet.lvl1$wind_deg_200[w_d_zero]/60)+26,
       pch = 20, cex.lab = 1.5, cex.axis=1.7, col="darkblue")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[w_d_flags],format="%Y-%m-%d %H:%M")),(db.bamet.lvl1$wind_deg_200[w_d_flags]/60)+26, pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="red")  # flags
points(as.numeric(strptime(db.bamet.lvl1$UTC[w_v_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$wind_v_200[w_v_zero], pch = 20, cex.lab = 1.5,
       col="green3")      
# points(as.numeric(strptime(db.bamet.lvl1$UTC[w_v_flags],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$wind_v_300[w_v_flags], pch = 20, cex.lab = 1.5,
#        col="red")  # flags
points(as.numeric(strptime(weg$day.mitte,format="%Y-%m-%d %H:%M")),weg$db.bamet.lvl1.wind_v_200,col="darkgreen",pch = 20, cex = 1.5)
axis(2, at=seq(0,24,2),labels=seq(0,24,2), las=2,cex.axis=4)
axis(2, at=seq(26,32,1),labels=seq(0,360,60), las=2,cex.axis=4)
#mtext("snowheihgt[m]                        precipitation[mm]",side=2,cex=2)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,22,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,10,"reindeer", las=2,srt=60,cex=4,col="gray80")}
# legend("center",cex=0.8,title="",lty=1,lwd=3,col=c("darkblue","green3","red"),y.intersp=0.8,
#        box.col = "white",inset=0.05,seg.len = 0.8,c("direction","speed","flagged"),bg="white")
dev.off()  ; rm(w_v_zero,w_d_zero,w_v_flags,w_d_flags,day,wind.mean,day.mitte)

#  windspeed and direction (2) windrose
# -----------------------------------------------------

db.wind<-db.bamet[,c(1,3,5,6)]
db.wind<-db.wind[complete.cases(db.wind),]




png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_wrose_",jahr,".png",sep=""),width=p.width*0.8,height=p.width*0.8,pointsize=8)
par(mar=c(1,1,1,1),omi=c(0,0,0,0)) 
wind.rose(wind.freq(db.wind$wind_v_200,db.wind$wind_deg_200), key=F, 6, 4, ang=-3*pi/16, main="")#paste(jahr)
legend("topright",cex=3,title="",lty=1,lwd=3,col=c("transparent"),y.intersp=0.8,
       box.col = "white",inset=0.05,seg.len = 0.8,c(paste(jahr)),bg="transparent")

dev.off()  ; rm(db.wind)

}# Wind

if(zack==1){ 

#  humidity
# -----------------------------------------------------


hr<-length(aggregate(db.bamet$RH_200~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,2])
humr<-matrix(ncol=4,nrow=hr,1)
humr[,1]<-aggregate(db.bamet$RH_200~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,1]
humr[,2]<-aggregate(db.bamet$RH_200~format(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),format="%Y-%m-%d"),FUN = mean)[,2]

hum_zero   <- which(as.numeric(db.bamet.lvl1$RH_200_fl) == 0) 
hum_flags   <- which(as.numeric(db.bamet.lvl1$RH_200_fl) > 0) 

png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_hum_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$RH_200, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(0,105), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(0,100,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(0,100),col="gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[hum_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$RH_200[hum_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="aquamarine3") 
points(as.numeric(strptime(humr[,1],format="%Y-%m-%d"))+43200,humr[,2], pch = 20, cex.lab = 1.5, cex.axis=1.7,cex=2.5, 
       col="aquamarine4")  

# points(as.numeric(strptime(db.bamet.lvl1$UTC[hum_flags],format="%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$RH_200[hum_flags], pch = 20, cex.lab = 1.5,       col="red")         
axis(2, at=seq(0,100,10),labels=seq(0,100,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,50,"reindeer", las=2,srt=60,cex=4,col="gray80")}
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(103,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,7,jahr, las=2,cex=6)
dev.off() ;rm(hum_zero,hum_flags)
}# Humidity

if(zack==1){ 
#  soil temperature
# -----------------------------------------------------

soil.cols<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(150)
png(file = paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_Ts_203_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)#,A4, landscape)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Ts_203_2, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # 
     xlim=xxlim, ylim=c(-15,18), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n",cex.lab=3)
plot_maintenance(jahr)
for(ll in seq(-15,15,5)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-16,18),col="gray80")} # vertical lines
right<-c(2,5,23,53,93,143)
for(qq in 28:23){
  points(as.numeric(strptime(db.bamet.lvl1[(db.bamet.lvl1[,qq*2+1]==0),1],format="%Y-%m-%d %H:%M")),
db.bamet.lvl1[(db.bamet.lvl1[,qq*2+1]==0),qq*2], pch = 20, cex.lab = 1.5, cex.axis=1.7, col=soil.cols[right[qq-22]]) }           
axis(2, at=seq(-15,15,5),labels=seq(-15,15,5), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(18,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,14,jahr, las=2,cex=6)
dev.off()#close pdf

png(file = paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_Ts_253_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)#,A4, landscape)
par(mar=c(1,5,1,1),omi=c(0,0,0,0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Ts_203_2, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # 
     xlim=xxlim, ylim=c(-15,18), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n",cex.lab=3)
plot_maintenance(jahr)
for(ll in seq(-15,15,5)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-16,18),col="gray80")} # vertical lines
left<-c(2,12,32,62,102,150)
for(qq in 22:17){
  points(as.numeric(strptime(db.bamet.lvl1[(db.bamet.lvl1[,qq*2+1]==0),1],format="%Y-%m-%d %H:%M")),
         db.bamet.lvl1[(db.bamet.lvl1[,qq*2+1]==0),qq*2], pch = 20, cex.lab = 1.5, cex.axis=1.7, col=soil.cols[left[qq-16]]) }           
axis(2, at=seq(-15,15,5),labels=seq(-15,15,5), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(18,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,14,jahr, las=2,cex=6)
dev.off()#close pdf
# get colors for wikiplot
# soil.cols[left]
# soil.cols[right]
# colorRampPalette(c("steelblue4"))(1)
# for(i in seq(1,150,by=2)){cat(paste0("|@",soil.cols[i],":-"))}
}# Soil temperature

if(zack==1){ 

#  snow temperature
# -----------------------------------------------------

snowt_zero    <- which(as.numeric(db.bamet.lvl1$Tair_20_fl) == 0) 
snowt_flags   <- which(as.numeric(db.bamet.lvl1$Tair_20_fl) > 0) 

png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_snowt_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_20, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-30,25), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_20[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="olivedrab3") 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_100[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="aquamarine2") 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_40[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="sienna2") 
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_4[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="sienna2")  

# colorRampPalette(c("aquamarine2","lightgoldenrod3","sienna2","olivedrab3"))(4)

axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}

dev.off()# ;rm(air_zero,air_flags,rr,murr)

png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_snowt2_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_40, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
     xlim=xxlim, ylim=c(-30,25), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
plot_maintenance(jahr)
for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_40[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="goldenrod3") 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_100[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="aquamarine2") 
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_40[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="sienna2") 
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_100[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
       col="darkorchid3")  

# colorRampPalette(c("goldenrod3","darkorchid3"))(2)
# colorRampPalette(c("aquamarine2","lightgoldenrod3","sienna2","olivedrab3"))(4)
axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahr, las=2,cex=6)
#if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}

dev.off()# ;rm(air_zero,air_flags,rr,murr)

}# Snow temperature

########################################################
if(zack==1){ 

# # diffplot
# png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_snowt_diff1_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
# par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
# plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_100, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
#      xlim=xxlim, ylim=c(-5,5), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
# plot_maintenance(jahr)
# for(ll in seq(-5,5,1)){abline(h=ll,col="gray80")} # horizontal lines
# for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-5,5),col="gray80")} # vertical lines
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$Tair_200[snowt_zero]-db.bamet.lvl1$Tair_20[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="lightgoldenrod3") 
# 
# # colorRampPalette(c("aquamarine2","lightgoldenrod3","sienna2","olivedrab3"))(4)
# 
# axis(2, at=seq(-5,5,1),labels=seq(-5,5,1), las=2,cex.axis=4)
# axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
# text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(5,12),labels=Months, las=2,cex=4)
# text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,-4,jahr, las=2,cex=6)
# if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
# 
# dev.off()# ;rm(air_zero,air_flags,rr,murr)
# png(paste(path$w[path$n=="plot.p"],jahr,"/BaMet2010_snowt_diff2_",jahr,".png",sep=""),width=p.width,height=p.height,pointsize=8)
# par(mar=c(1,8,1,1),omi=c(0,0,0,0)) 
# plot(as.numeric(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.bamet.lvl1$Tair_100, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
#      xlim=xxlim, ylim=c(-5,5), xlab=" ", ylab = " ",xaxt="n", yaxt="n",type="n", cex.axis=3)
# plot_maintenance(jahr)
# for(ll in seq(-5,5,1)){abline(h=ll,col="gray80")} # horizontal lines
# for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-5,5),col="gray80")} # vertical lines
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero],format="%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$Tair_200[snowt_zero]-db.bamet.lvl1$Tair_100[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        col="aquamarine2") 
# 
# # colorRampPalette(c("aquamarine2","lightgoldenrod3","sienna2","olivedrab3"))(4)
# 
# axis(2, at=seq(-5,5,1),labels=seq(-5,5,1), las=2,cex.axis=4)
# axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
# text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(5,12),labels=Months, las=2,cex=4)
# text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,-4,jahr, las=2,cex=6)
# if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
# 
# dev.off()# ;rm(air_zero,air_flags,rr,murr)


}# diffplot (off)

if(zack==1){ 


#summary(db.bamet.lvl1$prec)

#    # plot sw, albedo and snowheight (data with flag=0)
#    png(paste(plot.path,jahr,"/BaMet2010_RadAlbSnH_",jahr,".png",sep=""),width=1400,height=1100)
#    par(mfrow=c(3,1),mar=c(4.2, 4.5, 4.8, 2.1),omi=c(0.5,0.5,0.6,0.2))  
#    plot(strptime(db.bamet$UTC[sw_in],format="%Y-%m-%d %H:%M"),db.bamet$SwOut[sw_in], pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#         xlim=xxlim, ylim=ylim_sw, xlab="Date", ylab="[W / m2]", col=155, main="SW Incoming and Outgoing",panel.first=grid(),cex.main=2)  # plot sw_in
#    points(strptime(db.bamet$UTC[sw_out],format="%Y-%m-%d %H:%M"),db.bamet$SwIn[sw_out], pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#           xlab="Date", ylab = "[W / m2]", col = "mediumpurple3")                                                            # plot sw_out   
#    plot_maintenance(jahr)
#    plot(strptime(db.bamet$UTC[-albedo_bad],format="%Y-%m-%d %H:%M"),db.bamet$Albedo[-albedo_bad], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        xlim=xxlim, ylim=c(-1.2,1.2), xlab="Date", ylab="[%]",panel.first=grid(),main="Outgoing/Incoming",cex.main=2)             # albedo = out/in
#    plot_maintenance(jahr)
#    plot(strptime(db.bamet$UTC[-sh_out],format="%Y-%m-%d %H:%M"),db.bamet$snowh[-sh_out], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#        xlim=xxlim, ylim=c(-0.2,1.8), xlab="Date", ylab="[m]",panel.first=grid(),main="snowheight",cex.main=2)             # snowheight without bad data or noise
#    plot_maintenance(jahr)
#    title( paste( "Bayelva ", jahr, sep=""), line = 0, outer = TRUE, cex.main=3)
#    dev.off()


#    # all data
#    png(paste(plot.path,jahr,"/BaMet2010_RadAlbSnH_noFilter",jahr,".png",sep=""),width=1400,height=1100)
#    par(mfrow=c(3,1),mar=c(4.2, 4.5, 4.8, 2.1),omi=c(0.5,0.5,0.6,0.2))  
#    plot(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$SwOut, pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#         xlim=xxlim, ylim=ylim_sw, xlab="Date", ylab="[W / m2]", col=155, main="SW Incoming and Outgoing",panel.first=grid(),cex.main=2)  # plot sw_in
#    points(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$SwIn, pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#           xlab="Date", ylab = "[W / m2]", col = "mediumpurple3")                                                            # plot sw_out   
#    plot_maintenance(jahr)
#    plot(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$Albedo, pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#         xlim=xxlim, ylim=c(-1.2,1.2), xlab="Date", ylab="[%]",panel.first=grid(),main="Outgoing/Incoming",cex.main=2)             # albedo = out/in
#    plot_maintenance(jahr)
#    plot(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$snowh, pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#         xlim=xxlim, ylim=c(-0.2,1.8), xlab="Date", ylab="[m]",panel.first=grid(),main="snowheight",cex.main=2)             # snowheight without bad data or noise
#    plot_maintenance(jahr)
#    title( paste( "Bayelva ", jahr, sep=""), line = 0, outer = TRUE, cex.main=3)
#    dev.off()
#    
#    
#    # plotting radiation stuff
#    png(paste(plot.path,jahr,"/BaMet2010_rad_outliers_",jahr,".png",sep=""),width=1400,height=900)
#    par(mfrow=c(2,1),mar=c(6.5, 4.5, 4, 2.1),omi=c(0.5,0.5,0.5,0.2))     # global plotting settings
#    
#    ## plot1 (original sw data + detected bad data in red)
#    plot(strptime(db.bamet$UTC[sw_in],format="%Y-%m-%d %H:%M"),db.bamet$SwOut[sw_in], pch = 20, cex.lab = 1.7, cex.axis=1.7,           # plot sw_in
#           xlim=xxlim, ylim=ylim_sw, xlab="Date", ylab="[W / m2]", col=155, main="SW Incoming and Outgoing",panel.first=grid(),cex.main=2)    
#    lines (strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),rep(-5,each=length(db.bamet$UTC)), col="red")                                    # plot limit line
#    points(strptime(db.bamet$UTC[sw_out],format="%Y-%m-%d %H:%M"),db.bamet$SwIn[sw_out], pch = 20, cex.lab = 1.7, cex.axis=1.7,       # plot sw_out
#           xlab="Date", ylab = "[W / m2]", col = "mediumpurple3") 
#    plot_maintenance(jahr)
#    points(strptime(db.bamet$UTC[-sw_in],format="%Y-%m-%d %H:%M"), db.bamet$SwOut[-sw_in], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red") # plot flag!=0
#    points(strptime(db.bamet$UTC[-sw_out],format="%Y-%m-%d %H:%M"),db.bamet$SwIn[-sw_out], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red")# plot flag!=0
#    plot_maintenance(jahr)
#    par(xpd=TRUE)
#    legend("bottomright", inset=c(0,-0.39),pch=c(19, 19, 19),c("SwOut (In)","SwIn (Out)","bad flag"),col=c(155,"mediumpurple3","red"), bty="n", cex=1.5)
#    par(xpd=FALSE)
#    
#    ## plot 2 (plot lw data + detected bad data in red)
#    plot(strptime(db.bamet$UTC[lw_in],format="%Y-%m-%d %H:%M"),db.bamet$LwOut_co[lw_in], pch = 20, cex.lab=1.7, cex.axis=1.7,   # plot lw_in
#           xlim=xxlim, ylim=ylim_lw, xlab="Date", ylab = "[W / m2]", col = 155, main="LW Incoming and Outgoing",panel.first=grid(),cex.main=2)  
#    lines (strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),rep(100,each=length(db.bamet$UTC)), col="red")               # plot limit line
#    lines (strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),rep(500,each=length(db.bamet$UTC)), col="red")               # plot limit line
#    points(strptime(db.bamet$UTC[lw_out],format="%Y-%m-%d %H:%M"),db.bamet$LwIn_co[lw_out], pch = 20, cex.lab=1.7, cex.axis=1.7,   # plot lw_out
#           xlab="Date", ylab = "W / m2", col = "mediumpurple3") 
#    points(strptime(db.bamet$UTC[-lw_in],format="%Y-%m-%d %H:%M"), db.bamet$LwOut_co[-lw_in], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red")  # plot flag!=0
#    points(strptime(db.bamet$UTC[-lw_out],format="%Y-%m-%d %H:%M"),db.bamet$LwIn_co[-lw_out], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red")  # plot flag!=0
#    plot_maintenance(jahr)  
#    par(xpd=TRUE) # plot outside box
#    legend("bottomright", inset=c(0,-0.39),pch=c(19, 19, 19),c("LwOut_co (In)","LwIn_co (Out)","bad flag"),col=c(155,"mediumpurple3","red"), bty="n", cex=1.5)
#    par(xpd=FALSE)
#    title( paste( "Bayelva radiation for ", jahr, sep=""), line = 0, outer = TRUE, cex.main=3)   
#    dev.off()
#    
#    
#    
#    ## LEVEL 1 plot each month
#    # ------------------------
#    for(m in months){ 
#      
#      # extract only monthly data
#      tmp<-(m==substr(db.bamet[,1],6,7)) 
#      mm_data <- which(tmp==TRUE)
#      
#      # check if no radiation data at all exist for this month
#      if(    all(is.na(db.bamet$SwOut[mm_data]))==TRUE   && all(is.na(db.bamet$SwIn[mm_data]))==TRUE 
#             && all(is.na(db.bamet$LwOut_co[mm_data]))==TRUE && all(is.na(db.bamet$LwIn_co[mm_data]))==TRUE ) {  
#        #cat("\nNo Data for", m, jahr)
#        next
#      }
#         
#      # plotting bounderies
#      ylim_sw <- plot_bounderies(db.bamet$SwOut[intersect(mm_data,sw_in)],db.bamet$SwIn[intersect(mm_data,sw_in)])      # get plotting bounderies shortwave
#      ylim_lw <- plot_bounderies(db.bamet$LwOut_co[intersect(mm_data,sw_out)],db.bamet$LwIn_co[intersect(mm_data,sw_out)])  # get plotting bounderies longwave
#      if((m=="12")==TRUE){ xxlim = c(as.numeric(strptime(paste0("01.",m,".",jahr),format="%d.%m.%Y")), as.numeric(strptime(paste0("31.",m,".",jahr),format="%d.%m.%Y")))
#      } else { xxlim = c(as.numeric(strptime(paste0("01.",m,".",jahr),format="%d.%m.%Y")), as.numeric(strptime(paste0("01.",months[as.numeric(m)+1],".",jahr),format="%d.%m.%Y")))  }
# 
#      # plot sw, albedo and snowheight
#      png(paste(lvl1.path,"plots/",jahr,"/Bayelva_RadAlbSnH_",m,jahr,".png",sep=""),width=1400,height=1100)
#      par(mfrow=c(3,1),mar=c(4.2, 4.5, 4.8, 2.1),omi=c(0.5,0.5,0.6,0.2))  
#      plot(strptime(db.bamet$UTC[intersect(mm_data,sw_in)],format="%Y-%m-%d %H:%M"),db.bamet$SwOut[intersect(mm_data,sw_in)], pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#           xlim=xxlim, ylim=ylim_sw, xlab="Date", ylab="[W / m2]", col=155, main="SW Incoming and Outgoing",panel.first=grid(),cex.main=2)  # plot sw_in
#      points(strptime(db.bamet$UTC[intersect(mm_data,sw_out)],format="%Y-%m-%d %H:%M"),db.bamet$SwIn[intersect(mm_data,sw_out)], pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#             xlab="Date", ylab = "[W / m2]", col = "mediumpurple3")                                                            # plot sw_out   
#      plot_maintenance(jahr)
#      plot(strptime(db.bamet$UTC[-albedo_bad],format="%Y-%m-%d %H:%M"),db.bamet$Albedo[-albedo_bad], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#           xlim=xxlim, ylim=c(-1.2,1.2), xlab="Date", ylab="[%]",panel.first=grid(),main="Outgoing/Incoming",cex.main=2)             # albedo = out/in
#      plot_maintenance(jahr)
#      plot(strptime(db.bamet$UTC[-sh_out],format="%Y-%m-%d %H:%M"),db.bamet$snowh[-sh_out], pch = 20, cex.lab = 1.5, cex.axis=1.7, 
#           xlim=xxlim, ylim=c(-0.2,1.8), xlab="Date", ylab="[m]",panel.first=grid(),main="snowheight",cex.main=2)             # snowheight without bad data or noise
#      plot_maintenance(jahr)
#      title( paste( "Bayelva ", jahr, sep=""), line = 0, outer = TRUE, cex.main=3)
#      dev.off()
#      
#      
#      
#      # plotting radiation + bad flag data
#      # ----
#      #cat("\nPlotting analysis", m, jahr)
#      png(paste(lvl1.path,"plots/",jahr,"/Bayelva_rad_outliers_",m,jahr,".png",sep=""),width=1400,height=900)
#      par(mfrow=c(2,1),mar=c(6.5, 4.5, 4, 2.1),omi=c(0.5,0.5,0.5,0.2))     # global plotting settings
#      
#      # plot 1 (original sw data + detected bad flags in red)
#      plot(strptime(db.bamet$UTC[intersect(mm_data,sw_in)],format="%Y-%m-%d %H:%M"),db.bamet$SwOut[intersect(mm_data,sw_in)], pch = 20, cex.lab=1.7, cex.axis=1.7,    # plot sw_in
#            xlim=xxlim, ylim=ylim_sw, xlab="Date", ylab="[W / m2]", col=155, main="SW Incoming and Outgoing",panel.first=grid(),cex.main=2)       
#      lines (strptime(db.bamet$UTC[mm_data],format="%Y-%m-%d %H:%M"),rep(-5,each=length(db.bamet$UTC[mm_data])), col="red")                                                # plot limit line
#      points(strptime(db.bamet$UTC[intersect(mm_data,sw_out)],format="%Y-%m-%d %H:%M"),db.bamet$SwIn[intersect(mm_data,sw_out)], pch = 20, cex.lab=1.7, cex.axis=1.7,  # plot sw_out
#             xlab="Date", ylab = "W / m2", col = "mediumpurple3") 
#      points(strptime(db.bamet$UTC[-sw_in],format="%Y-%m-%d %H:%M"), db.bamet$SwOut[-sw_in], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red") # plot flag!=0
#      points(strptime(db.bamet$UTC[-sw_out],format="%Y-%m-%d %H:%M"),db.bamet$SwIn[-sw_out], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red")# plot flag!=0
#      plot_maintenance(jahr)
#      par(xpd=TRUE)
#      legend("bottomright", inset=c(0,-0.39),pch=c(19, 19, 19),c("SwOut (In)","SwIn (Out)","bad flag"),col=c(155,"mediumpurple3","red"), bty="n", cex=1.5)
#      par(xpd=FALSE)
#      
# 
#      # plot 2 (plot lw data + detected bad data in red)
#      plot(strptime(db.bamet$UTC[intersect(mm_data,lw_in)],format="%Y-%m-%d %H:%M"),db.bamet$LwOut_co[intersect(mm_data,lw_in)], pch = 20, cex.lab=1.7, cex.axis=1.7,     # plot lw_in
#           xlim=xxlim, ylim=ylim_lw, xlab="Date", ylab = "W / m2", col = 155, main="LW Incoming and Outgoing",panel.first=grid(),cex.main=2)  
#      lines (strptime(db.bamet$UTC[mm_data],format="%Y-%m-%d %H:%M"),rep(100,each=length(db.bamet$UTC[mm_data])), col="red")                                                 # plot limit line
#      lines (strptime(db.bamet$UTC[mm_data],format="%Y-%m-%d %H:%M"),rep(500,each=length(db.bamet$UTC[mm_data])), col="red")                                                 # plot limit line
#      points(strptime(db.bamet$UTC[intersect(mm_data,lw_out)],format="%Y-%m-%d %H:%M"),db.bamet$LwIn_co[intersect(mm_data,lw_out)], pch = 20, cex.lab=1.7, cex.axis=1.7, # plot lw_out
#             xlab="Date", ylab = "W / m2", col = "mediumpurple3") 
#      points(strptime(db.bamet$UTC[-lw_in],format="%Y-%m-%d %H:%M"), db.bamet$LwOut_co[-lw_in], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red")  # plot flag!=0
#      points(strptime(db.bamet$UTC[-lw_out],format="%Y-%m-%d %H:%M"),db.bamet$LwIn_co[-lw_out], pch = 20, cex.lab = 1.7, cex.axis=1.7, col="red")  # plot flag!=0
#      plot_maintenance(jahr)
#      par(xpd=TRUE) # plot outside box
#      legend("bottomright", inset=c(0,-0.39),pch=c(19, 19, 19),c("LwOut_co (In)","LwIn_co (Out)","bad flag"),col=c(155,"mediumpurple3","red"), bty="n", cex=1.5)
#      par(xpd=FALSE)
#      title( paste( "Bayelva radiation for ", m, "-", jahr, sep=""), line = 0, outer = TRUE, cex.main=3)   
#      dev.off()       
# 
#      
#      
#    } # end loop over months
#    
#   
}# monthly plots Kerstin (off)
########################################################
cat("#\n# level1 BaMet2010 ",jahr," plot done!\n#\n")

}
