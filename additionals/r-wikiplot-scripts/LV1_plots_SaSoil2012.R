###...........................................................................
##
#     wikiplots    SaSoil2012       --------------------
##   
##   see results here
##   http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:samoylov:soil:tsoil:sasoil2012
##
##   by: Stephan.Lange@awi.de
##   modified: 2021-05-07
##
##   changes:
##   2021-05-07 SL adapted to refresh app and git
##   
###...........................................................................
## to run this script separately, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type == "windows") {
  p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)

  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
} else {
  p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)

  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
}
###...........................................................................

options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
origin="1970-01-01"

run.year <- 2012:2021

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols.1<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(71)
soil.cols.2<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(275)
#mon.cols<-colorRampPalette(c("dodgerblue4","gold3","firebrick3"))(12)
#mon.cols<-colorRampPalette(c("gray60","olivedrab1","yellow1","tomato1","darkorange1","saddlebrown","dodgerblue3","snow3","gray60"))(13)[c(13,2:11)]
mon.cols<-c("slategray2","skyblue1","lightgreen","olivedrab2","olivedrab4","orangered","red","red4","darkgoldenrod1","tan4","gray40","steelblue4")

p.width=420*3.5;p.height=280*3.5
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)

zack=2
for (jahro in as.numeric(run.year)){
  if(zack==2){
    ##
    ###..........................................................................
    ##
    ##  00.1 load data ----
    ##
    ###..........................................................................
    
    
    db.Ts.lvl1 <-read.table(paste0(p.1$w[p.1$n=="LV1.p"],"SaSoil2012/00_full_dataset/SaSoil2012_",jahro,"_lv1.dat"),sep=",",dec=".",header=T, fill = TRUE)
    
    #  db.TDR.lvl1<-read.table(paste0(p.1$w[p.1$n=="LV1.p"],"SaSoil2002/00_full_dataset/SaSoil2002_TDR_",jahro,"_lv1.dat"),sep=",",dec=".",header=T,na.strings="NA")
    
    ## yearly LEVEL 1 plots
    ###..........................................................................
    
    xxlim = c(as.numeric(strptime(paste0("13.01.",jahro),format="%d.%m.%Y")), as.numeric(strptime(paste0("20.12.",jahro),format="%d.%m.%Y")))
    lischt<-c(db.Ts.lvl1$UTC[format(strptime(db.Ts.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%d %H:%M")=="01 00:00"],db.Ts.lvl1$UTC[length(db.Ts.lvl1$UTC)])
    tair_gut  <- which(db.Ts.lvl1$Ts_center_1_fl==0)
  }# Load data
  
  if(zack==2){
    ##
    ##
    ###..........................................................................
    ##
    ##  01.2 Tair ----
    ##
    ###..........................................................................
    Tair1_zero <- which(as.numeric(db.Ts.lvl1$Tair_70_fl) == 0)
    Tair1_flags <- which(as.numeric(db.Ts.lvl1$Tair_70_fl) > 0)
    rr <- length(aggregate(db.Ts.lvl1$Tair_70[Tair1_zero] ~ format(strptime(db.Ts.lvl1$UTC[Tair1_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
    murr <- matrix(ncol = 3, nrow = rr, 1)
    murr[, 1] <- aggregate(db.Ts.lvl1$Tair_70[Tair1_zero] ~ format(strptime(db.Ts.lvl1$UTC[Tair1_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
    murr[, 2] <- aggregate(db.Ts.lvl1$Tair_70[Tair1_zero] ~ format(strptime(db.Ts.lvl1$UTC[Tair1_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2012_airt_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC,format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Tair_70, pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-40, 25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for (ll in seq(-40, 30, 5)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-40, 30), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.Ts.lvl1$UTC[Tair1_zero], format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$Tair_70[Tair1_zero],
           pch = 20, cex.lab = 1.5, cex.axis = 1.7,
           col = "lightgoldenrod3"
    )    
    points(as.numeric(strptime(db.Ts.lvl1$UTC[Tair1_flags], format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$Tair_70[Tair1_flags],
           pch = 20, cex.lab = 1.5, cex.axis = 1.7,
           col = "navy"
    )
    points(as.numeric(strptime(murr[, 1], format = "%Y-%m-%d")) + 43200,
           murr[, 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "darkorange3")
    
    
    axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 01.2 Ts air
  
  if(zack==2){
    ##
    ##
    ###..........................................................................
    ##
    ##  01.1 Ts center ----
    ##
    ###..........................................................................
    
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2012_temp_center_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,8,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_cen_5[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(5,15,28,45,52,72,90,100))
    for(qq in 2:9){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.2[diefe[qq-1]],pch=20)
    }
    axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 01.1 Ts center
  
  if(zack==2){
    ##
    ##
    ###..........................................................................
    ##
    ##  01.3 Ts rim ----
    ##
    ###..........................................................................
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2012_temp_rim_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
    par(mar=c(1,5,1,1),omi=c(0,0,0,0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC[tair_gut],format="%Y-%m-%d %H:%M")),db.Ts.lvl1$Ts_0[tair_gut], pch = 20,# cex.lab=1.7, cex.axis=1.5,   # albedo from file
         xlim=xxlim, ylim=c(-30,25), xlab="", ylab = "",xaxt="n", yaxt="n",type="n", cex.axis=3)
    #plot_maintenance(jahr)
    for(ll in seq(-30,30,10)){abline(h=ll,col="gray80")} # horizontal lines
    for(pp in as.numeric(strptime(lischt,format="%Y-%m-%d %H:%M"))){lines(c(pp,pp),c(-30,30),col="gray80")} # vertical lines
    diefe<-(c(1,7,22,38,52,69,95,104))
    for(qq in 10:17){
      points(as.numeric(strptime(db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),1],format="%Y-%m-%d %H:%M")),
             db.Ts.lvl1[((db.Ts.lvl1[,qq*2+1])==0),qq*2],col=soil.cols.1[diefe[qq-9]],pch=20)
    }
    axis(2, at=seq(-30,30,10),labels=seq(-30,30,10), las=2,cex.axis=4)
    axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1],format="%Y-%m-%d %H:%M"))-1300000,rep(25,12),labels=Months, las=2,cex=4)
    text(as.numeric(strptime(lischt[11],format="%Y-%m-%d %H:%M"))+2000000,18,jahro, las=2,cex=6)
    #if(jahr==2012){text(as.numeric(strptime(lischt[3],format="%Y-%m-%d %H:%M"))+1000000,0,"reindeer", las=2,srt=60,cex=4,col="gray80")}
    dev.off()
  }# 01.3 Ts rim

  if(zack==2){
    ###..........................................................................
    ##
    ##  02 snowdepth ----
    ##
    ###..........................................................................
    
    sh_zero    <- which(as.numeric(db.Ts.lvl1$Dsn_fl) == 0)
    sh_flags   <- which(as.numeric(db.Ts.lvl1$Dsn_fl) > 0)
    
    png(paste(p.1$w[p.1$n=="plot.p"],jahro,"/SaSoil2012_Dsn_",jahro,".png",sep=""),width=p.width,height=p.height,pointsize=8)
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
  
  if(zack==2){
    ###..........................................................................
    ##
    ##  03 water level ----
    ##
    ###..........................................................................
    WL_zero <- which(db.Ts.lvl1$WL_cen_1_fl == 0)
    WL_eight <- which(db.Ts.lvl1$WL_cen_1_fl == 8)
    
    png(paste0(p.1$w[p.1$n == "plot.p"], jahro, "/SaSoil2012_wl_c1_", jahro, ".png"), width = p.width, height = p.height, pointsize = 8) # ,A4, landscape)
    par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$WL_cen_1,
         pch = 20, # cex.lab=1.7, cex.axis=1.5,   #
         xlim = xxlim, ylim = c(-0.1, 0.31), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n"
    )
    plot_maintenance(jahro)
    for (ll in seq(-0.2, 0.3, 0.05)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    abline(h = 0, col = "gray90", lwd = 5) # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-20, 20), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.Ts.lvl1$UTC[WL_zero], format = "%Y-%m-%d %H:%M")), 
           db.Ts.lvl1$WL_cen_1[WL_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#1b7ac1")
    points(as.numeric(strptime(db.Ts.lvl1$UTC[WL_eight], format = "%Y-%m-%d %H:%M")), 
           db.Ts.lvl1$WL_cen_1[WL_eight], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "peru")
    
    axis(2, at = seq(-0.2, 0.3, 0.05), labels = seq(-0.2, 0.3, 0.05), las = 2, cex.axis = 4)
    # axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.3, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -0.05, jahro, las = 2, cex = 6)
    dev.off() # close pdf
  
    WL_zero <- which(db.Ts.lvl1$WL_cen_2_fl == 0)
    WL_eight <- which(db.Ts.lvl1$WL_cen_2_fl == 8)
    
    png(paste0(p.1$w[p.1$n == "plot.p"], jahro, "/SaSoil2012_wl_c2_", jahro, ".png"), width = p.width, height = p.height, pointsize = 8) # ,A4, landscape)
    par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$WL_cen_2,
         pch = 20, # cex.lab=1.7, cex.axis=1.5,   #
         xlim = xxlim, ylim = c(-0.1, 0.31), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n"
    )
    plot_maintenance(jahro)
    for (ll in seq(-0.2, 0.3, 0.05)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    abline(h = 0, col = "gray90", lwd = 5) # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-20, 20), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.Ts.lvl1$UTC[WL_zero], format = "%Y-%m-%d %H:%M")), 
           db.Ts.lvl1$WL_cen_2[WL_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#c30e78")
    points(as.numeric(strptime(db.Ts.lvl1$UTC[WL_eight], format = "%Y-%m-%d %H:%M")), 
           db.Ts.lvl1$WL_cen_2[WL_eight], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "peru")
    
    axis(2, at = seq(-0.2, 0.3, 0.05), labels = seq(-0.2, 0.3, 0.05), las = 2, cex.axis = 4)
    # axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.3, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -0.05, jahro, las = 2, cex = 6)
    dev.off() # close pdf
    
    WL_zero <- which(db.Ts.lvl1$WL_rim_1_fl == 0)
    WL_eight <- which(db.Ts.lvl1$WL_rim_1_fl == 8)
    
    png(paste0(p.1$w[p.1$n == "plot.p"], jahro, "/SaSoil2012_wl_r1_", jahro, ".png"), width = p.width, height = p.height, pointsize = 8) # ,A4, landscape)
    par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$WL_rim_1,
         pch = 20, # cex.lab=1.7, cex.axis=1.5,   #
         xlim = xxlim, ylim = c(-0.41, 0.1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n"
    )
    plot_maintenance(jahro)
    for (ll in seq(-0.2, 0.3, 0.05)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    abline(h = 0, col = "gray90", lwd = 5) # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-20, 20), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.Ts.lvl1$UTC[WL_zero], format = "%Y-%m-%d %H:%M")), 
           db.Ts.lvl1$WL_rim_1[WL_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#7dd47f")
    points(as.numeric(strptime(db.Ts.lvl1$UTC[WL_eight], format = "%Y-%m-%d %H:%M")), 
           db.Ts.lvl1$WL_rim_1[WL_eight], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "peru")
    
    axis(2, at = seq(-0.2, 0.3, 0.05), labels = seq(-0.2, 0.3, 0.05), las = 2, cex.axis = 4)
    # axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.3, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -0.05, jahro, las = 2, cex = 6)
    dev.off() # close pdf
    
    WL_zero <- which(db.Ts.lvl1$WL_rim_1_fl == 0)
    WL_eight <- which(db.Ts.lvl1$WL_rim_1_fl == 8)
    
    png(paste0(p.1$w[p.1$n == "plot.p"], jahro, "/SaSoil2012_wl_", jahro, ".png"), width = p.width, height = p.height, pointsize = 8) # ,A4, landscape)
    par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.Ts.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$WL_rim_1,
         pch = 20, # cex.lab=1.7, cex.axis=1.5,   #
         xlim = xxlim, ylim = c(-0.1, 0.31), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n"
    )
    plot_maintenance(jahro)
    for (ll in seq(-0.2, 0.3, 0.05)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    abline(h = 0, col = "gray90", lwd = 5) # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-20, 20), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.Ts.lvl1$UTC[which(db.Ts.lvl1$WL_rim_1_fl == 0)], format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$WL_rim_1[which(db.Ts.lvl1$WL_rim_1_fl == 0)], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#7dd47f")
    points(as.numeric(strptime(db.Ts.lvl1$UTC[which(db.Ts.lvl1$WL_cen_1_fl == 0)], format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$WL_cen_1[which(db.Ts.lvl1$WL_cen_1_fl == 0)], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#1b7ac1")
    points(as.numeric(strptime(db.Ts.lvl1$UTC[which(db.Ts.lvl1$WL_cen_2_fl == 0)], format = "%Y-%m-%d %H:%M")), db.Ts.lvl1$WL_cen_2[which(db.Ts.lvl1$WL_cen_2_fl == 0)], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#c30e78")
    
        axis(2, at = seq(-0.2, 0.3, 0.05), labels = seq(-0.2, 0.3, 0.05), las = 2, cex.axis = 4)
    # axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.3, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -0.05, jahro, las = 2, cex = 6)
    dev.off() # close pdf
    
    }# 03 water level
  


  
  
  cat("#\n# level1 SaSoil2012 ",jahro," plot done!\n#\n")
}
