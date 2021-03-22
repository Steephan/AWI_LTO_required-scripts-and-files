


library(metvurst)
library(RColorBrewer)
#source("N:/geo5/SoilData/doc/scripts/database_R/db_func.R")
source("/home/steeph/Desktop/sylvester/db_func.R")
#options(scipen=100,stringsAsFactors=F,digits=2,scientific=T) # for non-exponential display of numeric values
#path<-read.table("N:/geo5/SoilData/doc/scripts/database_R/path.txt",sep="\t",header=T)
path<-read.table("/home/steeph/Desktop/sylvester/path.txt",sep="\t",header=T)
maint<-read.table("/home/steeph/Desktop/sylvester/maintance.txt",sep="\t",header=T)
origin="1970-01-01"
years  <- c(2014)#2009:2015
#years  <- c(2013)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


#p.width=420;p.height=280
p.width=420*2;p.height=280*2
color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)


for (jahr in years){
  
  db.bamet     <-read.table(paste(path$w[path$n=="BaMet.lv1.p"],"00_complete/BaMet2010_",jahr,"_noflag.dat",sep=""),sep=",",dec=".",header=T, fill = TRUE) 
  db.bamet.lvl1<-read.table(paste(path$w[path$n=="BaMet.lv1.p"],"00_complete/BaMet2010_",jahr,".dat",sep=""),sep=",",dec=".",header=T, fill = TRUE) 
  xxlim = c(as.numeric(strptime(paste0("13.01.",jahr),format="%d.%m.%Y")), as.numeric(strptime(paste0("20.12.",jahr),format="%d.%m.%Y")))
  
  # plotting analysis
  # -----------------
  #cat("\nPlotting analysis", jahr)
  ylim_sw <- plot_bounderies(db.bamet.lvl1$SR01Up_Avg,db.bamet.lvl1$SR01Dn_Avg)      # get plotting bounderies shortwave
  ylim_lw <- plot_bounderies(db.bamet.lvl1$IR01UpCo_Avg,db.bamet.lvl1$IR01DnCo_Avg)  # get plotting bounderies longwave
  lischt<-c(db.bamet.lvl1$UTC[format(strptime(db.bamet.lvl1$UTC,format="%Y-%m-%d %H:%M"),format="%d %H:%M")=="01 00:00"],db.bamet.lvl1$UTC[length(db.bamet.lvl1$UTC)])
  


#db.wind<-db.bamet.lvl1[,c(1,4,8,10)]
db.wind<-db.bamet[,c(1,3,5,6)]
dts <- as.POSIXct(strptime(db.wind$UTC, format = "%Y-%m-%d %H:%M"))
#db.wind<-db.wind[complete.cases(db.wind),]
#vier<-db.wind[as.numeric(format(as.POSIXct(db.wind$UTC,format='%Y-%m-%d %H:%M',origin=origin, tz = "UTC"),format='%M'))==0,]
vier<-db.wind
# dts <- as.POSIXct(strptime(vier$UTC, format = "%Y-%m-%d %H:%M")) 
hr <- substr(as.character(dts), 12,13)
windContours(hour = hr ,
             wd = vier$wind_deg_200,
             ws = vier$wind_v_300,
             speedlim = 20,
             keytitle = "hourly wind frequencies [%]",
             colour = rev(brewer.pal(11, "Spectral")),
             ncuts = .5,
             centre = "S",
             add.var = vier$Tair_200
             )
hour <- substr(as.character(dts), 12,13)
ws = vier$wind_v_300
wd = vier$wind_deg_200
smooth.contours = 1.2
dircat <- ordered(ceiling(wd/10), levels = 1:36, labels = 1:36)
tab.wd <- xtabs(~dircat + hour)
tab.wd_smooth <-  image.smooth(tab.wd, theta = smooth.contours, xwidth = 0, ywidth = 0)
image(tab.wd_smooth)
# # pro stunde
# hr <- substr(as.character(dts), 12,13)
# ws = vier$wind_v_300
# bwplot(rev(hr) ~ rev(ws), xlim = c(-0.25, 20), 
#                     ylim = 24.5:0.5, scales = list(x = list(draw = T), y = list(draw = F)), 
#                     xlab = NULL, ylab = NULL)
#hr <- substr(as.character(dts), 12,13)
# mon <- as.numeric(substr(as.character(dts), 6,7))
# hr <- substr(as.character(vier$UTC[mon==10]), 12,13)
# ws = vier$wind_v_300[mon==10]
# bwplot(rev(hr) ~ rev(ws), xlim = c(-0.25, 20), 
#        ylim = 24.5:0.5, scales = list(x = list(draw = T), y = list(draw = F)), 
#        xlab = NULL, ylab = NULL)
# 

}
#histogram(db.wind$wind_v_300)

