
# SaHole2006 samoylov

in.path<-"N:/sparc/data/LTO/level1/SaHole2006/00_full_dataset/"
out.path<-"N:/sparc/data/LTO/database/03_GTNP/SaHole2006/2019_1/year/"

files.temp<-list.files(in.path,pattern="noflag")
#files.temp<-dir(in.path, pattern = "SaHole2006*")
files.temp



for(years in 2018:2019){
#for(i in seq(2,22,by=2){
#i<-2

data.temp<-read.table(paste(in.path,files.temp[years-2005],sep=""),header=F,skip=1,dec=".",sep=",")[,c(1,2:25)]
data.temp[,1]<-as.POSIXct(data.temp[,1],format='%Y', tz = "UTC")
data.temp[,1]<-as.numeric(as.POSIXct(data.temp[,1],format='%Y', tz = "UTC"))


#mean
tem_mean<-aggregate(data.temp, list(data.temp[,1]), mean, na.action=na.pass, na.rm=TRUE)
tem_mean<-round(tem_mean,4)
colnames(tem_mean)<-c("v1","date","0.00m","0.75m","1.75m","2.75m","3.75m","4.75m","5.75m","6.75m","7.75m","8.75m","9.75m","10.75m","11.75m","12.75m","13.75m","14.75m","15.75m","16.75m","17.75m","18.75m","20.75m","22.75m","24.75m","26.75m")
tem_mean[,2]<-format(as.POSIXct(as.numeric(tem_mean[,2]),tz="UTC",origin="1970-01-01"),format='%Y')
cat(tem_mean[,23],"\n")
write.table(tem_mean[,c(2:26)],paste(out.path,"SaHole2006_year_mean",years,".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)

#max
tem_max<-aggregate(data.temp, list(data.temp[,1]), max, na.rm=TRUE)
tem_max<-round(tem_max,4)
colnames(tem_max)<-c("v1","date","0.00m","0.75m","1.75m","2.75m","3.75m","4.75m","5.75m","6.75m","7.75m","8.75m","9.75m","10.75m","11.75m","12.75m","13.75m","14.75m","15.75m","16.75m","17.75m","18.75m","20.75m","22.75m","24.75m","26.75m")
tem_max[,2]<-format(as.POSIXct(as.numeric(tem_max[,2]),tz="UTC",origin="1970-01-01"),format='%Y')

write.table(tem_max[,c(2:26)],paste(out.path,"SaHole2006_year_max",years,".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)

#min
tem_min<-aggregate(data.temp, list(data.temp[,1]), min, na.rm=TRUE)
tem_min<-round(tem_min,4)
colnames(tem_min)<-c("v1","date","0.00m","0.75m","1.75m","2.75m","3.75m","4.75m","5.75m","6.75m","7.75m","8.75m","9.75m","10.75m","11.75m","12.75m","13.75m","14.75m","15.75m","16.75m","17.75m","18.75m","20.75m","22.75m","24.75m","26.75m")
tem_min[,2]<-format(as.POSIXct(as.numeric(tem_min[,2]),tz="UTC",origin="1970-01-01"),format='%Y')

write.table(tem_min[,c(2:26)],paste(out.path,"SaHole2006_year_min",years,".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)


}


# Big File with all data for samoylov

in.path<-"N:/sparc/data/LTO/database/03_GTNP/SaHole2006/2019_1/year/"
out.path<-"N:/sparc/data/LTO/database/03_GTNP/SaHole2006/2019_1/year/all/"

for (y in 1:3) {
  if (y == 1) {
    name_file = '_mean'
  }
  if (y == 2) {
    name_file = "_min"
  }
  if (y == 3){
    name_file = "_max"
  }

  files.temp<-dir(in.path, pattern = paste("SaHole2006_year",name_file,sep=""))
 # files.temp<-files.temp[1:10]
 # read first year 2006
  data.temp<-read.table(paste(in.path,files.temp[1],sep=""),header=T,dec=".",sep=",")

  for(years in 2018:2019){
  #  i<-2
    data.temp<-rbind(data.temp,read.table(paste(in.path,files.temp[years-2017],sep=""),header=T,dec=".",sep=","))
  }
  write.table(data.temp,paste(out.path,"SaHole2006_18-19",name_file,".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
}
