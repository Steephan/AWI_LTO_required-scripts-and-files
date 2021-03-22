
# SaHole2006 samoylov

in.path<-"N:/geo5/SoilData/data/level1/SaHole2006/00_full_dataset/"
out.path<-"N:/geo5/SoilData/data/level1/SaHole2006/temperature/day/"

#files.temp<-dir(in.path, pattern = "SaHole2006*")
files.temp<-dir(in.path, pattern = "noflag")
files.temp

for(i in 1:12){

data.temp<-read.table(paste(in.path,files.temp[i],sep=""),header=F,skip=1,dec=".",sep="\t")#[,c(1,2:25)]


data.temp[,1]<-as.numeric(as.POSIXct(data.temp[,1],format='%Y-%m-%d', tz = "UTC"))


tem<-aggregate(data.temp, list(data.temp[,1]), mean)
tem<-round(tem,4)
colnames(tem)<-c("v1","date","0.00m","-0.75m","-1.75m","-2.75m","-3.75m","-4.75m","-5.75m","-6.75m","-7.75m","-8.75m","-9.75m","-10.75m","-11.75m","-12.75m","-13.75m","-14.75m","-15.75m","-16.75m","-17.75m","-18.75m","-20.75m","-22.75m","-24.75m","-26.75m")
tem[,2]<-format(as.POSIXct(as.numeric(tem[,2]),tz="UTC",origin="1970-01-01"),format='%Y-%m-%d')

write.table(tem[,c(2:26)],paste(out.path,"SaHole2006_day_",2005+i,".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
}


# Big File with all data for samoylov

in.path<-"N:/geo5/SoilData/data/level1/SaHole2006/temperature/day/"
out.path<-"N:/geo5/SoilData/data/level1/SaHole2006/temperature/day/all/"

files.temp<-dir(in.path, pattern = "SaHole2006")
files.temp
files.temp<-files.temp[1:10]
files.temp

data.temp<-read.table(paste(in.path,files.temp[1],sep=""),header=T,dec=".",sep=",")

for(i in 2:10){

  data.temp<-rbind(data.temp,read.table(paste(in.path,files.temp[i],sep=""),header=T,dec=".",sep=","))
}

write.table(data.temp,paste(out.path,"SaHole2006_06-17",".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)

