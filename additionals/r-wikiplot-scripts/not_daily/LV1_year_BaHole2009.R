#############################################################################
##
##   BaHole2009        Level1 year mean/min/max
##
##   no extrems,
##
##   by: Niko.bornemann@awi.de
##   last modified: 2020/11/27
##
#############################################################################


in.path<-"n:/sparc/data/LTO/level1/BaHole2009/00_full_dataset/"
out.path<-"n:/sparc/data/LTO/database/03_GTNP/BaHole2009/year/"


files.temp<-dir(in.path, pattern = "noflag")
files.temp

for(i in 2009:2019){
#i<-1

data.temp<-read.table(paste(in.path,files.temp[(i-2008)],sep=""),header=T,dec=".",sep=",")[,c(1,2:11)]
data.temp[,1]<-as.POSIXct(data.temp[,1],format='%Y', tz = "UTC")
data.temp[,1]<-as.numeric(as.POSIXct(data.temp[,1],format='%Y', tz = "UTC"))


for (y in 1:3) {
  if (y == 1) {
    name_file = '_mean'
    tem<-aggregate(data.temp, list(data.temp[,1]), mean,  na.rm=TRUE)
    cat(tem[,11]," ")
    }
  if (y == 2) {
    name_file = "_min"
    tem<-aggregate(data.temp, list(data.temp[,1]), max, na.rm=TRUE)
    }
  if (y == 3){
    name_file = "_max"
    tem<-aggregate(data.temp, list(data.temp[,1]), min, na.rm=TRUE)

    }
  tem<-round(tem,4)
  tem[,2]<-format(as.POSIXct(as.numeric(tem[,2]),tz="UTC",origin="1970-01-01"),format='%Y')
  write.table(tem[,c(2:12)],paste(out.path,"BaHole2009_year",name_file,i,".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
  }
}


# Big File with all data

in.path<-"n:/sparc/data/LTO/database/03_GTNP/BaHole2009/year/"
out.path<-"n:/sparc/data/LTO/database/03_GTNP/BaHole2009/year/all/"

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

  files.temp<-dir(in.path, pattern = paste("BaHole2009_year",name_file,sep=""))
  
  # files.temp<-files.temp[1:8]
  
  data.temp<-read.table(paste(in.path,files.temp[1],sep=""),header=T,dec=".",sep=",")
  x<-length(files.temp)
  for(i in 2:x){
      data.temp<-rbind(data.temp,read.table(paste(in.path,files.temp[i],sep=""),header=T,dec=".",sep=","))
  }
  write.table(data.temp,paste(out.path,"BaHole2009_09-19",name_file,".dat",sep=""),quote = F,dec=".",sep=",",row.names=F)
}
