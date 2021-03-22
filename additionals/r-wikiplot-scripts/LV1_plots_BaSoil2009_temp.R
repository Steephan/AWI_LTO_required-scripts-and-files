#############################################################################
##
##   Level1 plots to wiki
##   --------------------
##   BaSoil Temperature
##
##   by: Stephan.Lange@awi.de -2200
##   updated by: christian.lehr@awi.de
##   last modified: 2019-12-04
##
#############################################################################
##
##  last modification:
##  - delete the "+ 1" in the labeling of years
##  - new plotformat
##  - update of paths
##  - no spearate year files any more for variables
##
#############################################################################
##  to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t",header = T)
#   maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t",header = T)
#   p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t",header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t",header = T)
#
#   source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
# } else {
#   path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#   p.1 <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#
#   source("/sparc/LTO/R_database/database_R/settings/db_func.R")
# }
# #############################################################################


options(scipen = 100, stringsAsFactors = F) # for non-exponential display of numeric values
origin <- "1970-01-01"
recent.year <- as.numeric(format(Sys.Date(),"%Y"))

soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(150)

Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)
p.width <- 420 * 3.5
p.height <- 280 * 3.5
overall <- 1


########
# to run this script separately, you have to set run.year:
#
# run.year <- 2009:2020
#######

for (years in run.year) {
  #  dudu<-read.table(paste0(path$w[path$n=="LV1.p"],"BaSoil2009/01_temperature/BaSoil2009_Ts_",years,"_lv1.dat"),sep=",",dec=".",header=T,na.strings="NA")
  dudu <- read.table(paste0(path$w[path$n == "LV1.p"], "BaSoil2009/00_full_dataset/BaSoil2009_", years, "_lv1.dat"),
                     sep = ",", dec = ".", header = T, na.strings = "NA")[, 1:17]
  lischt <- c(dudu$UTC[format(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              dudu$UTC[length(dudu$UTC)])

  dudu[, 1] <- as.numeric(as.POSIXct(dudu[, 1], origin = origin, tz = "UTC",format = '%Y-%m-%d %H:%M'))
  xxlim <- c(as.numeric(strptime(paste0("13.01.", years), format = "%d.%m.%Y")),
            as.numeric(strptime(paste0("20.12.", years), format = "%d.%m.%Y")))
  #  db.last.year <-read.table(paste0(path$w[path$n=="LV1.p"],"BaSoil2009/01_temperature/BaSoil2009_Ts_",years-1,"_lv1.dat"),sep=",",dec=".",header=T,na.strings="NA")
  #  db.last.year[,1]<-as.numeric(as.POSIXct(db.last.year[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M'))
  #  db.last.year <-db.last.year[complete.cases(db.last.year),]
  last.von <- length(na.omit(dudu[, 2]))
  last.bis <- length(dudu[, 1])


  ##
  png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil2009_Ts_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#,A4, landscape)

  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")), dudu$Ts_1, pch = 20,# cex.lab=1.7, cex.axis=1.5,   #
       xlim = xxlim, ylim = c(-15, 18), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  plot_maintenance(years)

  # define grid
  # horizontal lines
  abline(h = seq(-15, 15, 5), col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

  depth <- c(1, 11, 21, 37, 55, 71, 89, 141)
  for (qq in 1:8) {
    points(dudu[(dudu[, qq * 2 + 1] == 0), 1],
           dudu[(dudu[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
    points(dudu[(dudu[, qq * 2 + 1] > 0), 1],  # the peaks
           dudu[(dudu[, qq * 2 + 1] > 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "red")
    #cat(paste(length(dudu[dudu[,qq*2+1] >0,qq*2+1])," "))
  }

  axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12), labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, years, las = 2, cex = 6)
  dev.off()#close pdf
  # get colors for wikiplot
  # soil.cols[depth]

  cat("#\n# level1 BaSoil2009_temp ", years," plot done!\n#\n")

}

# produce overview plot of the complete series every monday
if (weekdays(Sys.Date()) == "Montag" | weekdays(Sys.Date()) == "Monday") { # update every Monday
  start.date <- as.POSIXct(paste0("01.08.2009"), format = '%d.%m.%Y', tz = "UTC")
  end.date <- as.POSIXct(paste0(format(Sys.Date(), "%d.%m.%Y")), format = '%d.%m.%Y', tz = "UTC")

  db.ba2 <- as.data.frame(matrix(ncol = 9, nrow = length(seq(start.date, end.date, by = "hour")), NA))# soil
  db.ba2[, 1] <- as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d', tz = "UTC")

  colnames(db.ba2) <- c("UTC","Ts_1","Ts_11","Ts_21","Ts_37","Ts_55","Ts_71","Ts_89","Ts_141")

  #loscht <- c(db.ba2$UTC[format(strptime(db.ba2$UTC, format = "%Y-%m-%d %H:%M"), format = "%m-%d %H:%M") == "01-01 00:00"], db.ba2$UTC[length(db.ba2$UTC)])

  lascht <- c(db.ba2$UTC[format(strptime(db.ba2$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              db.ba2$UTC[length(db.ba2$UTC)])
  db.ba2[, 1] <- as.numeric(db.ba2[, 1])

  for (kk in 2009:recent.year) {
    didi <- read.table(paste0(path$w[path$n == "LV1.p"], "BaSoil2009/00_full_dataset/BaSoil2009_", kk, "_lv1_noflag.dat"),
                       sep = ",", dec = ".", header = T, na.strings = "NA")
    didi[, 1] <- as.numeric(as.POSIXct(didi[, 1], format = '%Y-%m-%d %H:%M', tz = "UTC"))
    #colnames(didi)
    db.ba3 <- merge(x = db.ba2[, 1:2], y = didi, by = "UTC", all.x = TRUE)
    db.ba2[, 2] <- round(rowMeans(cbind(db.ba2[, 2], db.ba3[, 3]), na.rm = T), 3)
    db.ba2[, 3] <- round(rowMeans(cbind(db.ba2[, 3], db.ba3[, 4]), na.rm = T), 3)
    db.ba2[, 4] <- round(rowMeans(cbind(db.ba2[, 4], db.ba3[, 5]), na.rm = T), 3)
    db.ba2[, 5] <- round(rowMeans(cbind(db.ba2[, 5], db.ba3[, 6]), na.rm = T), 3)
    db.ba2[, 6] <- round(rowMeans(cbind(db.ba2[, 6], db.ba3[, 7]), na.rm = T), 3)
    db.ba2[, 7] <- round(rowMeans(cbind(db.ba2[, 7], db.ba3[, 8]), na.rm = T), 3)
    db.ba2[, 8] <- round(rowMeans(cbind(db.ba2[, 8], db.ba3[, 9]), na.rm = T), 3)
    db.ba2[, 9] <- round(rowMeans(cbind(db.ba2[, 9], db.ba3[, 10]), na.rm = T), 3)
  }

  ####################
  # plot

  # xxxlim <- c(as.numeric(start.date) + 5000000, as.numeric(end.date))# - 7000000)
  #1252000000

  #png(file = paste(path$w[path$n == "plot.p"], run.year, "/BaSoil2009_ts_Ts_", run.year, ".png", sep = ""), width = 3600, height = 500, pointsize = 8)#,A4, landscape)
  png(file = paste(path$w[path$n == "plot.p"], "Longterm/BaSoil2009_Ts_Longterm.png", sep = ""),
      width = 3600, height = 500, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  plot(#as.numeric(strptime(db.ba2$UTC,format = "%Y-%m-%d %H:%M")),
       db.ba2$UTC,
       db.ba2$Ts_1, pch = 20,# cex.lab=1.7, cex.axis=1.5,   #
       #xlim = xxxlim,
       ylim = range(db.ba2[, -1], na.rm = TRUE), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  #plot_maintenance(years)

  # define grid
  # horizontal lines
  abline(h = seq(-15, 15, 5), col = "gray80")
  # vertical lines
  abline(v = as.numeric(seq(as.POSIXct("2009-01-01 01:00:00"),
                            as.POSIXct(paste(as.numeric(recent.year) + 1, "-01-01 01:00:00", sep = "")),
                            by = "year")), col = "gray80")

  depth <- c(1, 11, 21, 37, 55, 71, 89, 141)
  for (qq in 1:8) {
    points(db.ba2[, 1], db.ba2[, qq + 1], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
  }

  axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M"))),
      labels = rep("", length(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M")))), las = 2, tcl = 0.5, cex.axis = 4)
  ##
  # labeling of years
  #text(as.numeric(seq(as.POSIXct("2009-07-01 01:00:00"), as.POSIXct(paste(as.numeric(run.year) + 1, "-07-01 01:00:00", sep = "")), by = "year")), -14, c(2009:run.year), las = 2, cex = 4)
  text(as.numeric(seq(as.POSIXct("2009-07-01 01:00:00"),
                      as.POSIXct(paste(as.numeric(recent.year), "-07-01 01:00:00", sep = "")),
                      by = "year")), -14, c(2009:recent.year), las = 2, cex = 4)

  dev.off()#close png

}
