#############################################################################
##
##   Level1 plots to wiki
##   --------------------
##   TVCSoil2016 Soil Temperature
##
##   by: Stephan.Lange@awi.de -2200
##   updated by: christian.lehr@awi.de
##   last modified: 2020-04-29
##
#############################################################################
##
##  2020-04-29 CL initialised analogue to LV1_plots_BaSoil2009_tmp.R
##
#############################################################################
##  to run this script separately, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type == "windows") {
  path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t",header = T)
  maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t",header = T)
  p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t",header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t",header = T)

  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
} else {
  path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  p.1 <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)

  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
# #############################################################################


options(scipen = 100, stringsAsFactors = F) # for non-exponential display of numeric values
origin <- "1970-01-01"
recent.year <- as.numeric(format(Sys.Date(),"%Y"))
years <- 2016:recent.year

#soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(150)
soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(20)

Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)
p.width <- 420 * 3.5
p.height <- 280 * 3.5
overall <- 1


########
# to run this script separately, you have to set run.year:
#
run.year <- years
first.year <- min(run.year)
last.year <- max(run.year)
#######

for (years in run.year) {

  dudu <- read.table(paste0(path$w[path$n == "LV1.p"], "TVCSoil2016/00_full_dataset/TVCSoil2016_", years, "_lv1.dat"),
                     sep = ",", dec = ".", header = T, na.strings = "NA")
  lischt <- c(dudu$UTC[format(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              dudu$UTC[length(dudu$UTC)])

  dudu[, 1] <- as.numeric(as.POSIXct(dudu[, 1], origin = origin, tz = "UTC",format = '%Y-%m-%d %H:%M'))
  xxlim <- c(as.numeric(strptime(paste0("13.01.", years), format = "%d.%m.%Y")),
            as.numeric(strptime(paste0("20.12.", years), format = "%d.%m.%Y")))

  last.von <- length(na.omit(dudu[, 2]))
  last.bis <- length(dudu[, 1])


  ## soil temperature
  png(file = paste(path$w[path$n == "plot.p"], years, "/TVCSoil2016_Ts_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#,A4, landscape)

  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")), dudu$Ts_2, pch = 20,# cex.lab=1.7, cex.axis=1.5,   #
       xlim = xxlim, ylim = c(-15, 21), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  # plot_maintenance(years)

  # define grid
  # horizontal lines
  y.axis.seq <- seq(-15, 20, 5)
  abline(h = y.axis.seq, col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

  depth <- c(2, 5, 10, 20)
  for (qq in 1:length(depth)) {
    points(dudu[(dudu[, qq * 2 + 1] == 0), 1],
           dudu[(dudu[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
    points(dudu[(dudu[, qq * 2 + 1] > 0), 1],  # the peaks
           dudu[(dudu[, qq * 2 + 1] > 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "red")
    #cat(paste(length(dudu[dudu[,qq*2+1] >0,qq*2+1])," "))
  }

  axis(2, at = y.axis.seq, labels = y.axis.seq, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(21, 12), labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, years, las = 2, cex = 6)
  dev.off()#close pdf
  # get colors for wikiplot
  # soil.cols[depth]

  #cat("#\n# level1 TVCSoil2016_temp ", years," plot done!\n#\n")

  ################################
  # volumetric water content
  # horizontal sensors
  png(file = paste(path$w[path$n == "plot.p"], years, "/TVCSoil2016_vwc_h_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#,A4, landscape)

  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  yylim <- c(0, 0.5)

  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")), dudu$vwc_h_2, pch = 20,
       xlim = xxlim, ylim = yylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  # plot_maintenance(years)

  # define grid
  # horizontal lines
  y.axis.seq <- seq(0, 1, 0.1)
  abline(h = y.axis.seq, col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

  depth <- c(2, 5, 10, 20)
  # adjust the column number to select correct columns
  adj.columns <- 12
  for (qq in 1:length(depth)) {
    # adjust the column number to select correct columns
    qqq <- qq + adj.columns
    points(dudu[(dudu[, qqq * 2 + 1] == 0), 1],
           dudu[(dudu[, qqq * 2 + 1] == 0), qqq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
    points(dudu[(dudu[, qqq * 2 + 1] > 0), 1],  # the peaks
           dudu[(dudu[, qqq * 2 + 1] > 0), qqq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "red")
    #cat(paste(length(dudu[dudu[,qq*2+1] >0,qq*2+1])," "))
  }

  axis(2, at = y.axis.seq, labels = y.axis.seq, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(max(yylim), 12), labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, max(yylim) - (diff(yylim) * 0.2), years, las = 2, cex = 6)
  dev.off()#close pdf

  #######################################
  # vwc
  # vertical sensors
  png(file = paste(path$w[path$n == "plot.p"], years, "/TVCSoil2016_vwc_v_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#,A4, landscape)

  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  yylim <- c(0, 0.6)

  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")), dudu$vwc_v_0, pch = 20,
       xlim = xxlim, ylim = yylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  # plot_maintenance(years)

  # define grid
  # horizontal lines
  y.axis.seq <- seq(0, 0.5, 0.1)
  abline(h = y.axis.seq, col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

  # adjust the column number to select correct columns
  adj.columns <- 16
  for (qq in 1:3) {
    # adjust the column number to select correct columns
    qqq <- qq + adj.columns
    points(dudu[(dudu[, qqq * 2 + 1] == 0), 1],
           dudu[(dudu[, qqq * 2 + 1] == 0), qqq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = c('#a6cee3','#1f78b4','#b2df8a')[qq])
    points(dudu[(dudu[, qqq * 2 + 1] > 0), 1],  # the peaks
           dudu[(dudu[, qqq * 2 + 1] > 0), qqq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "red")
    #cat(paste(length(dudu[dudu[,qq*2+1] >0,qq*2+1])," "))
  }

  axis(2, at = y.axis.seq, labels = y.axis.seq, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(max(yylim), 12), labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, max(yylim) - (diff(yylim) * 0.2), years, las = 2, cex = 6)
  dev.off()#close pdf



  #cat("#\n# level1 TVCSoil2016_vwc ", years," plot done!\n#\n")
  cat("#\n# level1 TVCSoil2016 ", years," plot done!\n#\n")
}


##############################################
# produce overview plot of the complete series every monday
if (weekdays(Sys.Date()) == "Montag" | weekdays(Sys.Date()) == "Monday") { # update every Monday
  start.date <- as.POSIXct(paste0("01.08.2016"), format = '%d.%m.%Y', tz = "UTC")
  end.date <- as.POSIXct(paste0("01.09.2019"), format = '%d.%m.%Y', tz = "UTC")
  #end.date <- as.POSIXct(paste0(format(Sys.Date(), "%d.%m.%Y")), format = '%d.%m.%Y', tz = "UTC")

  db.ba2 <- as.data.frame(matrix(ncol = 12, nrow = length(seq(start.date, end.date, by = "hour")), NA))# soil
  db.ba2[, 1] <- as.POSIXct(seq(start.date, end.date, by = "hour"), format = '%Y-%m-%d', tz = "UTC")

  colnames(db.ba2) <- c("UTC","Ts_2","Ts_5","Ts_10","Ts_20", "vwc_h_2","vwc_h_5","vwc_h_10","vwc_h_20", "vwc_v_0", "vwc_a_v_0", "vwc_b_v_0")

  lascht <- c(db.ba2$UTC[format(strptime(db.ba2$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              db.ba2$UTC[length(db.ba2$UTC)])
  db.ba2[, 1] <- as.numeric(db.ba2[, 1])

  for (kk in first.year:last.year) {
    didi <- read.table(paste0(path$w[path$n == "LV1.p"], "TVCSoil2016/00_full_dataset/TVCSoil2016_", kk, "_lv1_noflag.dat"),
                       sep = ",", dec = ".", header = T, na.strings = "NA")
    didi[, 1] <- as.numeric(as.POSIXct(didi[, 1], format = '%Y-%m-%d %H:%M', tz = "UTC"))
    #colnames(didi)
    db.ba3 <- merge(x = db.ba2[, 1:2], y = didi, by = "UTC", all.x = TRUE)
    db.ba2[, 2] <- round(rowMeans(cbind(db.ba2[, 2], db.ba3[, 3]), na.rm = T), 3)
    db.ba2[, 3] <- round(rowMeans(cbind(db.ba2[, 3], db.ba3[, 4]), na.rm = T), 3)
    db.ba2[, 4] <- round(rowMeans(cbind(db.ba2[, 4], db.ba3[, 5]), na.rm = T), 3)
    db.ba2[, 5] <- round(rowMeans(cbind(db.ba2[, 5], db.ba3[, 6]), na.rm = T), 3)
    db.ba2[, 6] <- rowMeans(cbind(db.ba2[, 6], db.ba3[, 15]), na.rm = T)
    db.ba2[, 7] <- rowMeans(cbind(db.ba2[, 7], db.ba3[, 16]), na.rm = T)
    db.ba2[, 8] <- rowMeans(cbind(db.ba2[, 8], db.ba3[, 17]), na.rm = T)
    db.ba2[, 9] <- rowMeans(cbind(db.ba2[, 9], db.ba3[, 18]), na.rm = T)
    db.ba2[, 10] <- rowMeans(cbind(db.ba2[, 10], db.ba3[, 19]), na.rm = T)
    db.ba2[, 11] <- rowMeans(cbind(db.ba2[, 11], db.ba3[, 20]), na.rm = T)
    db.ba2[, 12] <- rowMeans(cbind(db.ba2[, 12], db.ba3[, 21]), na.rm = T)
    }

  ####################
  # longterm plot soil temperature

  png(file = paste(path$w[path$n == "plot.p"], "Longterm/TVCSoil2016_Ts_Longterm.png", sep = ""),
      width = 3600, height = 500, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  plot(db.ba2$UTC, db.ba2$Ts_2, pch = 20,
       ylim = range(db.ba2[, 2:5], na.rm = TRUE), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  #plot_maintenance(years)

  # define grid
  # horizontal lines
  y.axis.seq <- seq(-15, 20, 5)
  abline(h = y.axis.seq, col = "gray80")
  # vertical lines
  abline(v = as.numeric(seq(as.POSIXct(paste(as.numeric(first.year), "-01-01 01:00:00", sep = "")),
                            as.POSIXct(paste(as.numeric(last.year) + 1, "-01-01 01:00:00", sep = "")),
                            by = "year")), col = "gray80")

  depth <- c(2, 5, 10, 20)
  for (qq in 1:length(depth)) {
    points(db.ba2[, 1], db.ba2[, qq + 1], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
  }

  axis(2, at = y.axis.seq, labels = y.axis.seq, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M"))),
      labels = rep("", length(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M")))), las = 2, tcl = 0.5, cex.axis = 4)
  ##
  # labeling of years
  # text(as.numeric(c(as.POSIXct("2016-12-01 01:00:00"),
  #                   seq(as.POSIXct("2017-02-01 01:00:00"),
  #                   as.POSIXct(paste(as.numeric(last.year), "-12-01 01:00:00", sep = "")), by = "year"))),
  #                   18, c(2016:last.year), las = 2, cex = 4)

  text(as.numeric(seq(as.POSIXct("2016-07-01 01:00:00"),
                      as.POSIXct(paste(as.numeric(last.year), "-12-01 01:00:00", sep = "")), by = "year")),
                      -13, c(2016:last.year), las = 2, cex = 4)

  # close png
  dev.off()


  ####################
  # longterm plot volumetric water content
  # horizontal sensors

  png(file = paste(path$w[path$n == "plot.p"], "Longterm/TVCSoil2016_vwc_h_Longterm.png", sep = ""),
      width = 3600, height = 500, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  yylim <- c(0, 0.5) # range(db.ba2[, 6:9], na.rm = TRUE)
  plot(db.ba2$UTC, db.ba2$vwc_h_2, pch = 20,
       ylim = yylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  #plot_maintenance(years)

  # define grid
  # horizontal lines
  y.axis.seq <- seq(0, 0.5, 0.1)
  abline(h = y.axis.seq, col = "gray80")
  # vertical lines
  abline(v = as.numeric(seq(as.POSIXct(paste(as.numeric(first.year), "-01-01 01:00:00", sep = "")),
                            as.POSIXct(paste(as.numeric(last.year) + 1, "-01-01 01:00:00", sep = "")),
                            by = "year")), col = "gray80")

  depth <- c(2, 5, 10, 20)
  for (qq in 1:length(depth)) {
    points(db.ba2[, 1], db.ba2[, qq + 5], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
  }

  axis(2, at = y.axis.seq, labels = y.axis.seq, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M"))),
       labels = rep("", length(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M")))), las = 2, tcl = 0.5, cex.axis = 4)
  ##
  # labeling of years
  # text(as.numeric(c(as.POSIXct("2016-12-01 01:00:00"),
  #                   seq(as.POSIXct("2017-02-01 01:00:00"),
  #                   as.POSIXct(paste(as.numeric(last.year), "-12-01 01:00:00", sep = "")), by = "year"))),
  #                   0.4, c(2016:last.year), las = 2, cex = 4)

  text(as.numeric(seq(as.POSIXct("2016-07-01 01:00:00"),
                      as.POSIXct(paste(as.numeric(last.year), "-12-01 01:00:00", sep = "")), by = "year")),
                  0.02, c(2016:last.year), las = 2, cex = 4)

  # close png
  dev.off()

  ########################################
  # longterm plot volumetric water content
  # vertical sensors

  png(file = paste(path$w[path$n == "plot.p"], "Longterm/TVCSoil2016_vwc_v_Longterm.png", sep = ""),
      width = 3600, height = 500, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  yylim <- c(0, 0.5) #max(db.ba2[, 10:12], na.rm = TRUE))
  plot(db.ba2$UTC, db.ba2$vwc_v_0, pch = 20,
       ylim = range(db.ba2[, 10:12], na.rm = TRUE), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  #plot_maintenance(years)

  # define grid
  # horizontal lines
  y.axis.seq <- seq(0, 0.5, 0.1)
  abline(h = y.axis.seq, col = "gray80")
  # vertical lines
  abline(v = as.numeric(seq(as.POSIXct(paste(as.numeric(first.year), "-01-01 01:00:00", sep = "")),
                            as.POSIXct(paste(as.numeric(last.year) + 1, "-01-01 01:00:00", sep = "")),
                            by = "year")), col = "gray80")

  for (qq in 1:3) {
    points(db.ba2[, 1], db.ba2[, qq + 9], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = c('#a6cee3','#1f78b4','#b2df8a')[qq])
  }

  axis(2, at = y.axis.seq, labels = y.axis.seq, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M"))),
       labels = rep("", length(as.numeric(strptime(lascht, format = "%Y-%m-%d %H:%M")))), las = 2, tcl = 0.5, cex.axis = 4)


  text(as.numeric(seq(as.POSIXct("2016-07-01 01:00:00"),
                      as.POSIXct(paste(as.numeric(last.year), "-12-01 01:00:00", sep = "")), by = "year")),
       0.02, c(2016:last.year), las = 2, cex = 4)

  # close png
  dev.off()


}
