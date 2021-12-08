###...........................................................................
##
##
##  SaSnow2016     Level1 -> plots
##
##
##  by: stephan.lange@awi.de -2201
##  Niko.bornemann@awi.de
##  last modified: 2016-12-14
##
###...........................................................................
##
##   2021-11-04 SL  pathes and add level0 status data
##
##
##
##
##
###...........................................................................
## to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
#   
#   source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# } else {
#   p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
#   
#   source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# }
###...........................................................................


options(scipen = 100, stringsAsFactors = F, digits = 2, scientific = T) # for non-exponential display of numeric values
origin <- "1970-01-01"
aktuell <- as.numeric(format(Sys.Date(), "%Y"))

#run.year <- c(2016:aktuell)#2012:2016

# run.year <- 2017

zack <- 1
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


p.width <- 420*3.5
p.height <- 280*3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)


for (year in run.year){
  #year <- 2016
  if (zack == 1){
    db.snow <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "SaSnow2016/00_full_dataset/SaSnow2016_", year, "_lv1_final.dat", sep = ""),
                          sep = ",", dec = ".", header = T, fill = TRUE)
    db.snow.lvl1 <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "SaSnow2016/00_full_dataset/SaSnow2016_", year, "_lv1.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
    db.snow.lvl0 <- read.table(paste(p.1$w[p.1$n == "LV0.p"], "SaSnow2016/00_full_dataset/SaSnow2016_", year, "_lv0.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
    
    
    xxlim = c(as.numeric(strptime(paste0("13.01.", year), format = "%d.%m.%Y")),
              as.numeric(strptime(paste0("20.12.", year), format = "%d.%m.%Y")))
    
    # plotting analysis
  
      lischt <- c(db.snow.lvl1$UTC[format(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
                db.snow.lvl1$UTC[length(db.snow.lvl1$UTC)])
    
  }# Load data
  
  if (zack == 1){
    # air temperature -----
     #col2hex(soil.cols)#library(gplots)
    rr <- length(aggregate(db.snow$Tair_170~format(strptime(db.snow$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
    murr <- matrix(ncol = 4, nrow = rr, 1)
    murr[, 1] <- aggregate(db.snow$Tair_170~format(strptime(db.snow$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
    murr[, 2] <- aggregate(db.snow$Tair_170~format(strptime(db.snow$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]
    # murr[, 3] <- aggregate(db.snow.lvl1$Tair_200~format(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = min)[, 2]
    # murr[, 4] <- aggregate(db.snow.lvl1$Tair_200~format(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = max)[, 2]
    
    air_zero  <- which(as.numeric(db.snow.lvl1$Tair_170_fl) == 0)
    air_flags  <- which(as.numeric(db.snow.lvl1$Tair_170_fl) > 0)
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2016_airt_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$Tair_170, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  # albedo from file
         xlim = xxlim, ylim = c(-40, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    plot_maintenance(year)
    # horizontal lines
    abline(h = seq(-45, 30, 5), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    points(as.numeric(strptime(db.snow.lvl1$UTC[air_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$Tair_170[air_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightgoldenrod3")
    points(as.numeric(strptime(murr[, 1], format = "%Y-%m-%d")) + 43200,
           murr[, 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "darkorange3")
    # points(as.numeric(strptime(db.snow.lvl1$UTC[air_flags], format = "%Y-%m-%d %H:%M")),
    #    db.snow.lvl1$Tair[air_flags], pch = 20, cex.lab = 1.5,    col = "red")
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(22.5, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year, las = 2, cex = 6)
    #if (year == 2012){text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")}
    
    dev.off()
  }# Air temperature
  
  if (zack == 1){
    # # snowdepth -----
    sh_zero   <- which(as.numeric(db.snow.lvl1$Dsn_fl) == 0)
    sh_flags  <- which(as.numeric(db.snow.lvl1$Dsn_fl) > 0)
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2016_sh_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 6, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$Dsn, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  # albedo from file
         xlim = xxlim, ylim = c(0, 1.0), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    plot_maintenance(year)
    # horizontal lines
    abline(h = seq(0, 1.0, 0.1), col = "gray80")
    # vertical lines
    abline(h = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    # plot the 1 sensors
    points(as.numeric(strptime(db.snow.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$Dsn[sh_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#04A999")
    
    axis(2, at = seq(0, 1.0, 0.1), labels = seq(0, 1.0, 0.1), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.95, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")) + 2000000, 0.85, year, las = 2, cex = 6)
    dev.off()
  }# Snowdepth
  
  if (zack == 1){
    # station status Voltage -----
 
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2016_status_U_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 6, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl0$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl0$batt_U, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  # albedo from file
         xlim = xxlim, ylim = c(10, 20), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    plot_maintenance(year)
    # horizontal lines
    abline(h = seq(10, 20, 1), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    # plot the 1 sensors
    points(as.numeric(strptime(db.snow.lvl0$UTC, format = "%Y-%m-%d %H:%M")),
           db.snow.lvl0$batt_U, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "blue")
    
    axis(2, at = seq(10, 20, 1), labels = seq(10, 20, 1), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(19.5, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")) + 8000000, 17.5, year, las = 2, cex = 6)
    dev.off()
  }# station status Voltage
  
  if (zack == 1){
    # station status Temperature -----

    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2016_status_T_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 6, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl0$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl0$Tpan_CR1000, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  # albedo from file
         xlim = xxlim, ylim = c(-40, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    plot_maintenance(year)
    # horizontal lines
    abline(h = seq(-40, 30, 5), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    # plot the 1 sensors
    points(as.numeric(strptime(db.snow.lvl0$UTC, format = "%Y-%m-%d %H:%M")),
           db.snow.lvl0$Tpan_CR1000, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "#daa89a")
    
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(22.5, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")) + 8000000, 12.5, year, las = 2, cex = 6)
    dev.off()
  }# station status Temperature
  
  cat("#\n# level1 SaSnow2016 ", year, " plot done!\n#\n")
  
}
