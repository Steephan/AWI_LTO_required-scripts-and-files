###...........................................................................
##
#     wikiplots    SaSnow2012       --------------------
##   
##   see results here
##   http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:samoylov:met:rad:samet2002
##
##   by: Stephan.Lange@awi.de
##   modified: 2021-05-07
##
##   changes:
##   2021-05-07 SL adapted to refresh app and git
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
options(scipen = 100, stringsAsFactors = F, digits = 2, scientific = T) # for non - exponential display of numeric values
origin <- "1970-01-01"

aktuell <- as.numeric(format(Sys.Date(), "%Y"))

# run.year <- 2021 #2020:aktuell

#run.year <- c(2018)
zack <- 1
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


p.width <- 420*3.5
p.height <- 280*3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)


for (year in as.numeric(run.year)) {
  if (zack == 1) {
    # load data ----
    ###...........................................................................
    
    db.snow <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "SaSnow2012/00_full_dataset/SaSnow2012_", year, "_lv1_noflag.dat", sep = ""),
                          sep = ",", dec = ".", header = T, fill = TRUE)
    db.snow.lvl1 <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "SaSnow2012/00_full_dataset/SaSnow2012_", year, "_lv1.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
    
    
    
    xxlim = c(as.numeric(strptime(paste0("13.01.", year), format = "%d.%m.%Y")),
              as.numeric(strptime(paste0("20.12.", year), format = "%d.%m.%Y")))
    
    # plotting analysis
    ###...........................................................................
    lischt <- c(db.snow.lvl1$UTC[format(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
                db.snow.lvl1$UTC[length(db.snow.lvl1$UTC)])
    
  }# Load data
  
  if (zack == 1) {
    if(year<=2020){
    # air temperature ----
    ###...........................................................................
    #col2hex(soil.cols)#library(gplots)
    rr <- length(aggregate(db.snow$Tair_80~format(strptime(db.snow$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
    murr <- matrix(ncol = 4, nrow = rr, 1)
    murr[, 1] <- aggregate(db.snow$Tair_80~format(strptime(db.snow$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
    murr[, 2] <- aggregate(db.snow$Tair_80~format(strptime(db.snow$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]
    # murr[, 3] <- aggregate(db.snow.lvl1$Tair_200~format(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = min)[, 2]
    # murr[, 4] <- aggregate(db.snow.lvl1$Tair_200~format(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = max)[, 2]
    
    
    air_zero <- which(as.numeric(db.snow.lvl1$Tair_80_fl) == 0)
    air_flags <- which(as.numeric(db.snow.lvl1$Tair_80_fl) > 0)
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2012_airt_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.snow.lvl1$Tair_80, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(-40, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    #plot_maintenance(year)
    # horizontal lines
    abline(h = seq(-40, 30, 5), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    points(as.numeric(strptime(db.snow.lvl1$UTC[air_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$Tair_80[air_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightgoldenrod3")
    points(as.numeric(strptime(murr[, 1], format = "%Y-%m-%d")) + 43200, murr[, 2],
           pch = 20, cex = 2.5, col = "darkorange3")
    # points(as.numeric(strptime(db.snow.lvl1$UTC[air_flags], format = "%Y-%m-%d %H:%M")),
    # db.snow.lvl1$Tair_200[air_flags], pch = 20, cex.lab = 1.5, col = "red")
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year, las = 2, cex = 6)
    #if (year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")}
    
    dev.off()
  }}# Air temperature
  
  if (zack == 1) {
    ## snowdepth ----
    ###...........................................................................
    #
    sh_zero <- which(as.numeric(db.snow.lvl1$Dsn_0_fl) == 0)
    sh_flags <- which(as.numeric(db.snow.lvl1$Dsn_0_fl) > 0)
    
    sh.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(10)
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2012_sh_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 6, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$Dsn_0, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(0, 1.5), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    #plot_maintenance(year)
    # horizontal lines
    abline(h = seq(0, 1.4, 0.100), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    for (kk in 1:9) {# plot the 10 sensors
      # sh_zero <- which(as.numeric(x <- (paste("db.snow.lvl1$Dsn_", kk - 1, "_fl", sep = ""))) == 0)
      sh_zero <- which(as.numeric(db.snow.lvl1[, 95 + 2 * kk]) == 0)
      sh_flags <- which(as.numeric(db.snow.lvl1[, 95 + 2 * kk]) > 0)
      points(as.numeric(strptime(db.snow.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")),
             db.snow.lvl1[sh_zero, 94 + 2 * kk], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = sh.cols[kk + 1])
    }
    #points(as.numeric(strptime(db.snow.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")), db.snow.lvl1[sh_zero, 168 + 2*6], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
    # col = "red")
    #points(as.numeric(strptime(db.snow.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")), db.snow.lvl1[sh_zero, 168 + 2*7], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
    # col = "blue")
    #points(as.numeric(strptime(db.snow.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")), db.snow.lvl1[sh_zero, 168 + 2*8], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
    # col = "black")
    #points(as.numeric(strptime(db.snow.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")), db.snow.lvl1[sh_zero, 168 + 2*9], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
    # col = "pink")
    #points(as.numeric(strptime(db.snow.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")), db.snow.lvl1[sh_zero, 168 + 2*10], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
    # col = "yellow")
    
    
    
    axis(2, at = seq(0, 1.5, 0.25), labels = seq(0, 1.5, 0.25), las = 2, cex.axis = 2)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(1.5, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")) + 2000000, 1.25, year, las = 2, cex = 6)
    dev.off()
  }# Snowdepth
  
  if (zack == 1) {
    # soil temperature ----
    ###...........................................................................
    
    soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(100)
    png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2012_Ts_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$Ts_10, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, #
         xlim = xxlim, ylim = c(-25, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
    #plot_maintenance(year)
    # horizontal lines
    abline(h = seq(-30, 30, 5), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    right <- c(1, 5, 10, 20, 40, 60, 80, 100)
    for (qq in 46:39) {
      points(as.numeric(strptime(db.snow.lvl1[(db.snow.lvl1[, qq * 2 + 1] == 0), 1], format = "%Y-%m-%d %H:%M")),
             db.snow.lvl1[(db.snow.lvl1[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[right[qq - 38]]) }
    axis(2, at = seq(-30, 20, 5), labels = seq(-30, 20, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year, las = 2, cex = 6)
    dev.off()#close pdf
    
    # for(i in seq(1, 100, by = 2)) {cat(paste0("|@", soil.cols[i], ": - "))}
  }# Soil temperature
  
  if (zack == 1) {
    # snow temperature ----
    ###...........................................................................
    
    snowt_zero <- which(as.numeric(db.snow.lvl1$Tair_80_fl) == 0)
    snowt_flags <- which(as.numeric(db.snow.lvl1$Tair_80_fl) > 0)
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2012_snowt_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$Tair_80, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(-40, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    #plot_maintenance(year)
    # horizontal lines
    abline(h = seq(-40, 30, 5), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    points(as.numeric(strptime(db.snow.lvl1$UTC[snowt_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$Tsn_00[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "slateblue")
    points(as.numeric(strptime(db.snow.lvl1$UTC[snowt_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$Tsn_10[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "orchid2")
    points(as.numeric(strptime(db.snow.lvl1$UTC[snowt_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$Tsn_20[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "olivedrab3")
    points(as.numeric(strptime(db.snow.lvl1$UTC[snowt_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$Tsn_45[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "sienna2")
    
    # colorRampPalette(c("slateblue", "orchid2", "olivedrab3", "sienna2"))(4)
    
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year, las = 2, cex = 6)
    #if (year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")}
    
    dev.off()# ;rm(air_zero, air_flags, rr, murr)
    
  }# Snow temperature
  
  if (zack == 1) {
    # snow density SPA ----
    ###...........................................................................
    rho_zero <- which(db.snow.lvl1$rho_2_fl == 0)
    rho_flags <- which(db.snow.lvl1$rho_2_fl > 0)
    
    
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2012_rho_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$rho_2, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(-200, 720), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    # plot_maintenance(year)
    # horizontal lines
    abline(h = seq(-200, 600, 100), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    points(as.numeric(strptime(db.snow.lvl1$UTC[rho_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$rho_2[rho_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "chocolate")
    points(as.numeric(strptime(db.snow.lvl1$UTC[rho_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$rho_3[rho_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "burlywood4")
    points(as.numeric(strptime(db.snow.lvl1$UTC[rho_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$rho_4[rho_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "mistyrose3")
    axis(2, at = seq(-200, 600, 100), labels = seq(-200, 600, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, year, las = 2, cex = 6)
    if (year == 2012 | year == 2014 | year == 2015) {
      text(as.numeric(strptime(lischt[6], format = "%Y-%m-%d %H:%M")) + 2000000, 300, "no data", las = 2, cex = 8, col = "seashell4")
    }
    dev.off()
  }# snow density SPA
  
  if (zack == 1) {
    # water content SPA ----
    ###...........................................................................
    water_zero <- which(db.snow.lvl1$water_2_fl == 0)
    water_flags <- which(db.snow.lvl1$water_2_fl > 0)
    
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2012_wc_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$water_2, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(0, 5), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    # plot_maintenance(year)
    # horizontal lines
    abline(h = seq(0, 5, 1), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    points(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$water_2, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "chocolate")
    points(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$water_3, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "burlywood4")
    points(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$water_4, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "darkseagreen")
    axis(2, at = seq(0, 5, 1), labels = seq(0, 5, 1), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(5, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 5, year, las = 2, cex = 6)
    if (year == 2012 | year == 2014 | year == 2015) {
      text(as.numeric(strptime(lischt[6], format = "%Y-%m-%d %H:%M")) + 2000000, 3, "no data", las = 2, cex = 8, col = "seashell4")
    }
    dev.off()
  }# water content SPA
  
  if (zack == 3) {
    # snow water equivalent SPA (off) ----
    ###...........................................................................
    rho_zero <- which(db.snow.lvl1$rho_2_fl == 0)
    rho_flags <- which(db.snow.lvl1$rho_2_fl > 0)
    
    png(paste(p.1$w[p.1$n == "plot.p"], year, "/SaSnow2012_rho_", year, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.snow.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
         db.snow.lvl1$rho_2, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(-200, 720), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    # plot_maintenance(year)
    # horizontal lines
    abline(h = seq(-200, 600, 100), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    
    points(as.numeric(strptime(db.snow.lvl1$UTC[rho_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$rho_2[rho_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "maroon4")
    points(as.numeric(strptime(db.snow.lvl1$UTC[rho_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$rho_3[rho_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "darkcyan")
    points(as.numeric(strptime(db.snow.lvl1$UTC[rho_zero], format = "%Y-%m-%d %H:%M")),
           db.snow.lvl1$rho_4[rho_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "mediumvioletred")
    axis(2, at = seq(-200, 600, 100), labels = seq(-200, 600, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, year, las = 2, cex = 6)
    if (year == 2012 | year == 2014 | year == 2015) {
      text(as.numeric(strptime(lischt[6], format = "%Y-%m-%d %H:%M")) + 2000000, 600, "no data", las = 5, cex = 6)
    }
    
    dev.off()
  }# snow water equivalent SPA
  
  
  ###...........................................................................
  ###...........................................................................
  cat("#\n# level1 SaSnow2012 ", year, " plot done!\n#\n")
  
}
