#############################################################################
##
##   Level1 plots to wiki
##   --------------------
##   BaSoil Temperature
##
##   by: Stephan.Lange@awi.de -2201
##   last modified: 2016-06-05
##
##   last check: 2020-03-30
##   checked by: christian.lehr@awi.de
#############################################################################
##
## last modification:
## heatflux
## new plotformat
##
##
#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type  ==  "windows") {
  path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
  maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)

  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
} else {
  path <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
  p.1 <- read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)

  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
#############################################################################

library("RColorBrewer")
options(scipen = 100, stringsAsFactors = F) # for non-exponential display of numeric values

origin <- "1970-01-01"

#soil.cols<-c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4")
soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(150)
Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

p.width <- 420 * 3.5
p.height <- 280 * 3.5
#aktuell<-as.numeric(format(Sys.Date(), "%Y"))
#soil.cols[c(1, 25, 50, 75, 100, 125, 150)]
#for (i in seq(2, 65, 2)) {cat("BaSoil1998, ", colnames(dudu)[i], "\n")}

for (years in 1998:2012) {
  dudu <- read.table(paste0(path$w[path$n == "LV1.p"], "BaSoil1998/00_full_dataset/BaSoil1998_", years, "_lv1.dat"),
                   sep = ",", dec = ".", header = T, na.strings = "NA")
  dada <- read.table(paste0(path$w[path$n == "LV1.p"], "BaSoil1998/06_heat_flux/BaSoil1998_G_", years, "_lv1.dat"),
                     sep = ",", dec = ".", header = T, na.strings = "NA")

  lischt <- c(dudu$UTC[format(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              dudu$UTC[length(dudu$UTC)])

  dudu[, 1] <- as.numeric(as.POSIXct(dudu[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'))
  dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'))
  xxlim = c(as.numeric(strptime(paste0("13.01.", years), format = "%d.%m.%Y")),
            as.numeric(strptime(paste0("20.12.", years), format = "%d.%m.%Y")))
  window <- c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)[years - 1997]
  #         1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
  vorne <- c(3750, 4000, 4000, 3750, 4000, 4000, 4000, 3750, 3700,
             3750, 4000, 4000, 4000, 4000, 3750)[years - 1997]
  hinten <- c(8700, 6300, 7000, 6500, 6300, 6700, 7000, 6300, 6300,
              6300, 6600, 7000, 7000, 6600, 6300)[years - 1997]

#############################################################################
#############################################################################
### soil temperature plots
# Left
# ..........................
  png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_temp_left_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
  par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")),
       dudu$Ts_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
       xlim = xxlim, ylim = c(-15, 18), xlab = "Date", ylab = "temperature [°C]",
       xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
  plot_maintenance(years)

  # horizontal lines
  for (ll in seq(-15, 15, 5)) {
    abline(h = ll, col = "gray80")
  }
  # vertical lines
  for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
    lines(c(pp, pp), c(-16, 18), col = "gray80")
  }
  left <- c(4, 16, 17, 33, 38, 66, 91, 99, 108, 117)
  # 1:10
  for (qq in 1:10) {#points(dudu[((dudu[, qq*2+1])> 0), 1], dudu[((dudu[, qq*2+1])> 0), qq*2], col = "red", pch = 20)
    points(dudu[((dudu[, qq * 2 + 1]) == 0), 1],
           dudu[((dudu[, qq * 2 + 1]) == 0), qq * 2],
           col = soil.cols[left[qq]], pch = 20)
  }
  lines(c(dudu[vorne, 1], dudu[vorne, 1]), c(-16, 18), col = "aquamarine")
  lines(c(dudu[hinten, 1], dudu[hinten, 1]), c(-16, 18), col = "aquamarine")

  # legend("topleft", cex = 0.8, title = "", lty = 1, lwd = 3, col = soil.cols, y.intersp = 0.8,
  #        box.col = "white", inset = 0.05, seg.len = 0.8, c("    1 cm", "  11 cm", "  21 cm", "  37 cm", "  55 cm", "  71 cm", "  89 cm", "141 cm"), bg = "white")
  axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, years, las = 2, cex = 6)
  dev.off()#close png
  #
  # Center
  # ...............................
  png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_temp_center_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
  par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
  #Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  #plot(dudu[, 1], dudu[, 2], type = "l", col = soil.cols[1], ylim = c(-15, 18), xlab = "", ylab = paste(years, "Ts °C BaSoil2009"), xaxt = "n", yaxt = "n")
  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")),
       dudu$Ts_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
       xlim = xxlim, ylim = c(-15, 18), xlab = "Date", ylab = "temperature [°C]",
       xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
  plot_maintenance(years)

  center <- c(8, 25, 60, 6, 24, 40, 62, 76, 99, 112, 125)
  # horizontal lines
  for (ll in seq(-15, 15, 5)) {
    abline(h = ll, col = "gray80")
  }
  # vertical lines
  for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
    lines(c(pp, pp), c(-16, 18), col = "gray80")
  }
  for (qq in 1:11) {#points(dudu[((dudu[, qq*2+21])> 0), 1], dudu[((dudu[, qq*2+21])> 0), qq*2+20], col = "red", pch = 20)
    points(dudu[((dudu[, qq * 2 + 21]) == 0), 1],
           dudu[((dudu[, qq * 2 + 21]) == 0), qq * 2 + 20],
           col = soil.cols[center[qq]], pch = 20)
  }
  lines(c(dudu[vorne, 1], dudu[vorne, 1]), c(-16, 18), col = "aquamarine")
  lines(c(dudu[hinten, 1], dudu[hinten, 1]), c(-16, 18), col = "aquamarine")


  # legend("topleft", cex = 0.8, title = "", lty = 1, lwd = 3, col = soil.cols, y.intersp = 0.8,
  #        box.col = "white", inset = 0.05, seg.len = 0.8, c("    1 cm", "  11 cm", "  21 cm", "  37 cm", "  55 cm", "  71 cm", "  89 cm", "141 cm"), bg = "white")
  axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, years, las = 2, cex = 6)
  dev.off()#close png
  #
  # Right
  # ......................
  png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_temp_right_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
  par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
  #Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  #plot(dudu[, 1], dudu[, 2], type = "l", col = soil.cols[1], ylim = c(-15, 18), xlab = "", ylab = paste(years, "Ts °C BaSoil2009"), xaxt = "n", yaxt = "n")
  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")),
       dudu$Ts_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
       xlim = xxlim, ylim = c(-15, 18), xlab = "Date", ylab = "temperature [°C]",
       xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
  plot_maintenance(years)

  right <- c(9, 19, 52, 5, 13, 31, 50, 61, 90, 114, 118)
  # horizontal lines
  for (ll in seq(-15, 15, 5)) {
    abline(h = ll, col = "gray80")
  }
  # vertical lines
  for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
    lines(c(pp, pp), c(-16, 18), col = "gray80")
  }
  for (qq in 1:11) {#points(dudu[((dudu[, qq*2+43])> 0), 1], dudu[((dudu[, qq*2+43])> 0), qq*2+42], col = "red", pch = 20)
    points(dudu[((dudu[, qq * 2 + 43]) == 0), 1],
           dudu[((dudu[, qq * 2 + 43]) == 0), qq * 2 + 42],
           col = soil.cols[right[qq]], pch = 20)
  }
  lines(c(dudu[vorne, 1], dudu[vorne, 1]), c(-16, 18), col = "aquamarine")
  lines(c(dudu[hinten, 1], dudu[hinten, 1]), c(-16, 18), col = "aquamarine")


  # legend("topleft", cex = 0.8, title = "", lty = 1, lwd = 3, col = soil.cols, y.intersp = 0.8,
  #        box.col = "white", inset = 0.05, seg.len = 0.8, c("    1 cm", "  11 cm", "  21 cm", "  37 cm", "  55 cm", "  71 cm", "  89 cm", "141 cm"), bg = "white")
  axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, years, las = 2, cex = 6)
  dev.off()#close png

  ###############################################################################################################################
  ###############################################################################################################################
  ## tdr conductivity plots

  # NEW:
  yylim <- c(0, 0.04)

  png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_tdr_cond_left_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
  par(mar = c(1, 10, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")),
       dudu$cond_67_4, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
       xlim = xxlim, ylim = yylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  plot_maintenance(years)

  left <- c(4, 16, 17, 33, 38, 66, 91, 99, 108, 117)# ohne vertical (die letzte)
  # horizontal lines
  for (ll in seq(0, max(yylim), .005)) {
    abline(h = ll, col = "gray80")
  }
  # vertical lines
  for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
    lines(c(pp, pp), c(0, max(yylim)), col = "gray80")
  }
  for (qq in 1:9) {
    points(dudu[((dudu[, qq * 2 + 127]) == 0), 1],
           dudu[((dudu[, qq * 2 + 127]) == 0), qq * 2 + 126],
           col = soil.cols[left[qq]], pch = 20)
  }
  lines(c(dudu[vorne, 1], dudu[vorne, 1]), c(-16, 18), col = "aquamarine")
  lines(c(dudu[hinten, 1], dudu[hinten, 1]), c(-16, 18), col = "aquamarine")

  axis(2, at = seq(0, max(yylim), .005), labels = format(seq(0, max(yylim), .005), 3), las = 2, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(max(yylim), 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 0.02, years, las = 2, cex = 6)
  dev.off()#close png
  ###############################################################################################################################
  ###############################################################################################################################
  png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_tdr_cond_center_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
  par(mar = c(1, 10, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")), dudu$cond_67_4, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
       xlim = xxlim, ylim = yylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  plot_maintenance(years)

  center <- c(8, 25, 60, 6, 24, 40, 62, 76, 99, 112, 125)# ohne vertical (die letzte)
  # horizontal lines
  for (ll in seq(0, max(yylim), .005)) {
    abline(h = ll, col = "gray80")
  }
  # vertical lines
  for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
    lines(c(pp, pp), c(0, max(yylim)), col = "gray80")
  }
  for (qq in 1:10) {
    points(dudu[((dudu[, qq * 2 + 149]) == 0), 1],
           dudu[((dudu[, qq * 2 + 149]) == 0), qq * 2 + 148],
           col = soil.cols[center[qq]], pch = 20)}
  lines(c(dudu[vorne, 1], dudu[vorne, 1]), c(-16, 18), col = "aquamarine")
  lines(c(dudu[hinten, 1], dudu[hinten, 1]), c(-16, 18), col = "aquamarine")

  axis(2, at = seq(0, max(yylim), .005), labels = format(seq(0, max(yylim), .005), 3), las = 2, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(max(yylim), 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 0.02, years, las = 2, cex = 6)
  dev.off()#close png
  ###############################################################################################################################
  ###############################################################################################################################
  png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_tdr_cond_right_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
  par(mar = c(1, 10 , 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")),
       dudu$cond_67_4, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
       xlim = xxlim, ylim = yylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  plot_maintenance(years)

  right <- c(9, 19, 52, 5, 13, 31, 50, 61, 90, 114, 118)# ohne vertical (die letzte)
  # horizontal lines
  for (ll in seq(0, max(yylim), .005)) {
    abline(h = ll, col = "gray80")
  }
  # vertical lines
  for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
    lines(c(pp, pp), c(0, max(yylim)), col = "gray80")
  }

  for (qq in 1:10) {
    points(dudu[((dudu[, qq * 2 + 171]) == 0), 1],
           dudu[((dudu[, qq * 2 + 171]) == 0), qq * 2 + 170],
           col = soil.cols[right[qq]], pch = 20)
  }
  lines(c(dudu[vorne, 1], dudu[vorne, 1]), c(-16, 18), col = "aquamarine")
  lines(c(dudu[hinten, 1], dudu[hinten, 1]), c(-16, 18), col = "aquamarine")

  axis(2, at = seq(0, max(yylim), .005), labels = format(seq(0, max(yylim), .005), 3), las = 2, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(max(yylim), 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 0.02, years, las = 2, cex = 6)
  dev.off()#close png




  ################################################################################
  ################################################################################
  ## heatflux plots

  png(paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_heatflux_", years, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(dada$UTC, format = "%Y-%m-%d %H:%M")),
       dada$G_left_24, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
       # OLD: dada$hf_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
       xlim = xxlim, ylim = c(-60, 60), xlab = " ", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
  plot_maintenance(years)

  # horizontal lines
  for (ll in seq(-60, 60, 10)) {
    abline(h = ll, col = "gray80")
  }
  # vertical lines
  for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
    lines(c(pp, pp), c(-60, 60), col = "gray80")
  }

  points(dada$UTC,
         # OLD: dada$hf_1,
         dada$G_left_24,
         pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "maroon4")
  points(dada$UTC,
         dada$G_cen_18,
         #OLD: dada$hf_2,
         pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "darkseagreen4")

  axis(2, at = seq(-60, 60, 10), labels = seq(-60, 60, 10), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = FALSE, las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(55, 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -50, years,
       las = 2, cex = 6)

  dev.off()#close png


###############################################################################################################################
###############################################################################################################################

  #
  #
  #  snow diel
  # -----------------------------------------------------
  if (years >= 1998 && years <= 2009) {#

    if (years == 2009) {#
      db.snowD_b <- read.table(paste(path$w[path$n == "LV1.p"], "BaSoil2009/00_full_dataset/BaSoil2009_", years, "_lv1.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
      db.snowD_b[, 1] <- as.numeric(as.POSIXct(db.snowD_b[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'))

      snowDiel_zero_b <- which(as.numeric(db.snowD_b$E2_sn_v_0_fl) == 0)
      snowDiel_flags_b <- which(as.numeric(db.snowD_b$E2_sn_v_0_fl) == 8)
    }

    ###############################################################################################################################
    ###############################################################################################################################
    png(file = paste(path$w[path$n == "plot.p"], years, "/BaSoil1998_snow_diel_", years, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(dudu$UTC, format = "%Y-%m-%d %H:%M")), dudu$E2_sn_67_0_v, pch = 20,
         # cex.lab = 1.7, cex.axis = 1.5,   #
         xlim = xxlim, ylim = c(0, 10), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
    plot_maintenance(years)

    for (ll in seq(0, 10, 2)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(0, 31), col = "gray80")
    } # vertical lines

    points(dudu$UTC, dudu$E2_sn_67_0_v, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "turquoise")
    points(dudu$UTC[which(as.numeric(dudu$E2_sn_67_0_v_fl) > 0)],
           dudu$E2_sn_67_0_v[which(as.numeric(dudu$E2_sn_67_0_v_fl) > 0)],
           pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "red")


    if (years == 2009) {
      points(db.snowD_b$UTC, db.snowD_b$E2_sn_v_0,
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "turquoise")
      points(db.snowD_b$UTC[ which(as.numeric(db.snowD_b$E2_sn_v_0_fl) > 0)],
             db.snowD_b$E2_sn_v_0[ which(as.numeric(db.snowD_b$E2_sn_v_0_fl) > 0)],
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "red")
    }

    axis(2, at = seq(0, 10, 2), labels = seq(0, 10, 2), las = 2, cex.axis = 4)
    #axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(10, 12),
         labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 8, years, las = 2, cex = 6)
    dev.off()#close png
  }

cat("level1 BaSoil1998_temp ", years, " plot done!\n")
}


