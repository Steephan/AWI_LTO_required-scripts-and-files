############################################################################
#
#
# Legends for wikiplots
#
# by Stephan.Lange@awi.de
#
# last update: 2019-10-14
#
############################################################################
#  to run this script separately, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type == "windows") {
  p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/p.1_win.txt", sep = "\t", header = T)
  p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
  
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
} else {
  p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/p.1_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
  
  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
}

############################################################################
# dimensions of the plot files
#p.width <- (420*3.5)/6;p.height <- 280*3.5
p.height <- 980
p.width <- p.height / 3
############################################################################
#################################################################### Bayelva
#
# Soildata temperatures
#
png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil2009_Ts.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, legend = c(0.01, 0.11, 0.21, 0.37, 0.55, 0.71, 0.89, 1.41),
       col = c("#2E8B57", "#52AA68", "#77C979", "#B3CD26", "#DFDA4B", "#F1D27F", "#EE9F5B", "#A69286"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil \n temperatures \n [°C]", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaSoil2009", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_Ts_1.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c(0.025, 0.123, 0.328, 0.623, 1.023, 1.523),
       col = c("#318E58", "#56AD6A", "#A0CD43", "#EBE279", "#D68C47", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil \n temperatures \n [°C]", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
text(1, 0.05, "under mud boil", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_Ts_2.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c(0.02, 0.05, 0.235, 0.533, 0.933, 1.43),
       col = c("#318E58", "#3C975D", "#7ECD77", "#DCD83E", "#E79A55", "#A08C7F"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil \n temperatures \n [°C]", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
text(1, 0.05, "under vegetation cover", cex = 2)
dev.off()

#
# Climate (air temperature, humidity, precipitation)
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_Tair200.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("temperature", "daily mean"),
       col = c("#CDBE70", "#CD6600"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Air temperature\nat 2 m\n[°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_Tair100.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("temperature", "daily mean"),
       col = c("#CDBE70", "#CD6600"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Air temperature\nat 1 m\n[°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_hum.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("humidity", "daily mean"),
       col = c("#66CDAA", "#458B74"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Humidity\nat 2 m\n[%]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_prec.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("hourly", "daily", "monthly", "annual"),
       col = c("steelblue4", "steelblue3", "steelblue1", "turquoise4"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Precipitation\nat 2 m\n[mm]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

# OLD precip
png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_OLD_PREC.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("hourly", "daily"),
       col = c("#00868B", "#36648B"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Precipitation\nat 2 m\n[mm]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

#
# Snow temperature
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_Tsn.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.05", "0.20", " ", "0.05 (sc)", "0.20 (sc)"),
       col = c("#EE7942", "#9ACD32", "white", "#8B4726", "#698B22"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Snow temperature\n[°C]", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()


#
# Surface temperatures
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_Tsurf.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("temperature", "daily mean"),
       col = c("#CDBE70", "#CD6600"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Surface \n temperature \n [°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_Tsurf_diff.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("positive", "negative"),
       col = c("red", "blue"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Temperature \n difference \n [°C]", cex = 4)
text(1, 1.6, "T[2 m] - T[0 m]", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

#
# Mr. Moustache temperature profile
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaHole_Ts.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.0", "0.5", "1.0", "1.5", "2.0", "2.5", "3.5", "5.5", "7.5", "9.0"),
       col = c("#2E8B57", "#469F62", "#65B971", "#84CD6F", "#A4CD3E", "#C3CD0D", "#E4DD5E", "#EA9C57", "#CDAE9F", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Soil temperature\n[°C]", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
#text(1, 0.55, "5.5 m since\nSeptember 2015", cex = 2)
text(1, 0.2, "BaHole", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaHole_Ts_mean.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
       col = c("#B9D3EE", "#87CEFF", "#90EE90", "#B3EE3A", "#698B22", "#FF4500", "#FF0000", "#8B0000", "#FFB90F", "#8B5A2B", "#666666", "#36648B"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Soil temperature\nmonthly mean\n[°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaHole", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

#
# Radiation
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_rad_net.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("netto"),
       col = c("#8B1C62"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Radiation \n [W/m²]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_rad_gl.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("global"),
       col = c("#8B5742"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Radiation \n [W/m²]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_rad_sw.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("in", "out"),
       col = c("#8968CD", "#CDC673"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Radiation \n short wave \n [W/m²]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_rad_lw.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("in", "out"),
       col = c("#8968CD", "#CDC673"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Radiation \n long wave \n [W/m²]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_albedo.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("albedo", "flagged"),
       col = c("#551A8B", "#FF0000"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Albedo ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

#
# Snow depth
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSnow2013_product.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("depth", "data switch"),
       col = c("#8B8989", "#B03060"),
       lty = , cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow depth \n product \n [m]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaSnow2013", cex = 2)
text(1, 0.15, "78.920806°, 11.833000°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSnow2013_compare.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("SHM30", "BaSnow2013", "", "SR50", "BaMet1998", "BaMet2009", "BaEddy2007", "BaSnow2019"),
       col = c("#FFFFFF", "#5D478B", "#FFFFFF", "#FFFFFF", "#008B8B", "#6E8B3D", "#8B4513", "#0000FF"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5, y.intersp = 1.5)
text(1, 1.9, " Snow depth \n [m]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaSnow, BaMet\n78.921000°, 11.832917°\n\nBaEddy\n78.921640°, 11.830920°", cex = 2)
dev.off()

#
# Snow water equivalent (SWE)
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSnow2019cs_product.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("SWE", "data switch"),
       col = c("#8B8989", "#B03060"),
       lty = , cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " SWE \n product \n [mm]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaSnow2013", cex = 2)
text(1, 0.15, "78.92091°, 11.83277°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSnow2019cs_swe.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("SWE_K", "SWE_Tl"),
       col = c("#0000FF", "#FF0000FF"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5, y.intersp = 1.5)
text(1, 1.9, " SWE \n [mm]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaSnow2019 \n78.92091°, 11.83277°", cex = 2)
dev.off()

#
# Wind
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_wind.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("direction", "speed", "daily mean"),
       col = c("#00008B", "#00CD00", "#006400"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Wind direction [deg] \n and speed [m/s]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaMet2009_wrose.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0-3", "3-6", "6-9", ">9"),
       col = c("#78EB75", "#75EBB7", "#75DAEB", "#7596EB"),
       lty = 1, cex = 4, lwd = 15, seg.len = 0, bty = "n", xjust = 0.5)
text(1, 1.9, " Windrose \n speed \n [m/s]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaMet2009", cex = 2)
text(1, 0.15, "78.921000°, 11.832917°", cex = 2)
dev.off()

#
# Snow TDR
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil2009_snow_diel.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("dielectricity", "flagged"),
       col = c("#40E0D0", "red"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow TDR ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaSoil2009", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

#
# Soil TDR
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil2009_tdr_cond.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.01", "0.11", "0.21", "0.37", "0.55", "0.71", "0.89"),
       col = c("#2E8B57", "#7CCD7C", "#CDCD00", "#F0E68C", "#F4A460", "#CD853F", "#CDB7B5"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil TDR \n conductivity ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaSoil2009", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil2009_tdr_vwc.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.01", "0.11", "0.21", "0.37", "0.55", "0.71", "0.89"),
       col = c("#2E8B57", "#7CCD7C", "#CDCD00", "#F0E68C", "#F4A460", "#CD853F", "#CDB7B5"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil TDR \n volumetric \n water content ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaSoil2009", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil2009_tdr_ice.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.01", "0.11", "0.21", "0.37", "0.55", "0.71", "0.89"),
       col = c("#2E8B57", "#7CCD7C", "#CDCD00", "#F0E68C", "#F4A460", "#CD853F", "#CDB7B5"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil TDR \n ice ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaSoil2009", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil2009_tdr_icegrid.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("high", "medium", "low"),
       col = c("#5C5CA0", "#F5FFE8", "#9AFF9A"),
       lty = 1, cex = 4, lwd = 15, seg.len = 0, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil TDR \n ice content \n at depth ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "BaSoil2009", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil2009_tdr_diel.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.01", "0.11", "0.21", "0.37", "0.55", "0.71", "0.89"),
       col = c("#2E8B57", "#7CCD7C", "#CDCD00", "#F0E68C", "#F4A460", "#CD853F", "#CDB7B5"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil TDR \n dielectricity ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaSoil2009", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

#
# Soil Temperature (2D) 1998-2012
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil1998_Ts.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.00", "0.25", "0.50", "0.75", "1.00", "1.25", "1.50"),
       col = c("#2E8B57", "#86CD6C", "#D7D42A", "#F1C677", "#DA8F4A", "#CDAEA0", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil \n temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "BaSoil1998", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

#
# Soil heat flux 1998-2012
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/BaSoil1998_heatflux.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("left\n24cm", "center\n18cm"),
       col = c("#8B1C62", "#698B69"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5, y.intersp = 2)
text(1, 1.9, "Soil heat flux", cex = 4)
text(1, 1.6, "heat flux plates", cex = 3)
text(1, 0.2, "BaSoil1998", cex = 2)
text(1, 0.15, "78.92083°, 11.83421°", cex = 2)
dev.off()

#################################################################### Samoylov
#
# SaSoil2002 SoilT
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2002_temp_center.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.01", "0.05", "0.10", "0.20", "0.30", "0.40"),
       col = c("#2E8B57", "#4DA565", "#74C678", "#C4CD0C", "#ECE37E", "#F3AA64"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n (center) \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2002", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2002_temp_slope.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.03", "0.07", "0.16", "0.22", "0.32", "0.42"),
       col = c("#3D985E", "#5CB26D", "#A4CD3E", "#D0CF0E", "#F0DF87", "#F0A05C"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n (slope) \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2002", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2002_temp_rim.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.02", "0.06", "0.11", "0.16", "0.21", "0.27", "0.33", "0.38", "0.51", "0.61", "0.71"),
       col = c("#35915A", "#55AC69", "#7CCD7C", "#A4CD3E", "#CDCD00", "#E2DC54", "#F0D883", "#F2B76D", "#CD853F", "#CDB7B5", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n (rim) \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2002", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

#
# SaMet2002 SoilT
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaMet2002_Ts.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.01", "0.03", "0.06"),
       col = c("#7CCD7C", "#F4A460", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaMet2002", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

#
# SaMet2002 precipitation
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaMet2002_prec.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("hourly", "daily", "monthly", "annual"),
       col = c("steelblue4", "steelblue3", "steelblue1", "turquoise4"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, "Precipitation\nat 2 m\n[mm]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaMet2002", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()


#
# SaSoil2010 SoilT
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2010_temp_center.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.01", "0.05", "0.10", "0.20", "0.30", "0.40"),
       col = c("#2E8B57", "#4DA565", "#74C678", "#C4CD0C", "#ECE37E", "#F3AA64"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n (center) \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2010", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2010_temp_slope.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.03", "0.07", "0.16", "0.22", "0.32", "0.42"),
       col = c("#3D985E", "#5CB26D", "#A4CD3E", "#D0CF0E", "#F0DF87", "#F0A05C"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n (slope) \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2010", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2010_temp_rim.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.02", "0.06", "0.11", "0.16", "0.21", "0.27", "0.33", "0.38", "0.51", "0.61", "0.71"),
       col = c("#35915A", "#55AC69", "#7CCD7C", "#A4CD3E", "#CDCD00", "#E2DC54", "#F0D883", "#F2B76D", "#CD853F", "#CDB7B5", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n (rim) \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2010", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

#
# SaSoil2002 SoilT (icewedge)
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2002_temp_icewedge.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.61", "0.91", "1.21", "1.51", "1.81", "2.11", "2.41", "2.71"),
       col = c("#A7CD39", "#D7D429", "#F0E189", "#F3AF67", "#DC914C", "#CD976A", "#C4AEAA", "#917D6D"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n icewedge \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2002", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2002_icewedge_trompete.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
       col = c("#b9d3ee", "#87ceff", "#90ee90", "#b3ee3a", "#698b22", "#ff4500", "#ff0000", "#8b0000", "#ffb90f", "#8b5a2b", "#666666", "#36648b"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n monthly mean \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSoil2002", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

#
# SaSoil2010 SoilT (icewedge)
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2010_temp_icewedge.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.41", "0.61", "0.91", "1.21", "1.51", "1.81", "2.11", "2.41", "2.71"),
       col = c("#7DCD79", "#A7CD39", "#D7D429", "#F0E189", "#F3AF67", "#DC914C", "#CD976A", "#C4AEAA", "#917D6D"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n icewedge \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSoil2010", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2010_icewedge_trompete.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
       col = c("#b9d3ee", "#87ceff", "#90ee90", "#b3ee3a", "#698b22", "#ff4500", "#ff0000", "#8b0000", "#ffb90f", "#8b5a2b", "#666666", "#36648b"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n monthly mean \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSoil2010", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

#
# SaPond2006
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2006_temperature.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("00", "16", "33"),
       col = c("#F4A460", "#C3956D", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [cm]", cex = 3)
text(1, 0.2, "SaPond2006", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2006_Tw_edge.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("00", "20", "33"),
       col = c("#473C8B", "#5C6CAB", "#719DCB"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Water temperature \n (edge) \n [°C] ", cex = 4)
text(1, 1.6, "depth [cm]", cex = 3)
text(1, 0.2, "SaPond2006", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2006_Tw_center.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("05", "10", "15", "67", "86", "101"),
       col = c("#473C8B", "#5C6CAB", "#719DCB", "#87CEEB", "#8FBC8F", "#556B2F"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Water temperature \n (center) \n [°C] ", cex = 4)
text(1, 1.6, "depth [cm]", cex = 3)
text(1, 0.2, "SaPond2006", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

#
# SaPond2014
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2014_temperature.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("", "40", "30", "20", "10", "", "0", "", "10", "20", "30"),
       col = c("#FFFFFF", "#87CEEB", "#719DCB", "#5C6CAB", "#473C8B", "#FFFFFF", "#CDCD00", "#FFFFFF", "#F4A460", "#BF8D62", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil/water \n temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [cm]", cex = 3)
text(1, 1.5, "water", cex = 3)
text(1, 1.08, "water/soil", cex = 3)
text(1, 0.91, "soil", cex = 3)
text(1, 0.2, "SaPond2014", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2014_trompete.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
       col = c("#b9d3ee", "#87ceff", "#90ee90", "#b3ee3a", "#698b22", "#ff4500", "#ff0000", "#8b0000", "#ffb90f", "#8b5a2b", "#666666", "#36648b"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n monthly mean \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaPond2014", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

#
# SaHole2006
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaHole2006_temperature.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.58, c("00.00", "00.75", "01.75", "02.75", "03.75", "04.75", "05.75", "06.75", "07.75", "08.75", "09.75", "10.75", "11.75", "12.75", "13.75", "14.75", "15.75", "16.75", "17.75", "18.75", "20.75", "22.75", "24.75", "26.75"),
       col = colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(24),
       lty = 1, cex = 2.5, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaHole2006", cex = 2)
text(1, 0.15, "72.36944°, 126.47644°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaHole2006_trompete.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
       col = c("#b9d3ee", "#87ceff", "#90ee90", "#b3ee3a", "#698b22", "#ff4500", "#ff0000", "#8b0000", "#ffb90f", "#8b5a2b", "#666666", "#36648b"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n monthly mean \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaHole2006", cex = 2)
text(1, 0.15, "72.36944°, 126.47644°", cex = 2)
dev.off()

#
# SaHole2010
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaHole2010_temperature.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0", "1", "2", "3", "4", "5", "6", "7", "8"),
       col = colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(9),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaHole2010", cex = 2)
text(1, 0.15, "72.37381°, 126.49625°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaHole2010_trompete.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
       col = c("#b9d3ee", "#87ceff", "#90ee90", "#b3ee3a", "#698b22", "#ff4500", "#ff0000", "#8b0000", "#ffb90f", "#8b5a2b", "#666666", "#36648b"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n monthly mean \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaHole2010", cex = 2)
text(1, 0.15, "72.37381°, 126.49625°", cex = 2)
dev.off()

#
# SaHole2018
#
depth <- c(1:20, 22, 24, 26, 28, 30, 35, 40, 45, 50, 55, 60)

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaHole2018_temperature.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, depth,
       col = colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(60)[depth],
       lty = 1, cex = 2, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaHole2018", cex = 2)
text(1, 0.15, "72.2237°, 126.2850°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaHole2018_trompete.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
       col = c("#b9d3ee", "#87ceff", "#90ee90", "#b3ee3a", "#698b22", "#ff4500", "#ff0000", "#8b0000", "#ffb90f", "#8b5a2b", "#666666", "#36648b"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n monthly mean \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaHole2018", cex = 2)
text(1, 0.15, "72.2237°, 126.2850°", cex = 2)
dev.off()

#
# SaSnow2012 - soil temperature
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_Ts.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.00", "0.05", "0.10", "0.20", "0.40", "0.60", "0.80", "1.00"),
       col = c("#2E8B57", "#52AA68", "#77C979", "#B3CD26", "#DFDA4B", "#F1D27F", "#EE9F5B", "#A69286"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_airt.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.80 m", "daily mean"),
       col = c("#CDBE70", "#CD6600"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Air temperature \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

#
# SaSnow2012 Snow Depth
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_sh.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
       col = colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(10),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow \n heights \n [m] ", cex = 4)
text(1, 1.6, "sensor", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

#
# SaSnow2012 Snow Temperature
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_snowt.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("0.00", "0.10", "0.20", "0.45"),
       col = c("#6A5ACD", "#EE7AE9", "#9ACD32", "#EE7942"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow \n temperature \n [°C] ", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

#
# SnowPackAnalyzer - snow properties
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_rho.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("2", "3", "4"),
       col = c("#6A5ACD", "#EE7AE9", "#9ACD32"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow \n density \n [kg/m³] ", cex = 4)
text(1, 1.6, "band number", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_wc.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("02", "03", "04"),
       col = c("chocolate", "burlywood4", "darkseagreen"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Water \n content \n [%] ", cex = 4)
text(1, 1.6, "sensor", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

#
# SaPond2006 Snow depth
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2006_Dsn.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("snow depth"),
       col = c("#8B8989"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow \n depth \n [m] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaPond2006", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2006_WT.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("water table"),
       col = c("#8B8989"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Water \n table \n [m] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaPond2006", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

#
# SaSoil2010
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSoil2010_sh.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("snow heights"),
       col = c("#7CCD7C"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow \n heights \n [m] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSoil2010", cex = 2)
text(1, 0.15, "72.36997°, 126.48069°", cex = 2)
dev.off()

#
# SaSnow2016 Snow heights & Air temperature
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2016_sh.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("snow heights"),
       col = c("#00FF00"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Snow \n heights \n [m] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2016", cex = 2)
text(1, 0.15, "72.36944°, 126.47644°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2016_airt.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("temperature", "daily mean"),
       col = c("#CDBE70", "#CD6600"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Air \n temperature \n [°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2016", cex = 2)
text(1, 0.15, "72.36944°, 126.47644°", cex = 2)
dev.off()

#
# SaPond2006 Radiation
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2006_rad_net1.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("netto radiation"),
       col = c("#8B1C62"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Netto radiation \n (edge) \n [W/m²]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaPond2006", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaPond2006_rad_net2.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("netto radiation"),
       col = c("#8B1C62"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Netto radiation \n (center) \n [W/m²]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaPond2006", cex = 2)
text(1, 0.15, "72.37008°, 126.48339°", cex = 2)
dev.off()

#
# SaSnow2012 air temperature, humidity, precipitation
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_airt.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("temperature", "daily mean"),
       col = c("#CDBE70", "#CD6600"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Air temperature \n at 0.8 m \n [°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_hum.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("humidity", "daily mean"),
       col = c("#66CDAA", "#458B74"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Humidity \n at 2 m \n [°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2012_prec.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("hourly", "daily"),
       col = c("#00868B", "#36648B"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Precipitation \n at 2 m \n [°C]", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2012", cex = 2)
text(1, 0.15, "72.37419°, 126.49589°", cex = 2)
dev.off()

#
# SaSnow2016 Status
#

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2016_status_T.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("temperature"),
       col = c("green"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Panel \n temperature \n CR1000 \n [°C] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2016", cex = 2)
text(1, 0.15, "72.36944°, 126.47644°", cex = 2)
dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], "Legends/SaSnow2016_status_U.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, c("voltage"),
       col = c("blue"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Battery \n voltage \n [V] ", cex = 4)
text(1, 1.6, "", cex = 3)
text(1, 0.2, "SaSnow2016", cex = 2)
text(1, 0.15, "72.36944°, 126.47644°", cex = 2)
dev.off()


########################################
# Trail Valley Creek

# soil temperature
png(paste(p.1$w[p.1$n == "plot.p"], "Legends/TVCSoil2016_Ts.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, legend = c("0.01", "0.05", "0.10", "0.20"),
       col = c("#2E8B57", "#A2CD41", "#F1D17E", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Soil \n temperatures \n [°C]", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "TVCSoil2016", cex = 2)
text(1, 0.15, "68.74172°, -133.49689 E°", cex = 2)
dev.off()


# volumetric water content
# horizontal
png(paste(p.1$w[p.1$n == "plot.p"], "Legends/TVCSoil2016_vwc_h.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, legend = c("0.01", "0.05", "0.10", "0.20"),
       col = c("#2E8B57", "#A2CD41", "#F1D17E", "#8B7765"),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Volumetric \n water content \n [m^3 / m^3]", cex = 4)
text(1, 1.6, "depth [m]", cex = 3)
text(1, 0.2, "TVCSoil2016", cex = 2)
text(1, 0.15, "68.74172°, -133.49689 E°", cex = 2)
dev.off()

# vertical
png(paste(p.1$w[p.1$n == "plot.p"], "Legends/TVCSoil2016_vwc_v.png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
plot(1, 1, type = "n", xlab = "", ylab  =  "", xaxt = "n", yaxt = "n", cex.axis = 3, xlim = c(0, 2), ylim = c(0, 2))
legend(1, 1.6, legend = c("v_0", "a_v_0", "b_v_0"),
       col = c('#a6cee3','#1f78b4','#b2df8a'),
       lty = 1, cex = 4, lwd = 4, bty = "n", xjust = 0.5)
text(1, 1.9, " Volumetric \n water content \n [m^3 / m^3]", cex = 4)
text(1, 1.6, "vertical sensors", cex = 3) #depth [m]", cex = 3)
text(1, 0.2, "TVCSoil2016", cex = 2)
text(1, 0.15, "68.74172°, -133.49689 E°", cex = 2)
dev.off()


