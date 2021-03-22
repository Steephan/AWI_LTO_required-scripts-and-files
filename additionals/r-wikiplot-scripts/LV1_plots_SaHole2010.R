#############################################################################
##
##   SaHole2010          Level1 plots
##
##   no extremes
##
##   by: Stephan.Lange@awi.de
##   last modified:
##   2020-08-03 CL Script style updated
##
##
#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type ==  "windows") {
#   path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
#   maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
#   p.1 <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt", sep = "\t", header = T)
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
#############################################################################

options(scipen = 100, stringsAsFactors = F, digits = 2, scientific = T) # for non-exponential display of numeric values
origin <- "1970-01-01"

# run.year <- c(2010:2017)
#run.year <- c(2016:2017)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(70)
mon.cols <- c("slategray2", "skyblue1", "lightgreen", "olivedrab2", "olivedrab4", "orangered", "red", "red4", "darkgoldenrod1", "tan4", "gray40", "steelblue4")

p.width <- 420 * 3.5
p.height <- 280 * 3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)


for (year in run.year) {

  db.bahole.lvl1 <- read.table(paste(path$w[path$n == "LV1.p"], "SaHole2010/00_full_dataset/SaHole2010_", year, "_lv1.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
  db.bahole.lvl2 <- db.bahole.lvl1
  for (val in 1:8) {# set data to NA if flag is not 0
    db.bahole.lvl2[which(as.numeric(db.bahole.lvl2[, (val * 2) + 1]) >= 1), (val * 2)] <- NA
  }
  db.bahole.extra <- db.bahole.lvl2[, c(1, c(1:8) * 2)]
  db.bahole.extra$monate <- format(as.Date(db.bahole.extra[, 1]), format = "%m")

  stats.db <- aggregate(db.bahole.extra[, 2:9], by = list(db.bahole.extra$monate), FUN = mean, na.rm = TRUE)[2:9]

  y.values <- c(seq(0, 70, 10) * (-0.1))

  png(paste(path$w[path$n  ==  "plot.p"], year, "/SaHole2010_trompete_", year, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
  plot(c(stats.db[1, ]), y.values, type = "n", xlim = c(-30, 10), ylim = c(-7.2, 1),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex.axis = 3)

  # grid
  # horizontal lines
  abline(h = seq(-10, 0, 1), col = "gray80")
  # vertical lines
  for (pp in seq(-30, 10, 2.5)) {lines(c(pp, pp), c(-10, 0.2), col = "gray80")}

  # monthly temperature lines
  for (qqq in 1:12) {lines(c(stats.db[qqq, 1:8]), y.values, col = mon.cols[qqq], lwd = 2)}

  text(seq(-30, 10, 2.5), rep(0.8, 17), labels = seq(-30, 10, 2.5), las = 2, cex = 4)
  axis(2, at = seq(-30, 30, 1), labels = seq(-30, 30, 1), las = 2, cex.axis = 4)
  legend(7, -5, months, col = mon.cols, lty = 1, cex = 2, lwd = 4)
  text(7, -4, year, las = 2, cex = 6)
  dev.off()


  ## yearly LEVEL 0 plots
  #  =========================================================================================================

  xxlim <- c(as.numeric(strptime(paste0("13.01.", year), format = "%d.%m.%Y")), as.numeric(strptime(paste0("20.12.", year), format = "%d.%m.%Y")))
  lischt <- c(db.bahole.lvl1$UTC[format(strptime(db.bahole.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              db.bahole.lvl1$UTC[length(db.bahole.lvl1$UTC)])
  tair_gut <- which(db.bahole.lvl1$Ts_0_fl == 0)


  png(paste(path$w[path$n == "plot.p"], year, "/SaHole2010_temperature_", year, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)
  par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(db.bahole.lvl1$UTC[tair_gut], format = "%Y-%m-%d %H:%M")),
       db.bahole.lvl1$Ts_0[tair_gut], pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
       xlim = xxlim, ylim = c(-40, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
  #plot_maintenance(year)

  # grid
  # horizontal lines
  abline(h = seq(-40, 30, 10), col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

  depth <- c(1, seq(10, 70, 10))
  for (qq in rev(1:8)) {
    points(as.numeric(strptime(db.bahole.lvl1[((db.bahole.lvl1[, qq * 2 + 1]) == 0), 1], format = "%Y-%m-%d %H:%M")),
           db.bahole.lvl1[((db.bahole.lvl1[, qq * 2 + 1])  ==  0), qq * 2], col = soil.cols[depth[qq]], pch = 20)
    # mark all values with flag > 0 in red
    points(as.numeric(strptime(db.bahole.lvl1[((db.bahole.lvl1[, qq * 2 + 1]) > 1), 1], format = "%Y-%m-%d %H:%M")),
           db.bahole.lvl1[((db.bahole.lvl1[, qq * 2 + 1]) > 1), qq*2], col = "red", pch = 20)
  }
  axis(2, at = seq(-40, 30, 10), labels = seq(-40, 30, 10), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year, las = 2, cex = 6)
  #if(year ==  2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M"))+1000000, 0, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")}
  dev.off()


  cat("#\n# level1 SaHole2010 ", year, " plot done!\n#\n")
}
