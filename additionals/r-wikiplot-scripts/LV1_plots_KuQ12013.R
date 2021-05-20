###.........................................................................
##
##   Level1 plots to wiki -----
###.........................................................................
##   KuQ12013 -----
##
##   by: Stephan.Lange@awi.de
## 
##   last modified:  2021-05-19
##
##   last check: -----
##   checked by:Stephan.Lange@awi.de
###.........................................................................
##
## last modification: -----
##   
##    2021-05-19 SL created
###.........................................................................
# to run this script separately, you have to uncomment the next 10 lines!
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
###.........................................................................

# for non-exponential display of numeric values
options(scipen = 100, stringsAsFactors = F, scientific = T) # digits = 2,

origin <- "1970-01-01"
first.year <- 2013
recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# labels for months
# months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# plot options
p.width <- 420 * 3.5
p.height <- 280 * 3.5
# color to mark the maintenance periods. The maintenance periods are marked as vertical shaded areas.
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)


# list of colors with number of colors according to the number of variables to be plotted
lv1.colors <- c("maroon4", "darkseagreen4")
# define ticks of y-axis
# range of y-axis and ticks has to be set manually,
# because for different variables different ranges can yield better readable plots

###.........................................................................

###.........................................................................
# to run this script separately, you have to set run.year:
# run.year <- first.year:2018
# run.year <- 2016
###.........................................................................


# loop for annual plots -----
for (year in run.year) {
  # load data -----
  lv1.dat <- read.table(paste0(p.1$w[p.1$n == "LV1.p"], "KuQ12013/00_full_dataset/KuQ12013_", year, "_lv1.dat"),
                        sep = ",", dec = ".", header = T, na.strings = "NA")
  
  xticks <- c(lv1.dat$UTC[format(strptime(lv1.dat$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              lv1.dat$UTC[length(lv1.dat$UTC)])
  # transform date column as numeric values for plotting
  lv1.dat[, 1] <- as.numeric(as.POSIXct(lv1.dat[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'))
  # define range of axis
  xxlim <- c(as.numeric(strptime(paste0("13.01.", year), format = "%d.%m.%Y")),
             as.numeric(strptime(paste0("20.12.", year), format = "%d.%m.%Y")))
  yylim <- c(0, 200)
  yticks <- seq(min(yylim), max(yylim), by = 50)
  # plot G -----
  png(paste(p.1$w[p.1$n == "plot.p"], year, "/KuQ12013_Q_", year, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)
  # define empty plot
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(lv1.dat$UTC, format = "%Y-%m-%d %H:%M")), lv1.dat$Q,
       xlim = xxlim, ylim = yylim, xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
  
  plot_maintenance(year)
  
  # grid
  # horizontal lines
  abline(h = yticks, col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(xticks, format = "%Y-%m-%d %H:%M")), col = "gray80")
  
  points(lv1.dat$UTC[lv1.dat$Q_fl==0], lv1.dat$Q[lv1.dat$Q_fl==0],    pch = 20, col = "darkseagreen4")
  points(lv1.dat$UTC[lv1.dat$Q_fl>0], lv1.dat$Q[lv1.dat$Q_fl>0],    pch = 20, col = "red")
  # axis and labels
  axis(2, at = yticks, labels = yticks, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(xticks[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = FALSE, las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(xticks[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(3, 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(xticks[11], format = "%Y-%m-%d %H:%M")) + 2000000, -50, year,
       las = 2, cex = 6)
  dev.off()

  yylim <- c(-100, 100)
  yticks <- seq(min(yylim), max(yylim), by = 20)
  # plot WT -----
  png(paste(p.1$w[p.1$n == "plot.p"], year, "/KuQ12013_wt_", year, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)
  # define empty plot
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(lv1.dat$UTC, format = "%Y-%m-%d %H:%M")), lv1.dat$WT,
       xlim = xxlim, ylim = yylim, xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
  
  plot_maintenance(year)
  
  # grid
  # horizontal lines
  abline(h = yticks, col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(xticks, format = "%Y-%m-%d %H:%M")), col = "gray80")
  
  points(lv1.dat$UTC[lv1.dat$WT_fl==0], lv1.dat$Q[lv1.dat$WT_fl==0],    pch = 20, col = "darkseagreen4")
  points(lv1.dat$UTC[lv1.dat$WT_fl>0], lv1.dat$Q[lv1.dat$WT_fl>0],    pch = 20, col = "red")
  # axis and labels
  axis(2, at = yticks, labels = yticks, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(xticks[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = FALSE, las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(xticks[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(3, 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(xticks[11], format = "%Y-%m-%d %H:%M")) + 2000000, -50, year,
       las = 2, cex = 6)
  dev.off()
  
  cat("#\n# level1 KuQ12013", year, " plot done!\n#\n")
}
