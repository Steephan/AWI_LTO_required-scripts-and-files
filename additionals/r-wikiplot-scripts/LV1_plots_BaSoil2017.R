#############################################################################
##
##   Level1 plots to wiki
##   --------------------
##   BaSoil2017 heat flux
##
##   by: Stephan.Lange@awi.de
##       christian.lehr@awi.de
##
##   last modified: 2020-04-01
##
##   last check:
##   checked by:
#############################################################################
##
## last modification:
##
##
##
#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type  ==  "windows") {
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

# for non-exponential display of numeric values
options(scipen = 100, stringsAsFactors = F, scientific = T) # digits = 2,

origin <- "1970-01-01"
first.year <- 2017
recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# labels for months
# months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# plot options
p.width <- 420 * 3.5
p.height <- 280 * 3.5
# color to mark the maintenance periods. The maintenance periods are marked as vertical shaded areas.
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

##############
# has to be set manually:
# number of variables to be plotted:
n.var <- 2
# list of colors with number of colors according to the number of variables to be plotted
lv1.colors <- c("maroon4", "darkseagreen4")
# define ticks of y-axis
# range of y-axis and ticks has to be set manually,
# because for different variables different ranges can yield better readable plots
yylim <- c(-60, 60)
yticks <- seq(min(yylim), max(yylim), by = 10)
##############

########
# to run this script separately, you have to set run.year:
# run.year <- first.year:recent.year
#######


# loop for annual plots
for (year in run.year) {
  lv1.dat <- read.table(paste0(path$w[path$n == "LV1.p"], "BaSoil2017/00_full_dataset/BaSoil2017_", year, "_lv1.dat"),
                     sep = ",", dec = ".", header = T, na.strings = "NA")

  # define ticks of x-axis (dates)
  xticks <- c(lv1.dat$UTC[format(strptime(lv1.dat$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              lv1.dat$UTC[length(lv1.dat$UTC)])
  # transform date column as numeric values for plotting
  lv1.dat[, 1] <- as.numeric(as.POSIXct(lv1.dat[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'))
  # define range of axis
  xxlim <- c(as.numeric(strptime(paste0("13.01.", year), format = "%d.%m.%Y")),
            as.numeric(strptime(paste0("20.12.", year), format = "%d.%m.%Y")))

  # print png
  png(paste(path$w[path$n == "plot.p"], year, "/BaSoil2017_heatflux_", year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
  # define empty plot
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(lv1.dat$UTC, format = "%Y-%m-%d %H:%M")), lv1.dat[, 2],
     xlim = xxlim, ylim = yylim, xlab = "", ylab = "",
     xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)

  # mark maintenance periods
  plot_maintenance(year)

  # grid
  # horizontal lines
  abline(h = yticks, col = "gray80")
  # vertical lines
  abline(v = as.numeric(strptime(xticks, format = "%Y-%m-%d %H:%M")), col = "gray80")

  # plot data points
  for (i in 1:length(lv1.colors)) {
    points(lv1.dat$UTC, lv1.dat[, seq(from = 2, to = n.var * 2, by = 2)[i]],
           pch = 20, col = lv1.colors[i])
  }

  # axis and labels
  axis(2, at = yticks, labels = yticks, las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(xticks[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = FALSE, las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(xticks[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(55, 12),
       labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(xticks[11], format = "%Y-%m-%d %H:%M")) + 2000000, -50, year,
       las = 2, cex = 6)

  # close png
  dev.off()

  cat("level1 BaSoil2017_heatflux ", year, " plot done!\n")
}
