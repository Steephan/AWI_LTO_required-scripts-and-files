###.....................................................................
##
##   Level1 plots to wiki ----
##   .....................................................................
##   BaMet2009
##
##   by: Stephan.Lange@awi.de -2200
##   updated by: christian.lehr@awi.de
##   last modified: 2020-04-15
##
###.....................................................................
##
##  last modification:
##  2021-05-12 SL adapted to runner app and content management
##  2020-10-01 CL new precipitation plot format added
##	2020-04-15 CL update format of script and some plots
##  - longterm plots:
##    - of soil temperature sensors Ts_252 and Ts_203 every monday
##    - delete the " +  1" in the labeling of years
##    - new plotformat
##    - update of p.1s
##    - no separate year files any more for variables
##
##
##
###.....................................................................
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
###.....................................................................

# for non-exponential display of numeric values
options(scipen = 100, stringsAsFactors = F, scientific = T)  #, digits = 2
origin <- "1970-01-01"
recent.year <- as.numeric(format(Sys.Date(),"%Y"))
first.year <- 2010

zack <- 1
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

p.width <- 420 * 3.5
p.height <- 280 * 3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

####
# to run this script separately, you have to set run.year:
# run.year <- first.year:recent.year
# run.year <- 2020
#
### ATTENTION: Don?t use year 2009.
# The plot of year 2009 is produced with script LV1_plots_BaMet1998.R
#
###


for (t.year in run.year) {

################ Load data #########################
if (zack == 1) { # 1
  #db.bamet     <-read.table(paste(p.1$w[p.1$n == "LV1.p"], "BaMet2009/00_full_dataset/BaMet2009_", t.year, "_lv1_noflag.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
  db.bamet.lvl1 <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "BaMet2009/00_full_dataset/BaMet2009_", t.year, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)
  db.last.year <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "BaMet2009/00_full_dataset/BaMet2009_", t.year - 1, "_lv1.dat", sep = ""),
                             sep = ",", dec = ".", header = T, fill = TRUE)
  last.von <- length(na.omit(db.bamet.lvl1[, 2]))
  last.bis <- length(db.last.year[, 1])

  xxlim <- c(as.numeric(strptime(paste0("13.01.", t.year), format = "%d.%m.%Y")),
             as.numeric(strptime(paste0("20.12.", t.year), format = "%d.%m.%Y")))

  ylim_sw <- plot_bounderies(db.bamet.lvl1$SwOut, db.bamet.lvl1$SwIn)      # get plotting bounderies shortwave
  ylim_lw <- plot_bounderies(db.bamet.lvl1$LwOut, db.bamet.lvl1$LwIn)  # get plotting bounderies longwave
  lischt <- c(db.bamet.lvl1$UTC[format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              db.bamet.lvl1$UTC[length(db.bamet.lvl1$UTC)])

}# Load data

################ Albedo ######
#  albedo (from file vs. calculated (Out/In))
if (zack == 1) {
albedo_good <- which(db.bamet.lvl1$Albedo_fl == 0)
albedo_bad <- which(db.bamet.lvl1$Albedo_fl > 1)
ohne.albedo <- which(db.bamet.lvl1$Albedo_fl == 1)
alb.outside <- which(db.bamet.lvl1$Albedo < 0 | db.bamet.lvl1$Albedo > 1)

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_albedo_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$Albedo, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-1, 2), xlab = "Date", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)

plot_maintenance(t.year)

# horizontal lines
abline(h = seq(-4, 4, 1), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

if (t.year != 2009) {
  points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
         db.last.year$SwIn[last.von:last.bis] / db.last.year$SwOut[last.von:last.bis],
         pch = 20, cex.lab = 1.5, cex.axis = 1.7,
         xlim = xxlim, ylim = c(-5, 5), col = "snow2")             # albedo = out/in
  points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
         db.last.year$Albedo[last.von:last.bis],
         pch = 20, cex.lab = 1.5,
         xlim = xxlim, ylim = c(-5, 5), col = "snow3")             #
}

points(as.numeric(strptime(db.bamet.lvl1$UTC[ohne.albedo], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$SwIn[ohne.albedo] / db.bamet.lvl1$SwOut[ohne.albedo],
       pch = 20, cex.lab = 1.5, cex.axis = 1.7,
       xlim = xxlim, ylim = c(-5, 5), col = "plum")             # albedo = out/in
points(as.numeric(strptime(db.bamet.lvl1$UTC[albedo_good], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Albedo[albedo_good],
       pch = 20, cex.lab = 1.5,
       xlim = xxlim, ylim = c(-5, 5), col = "purple4")             #
points(as.numeric(strptime(db.bamet.lvl1$UTC[alb.outside], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Albedo[alb.outside],
       pch = 20, cex.lab = 1.5,
       xlim = xxlim, ylim = c(-5, 5), col = "red")
axis(2, at = c(0, 0.5, 1), labels = c(0, 50, 100), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(1.9, 12), labels = Months, las = 2, cex = 4)
 text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 1.5, t.year, las = 2, cex = 6)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}

# legend("bottom", cex = 0.8, title="", lty=1, lwd = 3, col = c("purple4", "plum"), y.intersp=0.8,
#        box.col = "white", inset=0.05, seg.len = 0.8, c("    albedo", "plus income/outgoing"), bg="white")
dev.off()
rm(albedo_bad, ohne.albedo)

}# Albedo


################ Radiation Netto #############################
if (zack == 1) {
net_zero <- which(db.bamet.lvl1$SwOut_fl == 0)
net_flags <- which(db.bamet.lvl1$SwOut_fl > 0)


png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_rad_net_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$SwOut, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-200, 720), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(-200, 600, 100), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$SwIn[last.von:last.bis] - db.last.year$SwOut[last.von:last.bis] +
         db.last.year$LwIn[last.von:last.bis] - db.last.year$LwOut[last.von:last.bis],
       pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast

points(as.numeric(strptime(db.bamet.lvl1$UTC[net_zero], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$SwIn[net_zero] - db.bamet.lvl1$SwOut[net_zero] +
         db.bamet.lvl1$LwIn[net_zero] - db.bamet.lvl1$LwOut[net_zero],
       pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "maroon4")

axis(2, at = seq(-200, 600, 100), labels = seq(-200, 600, 100), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, t.year, las = 2, cex = 6)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 350, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}

# legend("topleft", cex = 0.8, title="", lty=1, lwd = 3, col = c("mediumpurple3", "coral3"), y.intersp=0.8,
#        box.col = "white", inset=0.05, seg.len = 0.8, c("    in", "  out"), bg="white")
dev.off()
}# Radiation netto

################ Radiation Global #######
if (zack == 1) {
png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_rad_gl_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$SwIn, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-5, 720), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(0, 600, 100), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$SwIn[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast

points(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$SwIn, pch = 20, cex.lab = 1.5, cex.axis = 1.7,
       col = "lightsalmon4")
axis(2, at = seq(0, 600, 100), labels = seq(0, 600, 100), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
    labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, t.year, las = 2, cex = 6)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 350, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}

# legend("topleft", cex = 0.8, title="", lty=1, lwd = 3, col = c("mediumpurple3", "coral3"), y.intersp=0.8,
#        box.col = "white", inset=0.05, seg.len = 0.8, c("    in", "  out"), bg="white")
dev.off()
}# Radiation global

################ Radiation shortwave #######

if (zack == 1) {

sw_out <- which(db.bamet.lvl1$SwOut_fl == 0)
sw_in <- which(db.bamet.lvl1$SwIn_fl == 0)
sw_bad_out <- which(db.bamet.lvl1$SwOut_fl > 0)
sw_bad_in  <- which(db.bamet.lvl1$SwIn_fl > 0)

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_rad_sw_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$SwOut, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-5, 720), xlab = "Date", ylab = "", #ylab = "[W / m]",
     xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(0, 600, 100), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$SwOut[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast
points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$SwIn[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast

points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_in], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$SwIn[sw_in], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "mediumpurple3")
points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_out], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$SwOut[sw_out], pch = 20, cex.lab = 1.5, col = "khaki3")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_bad_in], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$SwOut[sw_bad_in], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        col = "red")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sw_bad_out], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$SwIn[sw_bad_out], pch = 20, cex.lab = 1.5,
#        col = "red")
axis(2, at = seq(0, 600, 100), labels = seq(0, 600, 100), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, t.year, las = 2, cex = 6)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 350, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}

# legend("topleft", cex = 0.8, title="", lty=1, lwd = 3, col = c("mediumpurple3", "coral3"), y.intersp=0.8,
#        box.col = "white", inset=0.05, seg.len = 0.8, c("    in", "  out"), bg="white")
dev.off()
}# Radiation shortwave


################ Radiation longwave #######
if (zack == 1) {
lw_in  <- which(db.bamet.lvl1$LwOut_fl == 0)
lw_out <- which(db.bamet.lvl1$LwIn_fl == 0)
lw_bad_in <- which(db.bamet.lvl1$LwOut_fl > 0)
lw_bad_out <- which(db.bamet.lvl1$LwIn_fl > 0)

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_rad_lw_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$LwOut, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(100, 500), xlab = "Date", ylab = "", #ylab = "[W / m]",
     xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

for (ll in seq(100, 500, 100)) {abline(h = ll, col = "gray80")} # horizontal lines
for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-20, 720), col = "gray80")} # vertical lines
points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$LwOut[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast
points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$LwIn[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast

points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_in], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$LwIn[lw_in], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "mediumpurple3")
points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_out], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$LwOut[lw_out], pch = 20, cex.lab = 1.5, col = "khaki3")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_bad_in], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$LwOut[lw_bad_in], pch = 20, cex.lab = 1.5,
#        col = "red")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[lw_bad_out], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$LwIn[lw_bad_out], pch = 20, cex.lab = 1.5,
#        col = "red")
axis(2, at = seq(100, 500, 100), labels = seq(100, 500, 100), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(500, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 450, t.year, las = 2, cex = 6)

# legend("topleft", cex = 0.8, title="", lty=1, lwd = 3, col = c("mediumpurple3", "coral3"), y.intersp=0.8,
#        box.col = "white", inset=0.05, seg.len = 0.8, c("    in", "  out"), bg="white")
dev.off()
}# Radiation longwave

################ Air temperature ########################################

if (zack == 1) {
#col2hex(soil.cols)#library(gplots)
rr <- length(aggregate(db.bamet.lvl1$Tair_200 ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
murr <- matrix(ncol = 4, nrow = rr, 1)
murr[, 1] <- aggregate(db.bamet.lvl1$Tair_200 ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
murr[, 2] <- aggregate(db.bamet.lvl1$Tair_200 ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]
ff <- length(aggregate(db.bamet.lvl1$Tair_100 ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
muff <- matrix(ncol = 4, nrow = ff, 1)
muff[, 1] <- aggregate(db.bamet.lvl1$Tair_100 ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
muff[, 2] <- aggregate(db.bamet.lvl1$Tair_100 ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]


air_zero <- which(as.numeric(db.bamet.lvl1$Tair_200_fl) == 0)
air_flags <- which(as.numeric(db.bamet.lvl1$Tair_200_fl) > 0)
air_zero1 <- which(as.numeric(db.bamet.lvl1$Tair_100_fl) == 0)
air_flags1 <- which(as.numeric(db.bamet.lvl1$Tair_100_fl) > 0)

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_Tair200_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$Tair_200, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-30, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(-30, 30, 10), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$Tair_200[last.von:last.bis], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
       xlim = xxlim, ylim = c(-5, 5), col = "snow3")             # hum last year

points(as.numeric(strptime(db.bamet.lvl1$UTC[air_zero], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Tair_200[air_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightgoldenrod3")
points(as.numeric(strptime(murr[, 1], format = "%Y-%m-%d")) + 43200,
       murr[, 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "darkorange3")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[air_flags], format = "%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$Tair_200[air_flags], pch = 20, cex.lab = 1.5,       col = "red")
axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, t.year, las = 2, cex = 6)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}

dev.off()

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_Tair100_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$Tair_100, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-30, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(-30, 30, 10), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$Tair_100[last.von:last.bis], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
       xlim = xxlim, ylim = c(-5, 5), col = "snow3")             # hum last year

points(as.numeric(strptime(db.bamet.lvl1$UTC[air_zero1], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Tair_100[air_zero1], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightgoldenrod3")
points(as.numeric(strptime(muff[, 1], format = "%Y-%m-%d")) + 43200,
       muff[, 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "darkorange3")
axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, t.year, las = 2, cex = 6)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}

dev.off() #;rm(air_zero, air_flags, rr, murr)

}# Air temperature (1 & 2)


################ Snowheight ########################################

if (zack == 5) {

pr_zero   <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec > 0)
pr_zero_zero   <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec == 0)
sh_zero   <- which(as.numeric(db.bamet.lvl1$snowh_fl) == 0)
pr_flags   <- which(as.numeric(db.bamet.lvl1$prec_fl) > 0)
sh_flags   <- which(as.numeric(db.bamet.lvl1$snowh_fl) > 0)

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_sh_", t.year, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$snowh, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(0, 2.3), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)
for (ll in seq(0, 1.5, 0.250)) {abline(h = ll, col = "gray80")} # horizontal lines
for (ll in seq(1.7, 2.2, 0.1)) {abline(h = ll, col = "gray80")} # horizontal lines
for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 1.5), col = "gray80")} # vertical lines
for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(1.7, 2.3), col = "gray80")} # vertical lines

points(as.numeric(strptime(db.bamet.lvl1$UTC[pr_zero_zero], format = "%Y-%m-%d %H:%M")), 2.2-(db.bamet.lvl1$prec[pr_zero_zero]/100),
       pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "gray80")
for (qq in pr_zero) {
  lines(c(as.numeric(strptime(db.bamet.lvl1$UTC[qq], format = "%Y-%m-%d %H:%M")),
          as.numeric(strptime(db.bamet.lvl1$UTC[qq], format = "%Y-%m-%d %H:%M"))),
        c(2.2, 2.2-(db.bamet.lvl1$prec[qq]/100)), lwd = 2, cex.lab = 1.5, cex.axis = 1.7, col = "darkblue")}

points(as.numeric(strptime(db.bamet.lvl1$UTC[pr_flags], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$prec[pr_flags], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
       col = "red")
points(as.numeric(strptime(db.bamet.lvl1$UTC[sh_zero], format = "%Y-%m-%d %H:%M")), 1.45-db.bamet.lvl1$snowh[sh_zero], pch = 20, cex.lab = 1.5,
       col = "green3")
points(as.numeric(strptime(db.bamet.lvl1$UTC[sh_flags], format = "%Y-%m-%d %H:%M")), 1.45-db.bamet.lvl1$snowh[sh_flags], pch = 20, cex.lab = 1.5,
       col = "red")
axis(2, at = seq(0, 1.5, 0.25), labels = seq(0, 1.5, 0.25), las = 2, cex.axis = 4)
axis(2, at = seq(1.7, 2.2, 0.1), labels = rev(seq(0, 100, 20)), las = 2, cex.axis = 4)
#mtext("snowheihgt[m]                        precipitation[mm]", side=2, cex = 2)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(2.3, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, t.year, las = 2, cex = 6)
if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 1, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}

# legend("center", cex = 0.8, title="", lty=1, lwd = 3, col = c("darkblue", "green3", "red"), y.intersp=0.8,
#        box.col = "white", inset=0.05, seg.len = 0.8, c("precipitation", "snowheight", "flagged"), bg="white")
dev.off() # ;rm(pr_zero, pr_zero_zero, sh_zero, pr_flags, sh_flags)
}# Snow depth (off)
################ Precipitation ########################################

# NEW plot (with monthly and annual sums)
if (zack == 1) {
  # only precipitation values with flag zero
  prec.zero <- db.bamet.lvl1$prec
  prec.zero[which(as.numeric(db.bamet.lvl1$prec_fl) > 0)] <- NA
  # only precipitation values with flags other than zero
  #prec.flags <- db.bamet.lvl1$prec
  #prec.flags[which(as.numeric(db.bamet.lvl1$prec_fl) == 0)] <- NA

  prec.daily <- aggregate(prec.zero ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = sum)
  prec.monthly <- aggregate(prec.zero ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m"), FUN = sum)

  png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_prec_", t.year, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)
  par(mfrow = c(3, 1), mar = c(0, 8, 0, 0), oma = c(1, 0, 1, 1))

  ###
  # hourly sums
  # plot all precipitation values (including the ones with flags other than zero)
  plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$prec, t = "h",
       xlim = xxlim, xlab = "", ylab = "", xaxt = "n", ylim = c(0, 20), cex.axis = 4, lwd = 3, col = "red")
  # overplot with flag zero precipitation values
  points(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), prec.zero, t = "h", lwd = 3, col = "steelblue4")
  # plot maintenance
  plot_maintenance(t.year)
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
  # horizontal lines
  abline(h = seq(0, 15, 5), lty = 1, col = "gray80")
  # add labels and axis
  mtext(text = Months, side = 3, line = -5, at = as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, cex = 4)
  mtext(text = "hourly sum", side = 3, line = -10, cex = 4, col = "steelblue4")
  mtext(text = "flags > 0", side = 3, line = -10, cex = 4, col = "red", adj = 0.98)
  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = FALSE, tcl = 0.5, cex.axis = 4)

  ###
  # daily sums
  plot(as.numeric(strptime(prec.daily[, 1], format = "%Y-%m-%d")), prec.daily[, 2], t = "h",
       xlim = xxlim, xlab = "", ylab = "", xaxt = "n", cex.axis = 4, lwd = 4, col = "steelblue3", ylim = c(0, max(prec.daily[, 2])))
  plot_maintenance(t.year)
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
  # horizontal lines
  grid(nx = NA, ny = NULL, lty = 1)

  # add labels
  mtext(text = "daily sum (only flag 0)", side = 3, line = -5, cex = 4, col = "steelblue3")

  ###.....................................................................
  # monthly sums
  # number of days in run.year ==> see: https://r.789695.n4.nabble.com/Count-days-of-current-year-td875319.html
  # days.run.year <- as.numeric(format(as.Date(paste(run.year, "12", "31", sep = "-")), "%j"))
  # last.day.month <- as.numeric(as.yearmon(prec.monthly[, 1]) + 1 / 12) - 1 / days.run.year
  # get last day of the month of the time vector of the aggregation of the monthly sums
  # ==> convert Year-Month format from aggregation results to POSIXct, add one month (1 / 12) and substract 86400 seconds for one day
  last.day.month <- as.POSIXct(as.yearmon(strptime(paste(prec.monthly[, 1], "-01 00:00", sep = ""), format = "%Y-%m-%d %H:%M")) + 1 / 12) - 86400

  #plot(as.numeric(strptime(prec.monthly[, 1], format = "%Y-%m-%d")), prec.monthly[, 2], t = "h",
  plot(last.day.month, prec.monthly[, 2], t = "h",
       xlim = xxlim, xlab = "", ylab = "", xaxt = "n", cex.axis = 4, lwd = 7, col = "steelblue1", ylim = c(0, max(prec.monthly[, 2])))
  plot_maintenance(t.year)
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
  # horizontal lines
  grid(nx = NA, ny = NULL, lty = 1)

  # add labels
  mtext(text = "monthly sum (only flag 0)", side = 3, line = -5, cex = 4, col = "steelblue1")

  ###.....................................................................
  # total sum of complete year
  mtext(text = c(t.year, paste("total (only flag 0): ", round(sum(prec.zero, na.rm = TRUE), 0), sep = "")),
        side = 3, line = c(-5, -12), cex = 4, adj = 0.98, col = c("black", "turquoise4"))

  dev.off()


###.....................................................................
# OLD plot

  if (t.year > 2010) {
  pr <- length(aggregate(db.bamet.lvl1$prec ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
  precr <- matrix(ncol = 4, nrow = pr, 1)
  precr[, 1] <- aggregate(db.bamet.lvl1$prec ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
  precr[, 2] <- aggregate(db.bamet.lvl1$prec ~ format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = sum)[, 2]
  pr2 <- length(aggregate(db.last.year$prec ~ format(strptime(db.last.year$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
  if (t.year == 2017) {
  precr2 <- matrix(ncol = 4, nrow = pr2, 1)
  precr2[, 1] <- aggregate(db.last.year$prec ~ format(strptime(db.last.year$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
  precr2[, 2] <- aggregate(db.last.year$prec ~ format(strptime(db.last.year$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = sum)[, 2]
  precr2[1:pr, 2] <- NA
  }
  pr_zero <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec > 0)
  pr_zero_zero <- which(as.numeric(db.bamet.lvl1$prec_fl) == 0 & db.bamet.lvl1$prec == 0)
  pr_flags <- which(as.numeric(db.bamet.lvl1$prec_fl) > 0)

  png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_OLD_PREC_", t.year, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
  plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$prec, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
       xlim = xxlim, ylim = c(0, 2.3), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
  plot_maintenance(t.year)

  # horizontal lines
  for (ll in seq(0, 1.5, 0.250)) {abline(h = ll, col = "gray80")}
  for (ll in seq(1.7, 2.2, 0.1)) {abline(h = ll, col = "gray80")}
  # vertical lines
  abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

  text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")), 2, "hourly", las = 2, srt = 20, cex = 5, col = "gray80")
  text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")), 1, "daily sum", las = 2, srt = 20, cex = 5, col = "gray80")

  # for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 1.5), col = "gray80")} # vertical lines
  # for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(1.7, 2.3), col = "gray80")} # vertical lines

  if (t.year == 2017) { # prec last year
    points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
         2.2 - (db.last.year$prec[last.von:last.bis] / 40),
         pch = 20, cex.lab = 1.5, cex.axis = 1.7,
         xlim = xxlim, ylim = c(-5, 5), col = "snow3")  }

    points(as.numeric(strptime(db.bamet.lvl1$UTC[pr_zero_zero], format = "%Y-%m-%d %H:%M")),
         2.2 - (db.bamet.lvl1$prec[pr_zero_zero] / 40),
         pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "gray80")
  for (qq in pr_zero) {# real values in upper part
    lines(c(as.numeric(strptime(db.bamet.lvl1$UTC[qq], format = "%Y-%m-%d %H:%M")),
            as.numeric(strptime(db.bamet.lvl1$UTC[qq], format = "%Y-%m-%d %H:%M"))),
          c(2.2, 2.2 - (db.bamet.lvl1$prec[qq] / 40)), lwd = 2, cex.lab = 1.5, cex.axis = 1.7, col = "turquoise4")}
  if (t.year == 2017) {
  for (ee in 1:length(precr2[, 1])) {# mean/sum values in bottom part (last year/forecast)
    lines(c(as.numeric(strptime(precr2[ee, 1], format = "%Y-%m-%d") + (365*60*60*24)) + 43200,
            as.numeric(strptime(precr2[ee, 1], format = "%Y-%m-%d") + (365*60*60*24)) + 43200),
          c(1.5, (1.5 - as.numeric(precr2[ee, 2]) / 20)), lwd = 3, cex.lab = 1.5, cex.axis = 1.7, col = "snow3")}}

  for (dd in 1:length(precr[, 1])) {# mean/sum values in bottom part
  lines(c(as.numeric(strptime(precr[dd, 1], format = "%Y-%m-%d")) + 43200,
          as.numeric(strptime(precr[dd, 1], format = "%Y-%m-%d")) + 43200),
        c(1.5, (1.5 - as.numeric(precr[dd, 2]) / 20)), lwd = 3, cex.lab = 1.5, cex.axis = 1.7, col = "steelblue4")}

  axis(2, at = seq(0, 1.5, 0.25) , labels = rev(seq(0, 30, 5)), las = 2, cex.axis = 4)
  axis(2, at = seq(1.7, 2.2, 0.1), labels = rev(seq(0, 20, 4)), las = 2, cex.axis = 4)

  axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
       labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
  text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(2.3, 12), labels = Months, las = 2, cex = 4)
  text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, t.year, las = 2, cex = 6)
  dev.off()  ;rm(pr_zero, pr_zero_zero, pr_flags)
  }
}# Precipitation
################ Windspeed and direction ########################################

### windspeed and direction
if (zack == 1) {

 w_v_zero <- which(as.numeric(db.bamet.lvl1$wind_v_200_fl) == 0 )
 w_d_zero <- which(as.numeric(db.bamet.lvl1$wind_deg_200_fl) == 0)
 w_v_flags <- which(as.numeric(db.bamet.lvl1$wind_v_200_fl) > 0)
 w_d_flags <- which(as.numeric(db.bamet.lvl1$wind_deg_200_fl) > 0)
 day <- paste0(substr(as.character(db.bamet.lvl1$UTC), 6, 7), substr(as.character(db.bamet.lvl1$UTC), 9, 10))

 wind.mean <- aggregate(db.bamet.lvl1$wind_v_200 ~ day, FUN = mean, na.action = NULL)
 day.mitte <- db.bamet.lvl1$UTC[substr(as.character(db.bamet.lvl1$UTC), 12, 16) == "12:00"]
 weg <- data.frame(day.mitte, wind.mean)
 weg <- weg[complete.cases(weg[]), ]

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_wind_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$wind_v_200, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(0, 32), xlab = "Date", ylab = "", #ylab = "speed[m/s] direction[deg]",
     xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
for (ll in seq(0, 24, 2)) {abline(h = ll, col = "gray80")}
for (ll in seq(26, 32, 1)) {abline(h = ll, col = "gray80")}
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
# for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 24.3), col = "gray80")} # vertical lines
# for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(25.7, 32), col = "gray80")} # vertical lines

points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       (db.last.year$wind_deg_200[last.von:last.bis] / 60) + 26, pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast

points(as.numeric(strptime(db.bamet.lvl1$UTC[w_d_zero], format = "%Y-%m-%d %H:%M")),
       (db.bamet.lvl1$wind_deg_200[w_d_zero] / 60) + 26,
       pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "darkblue")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[w_d_flags], format = "%Y-%m-%d %H:%M")), (db.bamet.lvl1$wind_deg_200[w_d_flags]/60) + 26, pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        col = "red")  # flags
points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$wind_v_200[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast

points(as.numeric(strptime(db.bamet.lvl1$UTC[w_v_zero], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$wind_v_200[w_v_zero], pch = 20, cex.lab = 1.5, col = "green3")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[w_v_flags], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$wind_v_300[w_v_flags], pch = 20, cex.lab = 1.5,
#        col = "red")  # flags
points(as.numeric(strptime(weg$day.mitte, format = "%Y-%m-%d %H:%M")),
       weg$db.bamet.lvl1.wind_v_200, col = "darkgreen", pch = 20, cex = 1.5)
axis(2, at = seq(0, 24, 2), labels = seq(0, 24, 2), las = 2, cex.axis = 4)
axis(2, at = seq(26, 32, 1), labels = seq(0, 360, 60), las = 2, cex.axis = 4)
#mtext("snowheihgt[m]                        precipitation[mm]", side=2, cex = 2)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 22, t.year, las = 2, cex = 6)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 10, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}
# legend("center", cex = 0.8, title="", lty=1, lwd = 3, col = c("darkblue", "green3", "red"), y.intersp=0.8,
#        box.col = "white", inset=0.05, seg.len = 0.8, c("direction", "speed", "flagged"), bg="white")
dev.off()  #; rm(w_v_zero, w_d_zero, w_v_flags, w_d_flags, day, wind.mean, day.mitte)

#  windspeed and direction (2) windrose



db.wind <- db.bamet.lvl1[, c("UTC", "wind_v_200", "wind_deg_200")]#1, 8, 9
db.wind <- db.wind[complete.cases(db.wind), ]

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_wrose_", t.year, ".png", sep = ""),
    width = p.width * 0.8, height = p.width * 0.8, pointsize = 8)
par(mar = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
wind.rose(wind.freq(db.wind$wind_v_200, db.wind$wind_deg_200), key = F, 6, 4, ang = (-3) * pi / 16, main = "", text.cex = 6)#paste(t.year)
legend("topright", cex = 6, title = "", lty = 1, lwd = 3, col = c("transparent"), y.intersp = 0.8,
       box.col = "white", inset = 0.05, seg.len = 0.8, c(paste(t.year)), bg = "transparent")
dev.off()

####
png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_wrose_monthly_", t.year, ".png", sep = ""),
    width = p.width * 0.8, height = p.width * 0.8, pointsize = 8)
par(mar = c(1, 1, 1, 1), omi = c(0, 0, 0, 0), mfrow = c(3, 4))
for (monz in 1:12) {
  db.windity <- db.wind[which(format(as.Date(db.wind$UTC), format = "%m") == months[monz]), ]
  wind.rose.3(wind.freq(db.windity$wind_v_200, db.windity$wind_deg_200), key = F, 6, 4, ang = (-3) * pi / 16, main = "", text.cex = 4)
  if (monz %in% c(1:4)) {
    legend("top", cex = 3, title = "", lty = 1, lwd = 3,
           col = c("transparent"), y.intersp = 0.8,
           box.col = "white", inset = 0.05, seg.len = 0.8,
           "----------------------------------------------------------------------------------------------------------",
           bg = "transparent")
  }
}
dev.off()

}# Wind
################ Humidity ########################################

if (zack == 1) {

hr <- length(aggregate(db.bamet.lvl1$RH_200~format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
humr <- matrix(ncol = 4, nrow = hr, 1)
humr[, 1] <- aggregate(db.bamet.lvl1$RH_200~format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
humr[, 2] <- aggregate(db.bamet.lvl1$RH_200~format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]

hum_zero   <- which(as.numeric(db.bamet.lvl1$RH_200_fl) == 0)
hum_flags   <- which(as.numeric(db.bamet.lvl1$RH_200_fl) > 0)

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_hum_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$RH_200, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(0, 105), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(0, 100, 10), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
       db.last.year$RH_200[last.von:last.bis], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
       xlim = xxlim, ylim = c(-5, 5), col = "snow3")             # hum last year

points(as.numeric(strptime(db.bamet.lvl1$UTC[hum_zero], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$RH_200[hum_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "aquamarine3")
points(as.numeric(strptime(humr[, 1], format = "%Y-%m-%d")) + 43200,
       humr[, 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "aquamarine4")

# points(as.numeric(strptime(db.bamet.lvl1$UTC[hum_flags], format = "%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$RH_200[hum_flags], pch = 20, cex.lab = 1.5,       col = "red")
axis(2, at = seq(0, 100, 10), labels = seq(0, 100, 10), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
#if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 50, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(103, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 7, t.year, las = 2, cex = 6)
dev.off() ;rm(hum_zero, hum_flags)
}# Humidity
################ Soil temperature ########################################

if (zack == 1) {
soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(150)
png(file = paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_Ts_203_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$Ts_203_2, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
     xlim = xxlim, ylim = c(-15, 18), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(-15, 15, 5), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

right <- c(2, 5, 23, 53, 93, 143)
for (qq in 24:19) {
  points(as.numeric(strptime(db.bamet.lvl1[(db.bamet.lvl1[, qq * 2 + 1] == 0), 1], format = "%Y-%m-%d %H:%M")),
  db.bamet.lvl1[(db.bamet.lvl1[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[right[qq - 18]])
}
axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
    labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, t.year, las = 2, cex = 6)
dev.off()#close pdf

png(file = paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_Ts_253_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$Ts_203_2, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   #
     xlim = xxlim, ylim = c(-15, 18), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(-15, 15, 5), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

left <- c(2, 12, 32, 62, 102, 150)
for (qq in 18:13) {
  points(as.numeric(strptime(db.bamet.lvl1[(db.bamet.lvl1[, qq * 2 + 1] == 0), 1], format = "%Y-%m-%d %H:%M")),
         db.bamet.lvl1[(db.bamet.lvl1[, qq*2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[left[qq - 12]]) }
axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
    labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, t.year, las = 2, cex = 6)
dev.off()#close pdf
# get colors for wikiplot
# soil.cols[left]
# soil.cols[right]
# colorRampPalette(c("steelblue4"))(1)
# for (i in seq(1, 150, by=2)) {cat(paste0("|@", soil.cols[i], ":-"))}
}# Soil temperature
################ Snow temperature ########################################
 
if (zack == 1) {
snowt05_zero <- which(as.numeric(db.bamet.lvl1$Tair_4_fl) == 0)
snowt20_zero <- which(as.numeric(db.bamet.lvl1$Tair_20_fl) == 0)
#snowt40_zero    <- which(as.numeric(db.bamet.lvl1$Tair_40_fl) == 0)
snowt05_flags <- which(as.numeric(db.bamet.lvl1$Tair_4_fl) > 0)
snowt20_flags <- which(as.numeric(db.bamet.lvl1$Tair_20_fl) > 0)
#snowt40_flags   <- which(as.numeric(db.bamet.lvl1$Tair_40_fl) > 0)

png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_Tsn_", t.year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")),
     db.bamet.lvl1$Tair_20, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-30, 25), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(t.year)

# horizontal lines
abline(h = seq(-30, 30, 10), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

# points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
#        db.last.year$Tair_20[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast
# points(as.numeric(strptime(db.last.year$UTC[last.von:last.bis], format = "%Y-%m-%d %H:%M")) + (366 * 60 * 60 * 24),
#        db.last.year$Tair_4[last.von:last.bis], pch = 20, cex.lab = 1.5, xlim = xxlim, ylim = c(-5, 5), col = "snow3")  # forecast

# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt40_zero], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$Tair_40[snowt40_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        col = "darkorchid4")
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt20_zero], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Tair_20[snowt20_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "olivedrab3")
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt05_zero], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Tair_4[snowt05_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "sienna2")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt40_flags], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$Tair_40[snowt40_flags], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        col = "orchid1")
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt20_flags], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Tair_20[snowt20_flags],
       pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "olivedrab4")
points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt05_flags], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Tair_4[snowt05_flags], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "sienna4")
axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, t.year, las = 2, cex = 6)

dev.off()#


}# Snow temperature

###.....................................................................
if (zack == 0) {

# # diffplot
# png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_snowt_diff1_", t.year, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
# par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
# plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$Tair_100, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
#      xlim = xxlim, ylim = c(-5, 5), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
# plot_maintenance(t.year)
# for (ll in seq(-5, 5, 1)) {abline(h = ll, col = "gray80")} # horizontal lines
# for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-5, 5), col = "gray80")} # vertical lines
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero], format = "%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$Tair_200[snowt_zero]-db.bamet.lvl1$Tair_20[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        col = "lightgoldenrod3")
#
# # colorRampPalette(c("aquamarine2", "lightgoldenrod3", "sienna2", "olivedrab3"))(4)
#
# axis(2, at = seq(-5, 5, 1), labels = seq(-5, 5, 1), las = 2, cex.axis = 4)
# axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
# text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(5, 12), labels = Months, las = 2, cex = 4)
# text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -4, t.year, las = 2, cex = 6)
# if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}
#
# dev.off()# ;rm(air_zero, air_flags, rr, murr)
# png(paste(p.1$w[p.1$n == "plot.p"], t.year, "/BaMet2009_snowt_diff2_", t.year, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
# par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
# plot(as.numeric(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$Tair_100, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
#      xlim = xxlim, ylim = c(-5, 5), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
# plot_maintenance(t.year)
# for (ll in seq(-5, 5, 1)) {abline(h = ll, col = "gray80")} # horizontal lines
# for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-5, 5), col = "gray80")} # vertical lines
# points(as.numeric(strptime(db.bamet.lvl1$UTC[snowt_zero], format = "%Y-%m-%d %H:%M")),
#        db.bamet.lvl1$Tair_200[snowt_zero]-db.bamet.lvl1$Tair_100[snowt_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        col = "aquamarine2")
#
# # colorRampPalette(c("aquamarine2", "lightgoldenrod3", "sienna2", "olivedrab3"))(4)
#
# axis(2, at = seq(-5, 5, 1), labels = seq(-5, 5, 1), las = 2, cex.axis = 4)
# axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
# text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(5, 12), labels = Months, las = 2, cex = 4)
# text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -4, t.year, las = 2, cex = 6)
# if (t.year == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 0, "reindeer", las = 2, srt=60, cex = 4, col = "gray80")}
#
# dev.off()# ;rm(air_zero, air_flags, rr, murr)


}# diffplot (off)

if (zack == 0) {


#summary(db.bamet.lvl1$prec)

#    # plot sw, albedo and snowheight (data with flag=0)
#    png(paste(plot.p.1, t.year, "/BaMet2009_RadAlbSnH_", t.year, ".png", sep = ""), width = 1400, height=1100)
#    par(mfrow = c(3, 1), mar = c(4.2, 4.5, 4.8, 2.1), omi = c(0.5, 0.5, 0.6, 0.2))
#    plot(strptime(db.bamet$UTC[sw_in], format = "%Y-%m-%d %H:%M"), db.bamet$SwOut[sw_in], pch = 20, cex.lab = 1.7, cex.axis = 1.7,
#         xlim = xxlim, ylim = ylim_sw, xlab = "Date", ylab = "[W / m2]", col = 155, main="SW Incoming and Outgoing", panel.first=grid(), cex.main=2)  # plot sw_in
#    points(strptime(db.bamet$UTC[sw_out], format = "%Y-%m-%d %H:%M"), db.bamet$SwIn[sw_out], pch = 20, cex.lab = 1.7, cex.axis = 1.7,
#           xlab = "Date", ylab = "[W / m2]", col = "mediumpurple3")                                                            # plot sw_out
#    plot_maintenance(t.year)
#    plot(strptime(db.bamet$UTC[-albedo_bad], format = "%Y-%m-%d %H:%M"), db.bamet$Albedo[-albedo_bad], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        xlim = xxlim, ylim = c(-1.2, 1.2), xlab = "Date", ylab = "[%]", panel.first=grid(), main="Outgoing/Incoming", cex.main=2)             # albedo = out/in
#    plot_maintenance(t.year)
#    plot(strptime(db.bamet$UTC[-sh_out], format = "%Y-%m-%d %H:%M"), db.bamet$snowh[-sh_out], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        xlim = xxlim, ylim = c(-0.2, 1.8), xlab = "Date", ylab = "[m]", panel.first=grid(), main="snowheight", cex.main=2)             # snowheight without bad data or noise
#    plot_maintenance(t.year)
#    title( paste( "Bayelva ", t.year, sep=""), line = 0, outer = TRUE, cex.main=3)
#    dev.off()


#    # all data
#    png(paste(plot.p.1, t.year, "/BaMet2009_RadAlbSnH_noFilter", t.year, ".png", sep = ""), width = 1400, height=1100)
#    par(mfrow = c(3, 1), mar = c(4.2, 4.5, 4.8, 2.1), omi = c(0.5, 0.5, 0.6, 0.2))
#    plot(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M"), db.bamet$SwOut, pch = 20, cex.lab = 1.7, cex.axis = 1.7,
#         xlim = xxlim, ylim = ylim_sw, xlab = "Date", ylab = "[W / m2]", col = 155, main="SW Incoming and Outgoing", panel.first=grid(), cex.main=2)  # plot sw_in
#    points(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M"), db.bamet$SwIn, pch = 20, cex.lab = 1.7, cex.axis = 1.7,
#           xlab = "Date", ylab = "[W / m2]", col = "mediumpurple3")                                                            # plot sw_out
#    plot_maintenance(t.year)
#    plot(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M"), db.bamet$Albedo, pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#         xlim = xxlim, ylim = c(-1.2, 1.2), xlab = "Date", ylab = "[%]", panel.first=grid(), main="Outgoing/Incoming", cex.main=2)             # albedo = out/in
#    plot_maintenance(t.year)
#    plot(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M"), db.bamet$snowh, pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#         xlim = xxlim, ylim = c(-0.2, 1.8), xlab = "Date", ylab = "[m]", panel.first=grid(), main="snowheight", cex.main=2)             # snowheight without bad data or noise
#    plot_maintenance(t.year)
#    title( paste( "Bayelva ", t.year, sep=""), line = 0, outer = TRUE, cex.main=3)
#    dev.off()
#
#
#    # plotting radiation stuff
#    png(paste(plot.p.1, t.year, "/BaMet2009_rad_outliers_", t.year, ".png", sep = ""), width = 1400, height=900)
#    par(mfrow = c(2, 1), mar = c(6.5, 4.5, 4, 2.1), omi = c(0.5, 0.5, 0.5, 0.2))     # global plotting settings
#
#    ## plot1 (original sw data  +  detected bad data in red)
#    plot(strptime(db.bamet$UTC[sw_in], format = "%Y-%m-%d %H:%M"), db.bamet$SwOut[sw_in], pch = 20, cex.lab = 1.7, cex.axis = 1.7,           # plot sw_in
#           xlim = xxlim, ylim = ylim_sw, xlab = "Date", ylab = "[W / m2]", col = 155, main="SW Incoming and Outgoing", panel.first=grid(), cex.main=2)
#    lines (strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M"), rep(-5, each = length(db.bamet$UTC)), col = "red")                                    # plot limit line
#    points(strptime(db.bamet$UTC[sw_out], format = "%Y-%m-%d %H:%M"), db.bamet$SwIn[sw_out], pch = 20, cex.lab = 1.7, cex.axis = 1.7,       # plot sw_out
#           xlab = "Date", ylab = "[W / m2]", col = "mediumpurple3")
#    plot_maintenance(t.year)
#    points(strptime(db.bamet$UTC[-sw_in], format = "%Y-%m-%d %H:%M"), db.bamet$SwOut[-sw_in], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red") # plot flag!=0
#    points(strptime(db.bamet$UTC[-sw_out], format = "%Y-%m-%d %H:%M"), db.bamet$SwIn[-sw_out], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red")# plot flag!=0
#    plot_maintenance(t.year)
#    par(xpd = TRUE)
#    legend("bottomright", inset = c(0, -0.39), pch = c(19, 19, 19), c("SwOut (In)", "SwIn (Out)", "bad flag"), col = c(155, "mediumpurple3", "red"), bty="n", cex = 1.5)
#    par(xpd=FALSE)
#
#    ## plot 2 (plot lw data  +  detected bad data in red)
#    plot(strptime(db.bamet$UTC[lw_in], format = "%Y-%m-%d %H:%M"), db.bamet$LwOut_cor[lw_in], pch = 20, cex.lab = 1.7, cex.axis = 1.7,   # plot lw_in
#           xlim = xxlim, ylim = ylim_lw, xlab = "Date", ylab = "[W / m2]", col = 155, main="LW Incoming and Outgoing", panel.first=grid(), cex.main=2)
#    lines (strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M"), rep(100, each = length(db.bamet$UTC)), col = "red")               # plot limit line
#    lines (strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M"), rep(500, each = length(db.bamet$UTC)), col = "red")               # plot limit line
#    points(strptime(db.bamet$UTC[lw_out], format = "%Y-%m-%d %H:%M"), db.bamet$LwIn_cor[lw_out], pch = 20, cex.lab = 1.7, cex.axis = 1.7,   # plot lw_out
#           xlab = "Date", ylab = "W / m2", col = "mediumpurple3")
#    points(strptime(db.bamet$UTC[-lw_in], format = "%Y-%m-%d %H:%M"), db.bamet$LwOut_cor[-lw_in], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red")  # plot flag!=0
#    points(strptime(db.bamet$UTC[-lw_out], format = "%Y-%m-%d %H:%M"), db.bamet$LwIn_cor[-lw_out], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red")  # plot flag!=0
#    plot_maintenance(t.year)
#    par(xpd = TRUE) # plot outside box
#    legend("bottomright", inset = c(0, -0.39), pch = c(19, 19, 19), c("LwOut_cor (In)", "LwIn_cor (Out)", "bad flag"), col = c(155, "mediumpurple3", "red"), bty="n", cex = 1.5)
#    par(xpd=FALSE)
#    title( paste( "Bayelva radiation for ", t.year, sep=""), line = 0, outer = TRUE, cex.main=3)
#    dev.off()
#
#
#
#    ## LEVEL 1 plot each month
#    # .....................................................................
#    for (m in months) {
#
#      # extract only monthly data
#      tmp<-(m == substr(db.bamet[, 1], 6, 7))
#      mm_data <- which(tmp= = TRUE)
#
#      # check if no radiation data at all exist for this month
#      if (    all(is.na(db.bamet$SwOut[mm_data]))= = TRUE   && all(is.na(db.bamet$SwIn[mm_data]))= = TRUE
#             && all(is.na(db.bamet$LwOut_cor[mm_data]))= = TRUE && all(is.na(db.bamet$LwIn_cor[mm_data]))= = TRUE ) {
#        #cat("\nNo Data for", m, t.year)
#        next
#      }
#
#      # plotting bounderies
#      ylim_sw <- plot_bounderies(db.bamet$SwOut[intersect(mm_data, sw_in)], db.bamet$SwIn[intersect(mm_data, sw_in)])      # get plotting bounderies shortwave
#      ylim_lw <- plot_bounderies(db.bamet$LwOut_cor[intersect(mm_data, sw_out)], db.bamet$LwIn_cor[intersect(mm_data, sw_out)])  # get plotting bounderies longwave
#      if ((m == "12")= = TRUE) { xxlim = c(as.numeric(strptime(paste0("01.", m, ".", t.year), format = "%d.%m.%Y")), as.numeric(strptime(paste0("31.", m, ".", t.year), format = "%d.%m.%Y")))
#      } else { xxlim = c(as.numeric(strptime(paste0("01.", m, ".", t.year), format = "%d.%m.%Y")), as.numeric(strptime(paste0("01.", months[as.numeric(m) + 1], ".", t.year), format = "%d.%m.%Y")))  }
#
#      # plot sw, albedo and snowheight
#      png(paste(lvl1.p.1, "plots/", t.year, "/Bayelva_RadAlbSnH_", m, t.year, ".png", sep = ""), width = 1400, height=1100)
#      par(mfrow = c(3, 1), mar = c(4.2, 4.5, 4.8, 2.1), omi = c(0.5, 0.5, 0.6, 0.2))
#      plot(strptime(db.bamet$UTC[intersect(mm_data, sw_in)], format = "%Y-%m-%d %H:%M"), db.bamet$SwOut[intersect(mm_data, sw_in)], pch = 20, cex.lab = 1.7, cex.axis = 1.7,
#           xlim = xxlim, ylim = ylim_sw, xlab = "Date", ylab = "[W / m2]", col = 155, main="SW Incoming and Outgoing", panel.first=grid(), cex.main=2)  # plot sw_in
#      points(strptime(db.bamet$UTC[intersect(mm_data, sw_out)], format = "%Y-%m-%d %H:%M"), db.bamet$SwIn[intersect(mm_data, sw_out)], pch = 20, cex.lab = 1.7, cex.axis = 1.7,
#             xlab = "Date", ylab = "[W / m2]", col = "mediumpurple3")                                                            # plot sw_out
#      plot_maintenance(t.year)
#      plot(strptime(db.bamet$UTC[-albedo_bad], format = "%Y-%m-%d %H:%M"), db.bamet$Albedo[-albedo_bad], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#           xlim = xxlim, ylim = c(-1.2, 1.2), xlab = "Date", ylab = "[%]", panel.first=grid(), main="Outgoing/Incoming", cex.main=2)             # albedo = out/in
#      plot_maintenance(t.year)
#      plot(strptime(db.bamet$UTC[-sh_out], format = "%Y-%m-%d %H:%M"), db.bamet$snowh[-sh_out], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#           xlim = xxlim, ylim = c(-0.2, 1.8), xlab = "Date", ylab = "[m]", panel.first=grid(), main="snowheight", cex.main=2)             # snowheight without bad data or noise
#      plot_maintenance(t.year)
#      title( paste( "Bayelva ", t.year, sep=""), line = 0, outer = TRUE, cex.main=3)
#      dev.off()
#
#
#
#      # plotting radiation  +  bad flag data
#     # .....................................................................
#      #cat("\nPlotting analysis", m, t.year)
#      png(paste(lvl1.p.1, "plots/", t.year, "/Bayelva_rad_outliers_", m, t.year, ".png", sep = ""), width = 1400, height=900)
#      par(mfrow = c(2, 1), mar = c(6.5, 4.5, 4, 2.1), omi = c(0.5, 0.5, 0.5, 0.2))     # global plotting settings
#
#      # plot 1 (original sw data  +  detected bad flags in red)
#      plot(strptime(db.bamet$UTC[intersect(mm_data, sw_in)], format = "%Y-%m-%d %H:%M"), db.bamet$SwOut[intersect(mm_data, sw_in)], pch = 20, cex.lab = 1.7, cex.axis = 1.7,    # plot sw_in
#            xlim = xxlim, ylim = ylim_sw, xlab = "Date", ylab = "[W / m2]", col = 155, main="SW Incoming and Outgoing", panel.first=grid(), cex.main=2)
#      lines (strptime(db.bamet$UTC[mm_data], format = "%Y-%m-%d %H:%M"), rep(-5, each = length(db.bamet$UTC[mm_data])), col = "red")                                                # plot limit line
#      points(strptime(db.bamet$UTC[intersect(mm_data, sw_out)], format = "%Y-%m-%d %H:%M"), db.bamet$SwIn[intersect(mm_data, sw_out)], pch = 20, cex.lab = 1.7, cex.axis = 1.7,  # plot sw_out
#             xlab = "Date", ylab = "W / m2", col = "mediumpurple3")
#      points(strptime(db.bamet$UTC[-sw_in], format = "%Y-%m-%d %H:%M"), db.bamet$SwOut[-sw_in], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red") # plot flag!=0
#      points(strptime(db.bamet$UTC[-sw_out], format = "%Y-%m-%d %H:%M"), db.bamet$SwIn[-sw_out], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red")# plot flag!=0
#      plot_maintenance(t.year)
#      par(xpd = TRUE)
#      legend("bottomright", inset = c(0, -0.39), pch = c(19, 19, 19), c("SwOut (In)", "SwIn (Out)", "bad flag"), col = c(155, "mediumpurple3", "red"), bty="n", cex = 1.5)
#      par(xpd=FALSE)
#
#
#      # plot 2 (plot lw data  +  detected bad data in red)
#      plot(strptime(db.bamet$UTC[intersect(mm_data, lw_in)], format = "%Y-%m-%d %H:%M"), db.bamet$LwOut_cor[intersect(mm_data, lw_in)], pch = 20, cex.lab = 1.7, cex.axis = 1.7,     # plot lw_in
#           xlim = xxlim, ylim = ylim_lw, xlab = "Date", ylab = "W / m2", col = 155, main="LW Incoming and Outgoing", panel.first=grid(), cex.main=2)
#      lines (strptime(db.bamet$UTC[mm_data], format = "%Y-%m-%d %H:%M"), rep(100, each = length(db.bamet$UTC[mm_data])), col = "red")                                                 # plot limit line
#      lines (strptime(db.bamet$UTC[mm_data], format = "%Y-%m-%d %H:%M"), rep(500, each = length(db.bamet$UTC[mm_data])), col = "red")                                                 # plot limit line
#      points(strptime(db.bamet$UTC[intersect(mm_data, lw_out)], format = "%Y-%m-%d %H:%M"), db.bamet$LwIn_cor[intersect(mm_data, lw_out)], pch = 20, cex.lab = 1.7, cex.axis = 1.7, # plot lw_out
#             xlab = "Date", ylab = "W / m2", col = "mediumpurple3")
#      points(strptime(db.bamet$UTC[-lw_in], format = "%Y-%m-%d %H:%M"), db.bamet$LwOut_cor[-lw_in], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red")  # plot flag!=0
#      points(strptime(db.bamet$UTC[-lw_out], format = "%Y-%m-%d %H:%M"), db.bamet$LwIn_cor[-lw_out], pch = 20, cex.lab = 1.7, cex.axis = 1.7, col = "red")  # plot flag!=0
#      plot_maintenance(t.year)
#      par(xpd = TRUE) # plot outside box
#      legend("bottomright", inset = c(0, -0.39), pch = c(19, 19, 19), c("LwOut_cor (In)", "LwIn_cor (Out)", "bad flag"), col = c(155, "mediumpurple3", "red"), bty="n", cex = 1.5)
#      par(xpd=FALSE)
#      title( paste( "Bayelva radiation for ", m, "-", t.year, sep=""), line = 0, outer = TRUE, cex.main=3)
#      dev.off()
#
#
#
#    } # end loop over months
#
#
}# monthly plots Kerstin (off)
###.....................................................................
cat("#\n# level1 BaMet2009 ", t.year, " plot done!\n#\n")

}





###.....................................................................
###.....................................................................
# update every monday the overview plot of the complete soil temperature series of sensor Ts_252 and sensor Ts_203 of BaMet2009
# adapted from LV1_plots_BaSoil2009_temp.R

if (weekdays(Sys.Date()) == "Montag" | weekdays(Sys.Date()) == "Monday") { # update every Monday
  start.date <- as.POSIXct(paste0("01.08.2009"), format = '%d.%m.%Y', tz = "UTC")
  end.date <- as.POSIXct(paste0(format(Sys.Date(), "%d.%m.%Y")), format = '%d.%m.%Y', tz = "UTC")

  # define data frame
  db.ba2 <- as.data.frame(matrix(ncol = 13, nrow = length(seq(start.date, end.date, by = "30 min")), NA))# soil
  db.ba2[, 1] <- as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d', tz = "UTC")

  colnames(db.ba2) <- c("UTC", "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62", "Ts_252_102",
                        "Ts_252_152", "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53", "Ts_203_93", "Ts_203_143")

  #loscht <- c(db.ba2$UTC[format(strptime(db.ba2$UTC, format = "%Y-%m-%d %H:%M"), format = "%m-%d %H:%M") == "01-01 00:00"], db.ba2$UTC[length(db.ba2$UTC)])

  # define time vector
  time.vec <- c(db.ba2$UTC[format(strptime(db.ba2$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
              db.ba2$UTC[length(db.ba2$UTC)])
  # format time column as numeric values
  db.ba2[, 1] <- as.numeric(db.ba2[, 1])

  # loop to combine all years since 2009
  for (kk in 2009:recent.year) {
    didi <- read.table(paste0(p.1$w[p.1$n == "LV1.p"], "BaMet2009/00_full_dataset/BaMet2009_", kk, "_lv1_noflag.dat"),
                       sep = ",", dec = ".", header = T, na.strings = "NA")
    didi[, 1] <- as.numeric(as.POSIXct(didi[, 1], format = '%Y-%m-%d %H:%M', tz = "UTC"))

	# select columns
	didi <- didi[, c("UTC", "Ts_252_2", "Ts_252_12", "Ts_252_32", "Ts_252_62", "Ts_252_102",
	                 "Ts_252_152", "Ts_203_2", "Ts_203_5", "Ts_203_23", "Ts_203_53", "Ts_203_93", "Ts_203_143")]
    # select row indices of the same dates:
	ind1 <- which(db.ba2$UTC %in% didi$UTC)
	ind2 <- which(didi$UTC %in% db.ba2$UTC)
	# check:
	all.equal(db.ba2[ind1, 1], didi[ind2, 1])
	# store data from year kk in db.ba2
	db.ba2[ind1, -1] <- didi[ind2, -1]
  }


  ###.....................................................................
  # plot sensor Ts_252
  png(file = paste(p.1$w[p.1$n == "plot.p"], "Longterm/BaMet2009_Ts_252_Longterm.png", sep = ""),
      width = 3600, height = 500, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  plot(db.ba2$UTC,
       db.ba2$Ts_252_2, pch = 20,
       ylim = range(db.ba2[, 2:7], na.rm = TRUE), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")

  # define grid
  # horizontal lines
  abline(h = seq(-25, 25, 5), col = "gray80")
  # vertical lines
  abline(v = as.numeric(seq(as.POSIXct("2009-01-01 01:00:00"), as.POSIXct(paste(as.numeric(recent.year) + 1, "-01-01 01:00:00", sep = "")), by = "year")), col = "gray80")

  depth <- c(2, 12, 32, 62, 102, 150)
  #  == > ATTENTION
  # it should be: c(2, 12, 32, 62, 102, 152), because of:
  # the depths: c(0.025, 	0.123, 0.328, 0.623, 1.023, 1.523)
  # but there are only 150 colours  == > see definition of soil.cols in the beginning of the script

  for (qq in 1:6) {
    points(db.ba2[, 1], db.ba2[, qq + 1], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
  }

  axis(2, at = seq(-25, 25, 5), labels = seq(-25, 25, 5), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(time.vec, format = "%Y-%m-%d %H:%M"))),
      labels = rep("", length(as.numeric(strptime(time.vec, format = "%Y-%m-%d %H:%M")))), las = 2, tcl = 0.5, cex.axis = 4)
  ##
  # labeling of years
  text(as.numeric(seq(as.POSIXct("2009-07-01 01:00:00"), as.POSIXct(paste(as.numeric(recent.year), "-07-01 01:00:00", sep = "")), by = "year")),
       -14, c(2009:recent.year), las = 2, cex = 4)

  dev.off()#close png


  ###.....................................................................
  # plot sensor Ts_203
  png(file = paste(p.1$w[p.1$n == "plot.p"], "Longterm/BaMet2009_Ts_203_Longterm.png", sep = ""),
      width = 3600, height = 500, pointsize = 8)
  par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))

  plot(db.ba2$UTC,
       db.ba2$Ts_203_2, pch = 20,
       ylim = range(db.ba2[, 8:13], na.rm = TRUE), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")

  # define grid
  # horizontal lines
  abline(h = seq(-25, 25, 5), col = "gray80")
  # vertical lines
  abline(v = as.numeric(seq(as.POSIXct("2009-01-01 01:00:00"), as.POSIXct(paste(as.numeric(recent.year) + 1, "-01-01 01:00:00", sep = "")), by = "year")), col = "gray80")

  depth <- c(2, 5, 23, 53, 93, 143)

  for (qq in 1:6) {
    points(db.ba2[, 1], db.ba2[, qq + 7], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[depth[qq]])
  }

  axis(2, at = seq(-25, 25, 5), labels = seq(-25, 25, 5), las = 2, cex.axis = 4)
  axis(3, at = c(as.numeric(strptime(time.vec, format = "%Y-%m-%d %H:%M"))),
      labels = rep("", length(as.numeric(strptime(time.vec, format = "%Y-%m-%d %H:%M")))),
      las = 2, tcl = 0.5, cex.axis = 4)
  ##
  # labeling of years
  text(as.numeric(seq(as.POSIXct("2009-07-01 01:00:00"), as.POSIXct(paste(as.numeric(recent.year), "-07-01 01:00:00", sep = "")), by = "year")),
       -9, c(2009:recent.year), las = 2, cex = 4)

  dev.off()#close png

}



