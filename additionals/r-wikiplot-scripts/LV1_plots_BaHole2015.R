###.........................................................................
##
##   BaHole2015         Level1 wikiplots ----
##
##   no extremes,
##
##   by: Stephan.Lange@awi.de
##   modified: ---
##   2021-05-12 SL adapt to runnerapp and content management
##   2015-05-22 SL create this plot script
##
##
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

options(scipen = 100, stringsAsFactors = F, scientific = T) # for non-exponential display of numeric values # digits = 2,
origin <- "1970-01-01"
recent.year <- as.numeric(format(Sys.Date(),"%Y"))
first.year <- 2015

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(90)
mon.cols <- c("slategray2", "skyblue1", "lightgreen", "olivedrab2", "olivedrab4", "orangered", "red", "red4", "darkgoldenrod1", "tan4", "gray40", "steelblue4")

#col2rgb(soil.cols[diefe])
#p.width = 420;p.height = 280
#p.width = 420*1.5;p.height = 280*1.5
p.width <- 420 * 3.5
p.height <- 280 * 3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

###.........................................................................
# to run this script separately, you have to set run.year:
# run.year <- first.year:recent.year
#
###.........................................................................


for (year_i in run.year) {
  # load data  ----
  db.bahole <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "BaHole2015/00_full_dataset/BaHole2015_", year_i, "_lv1_noflag.dat", sep = ""),
                          sep = ",", dec = ".", header = T, fill = TRUE)
  db.bahole.lvl1 <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "BaHole2015/00_full_dataset/BaHole2015_", year_i, "_lv1.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
  db.bahole <- db.bahole[, -c(6)]
  db.bahole.lvl1 <- db.bahole.lvl1[, -c(10, 11)]

xxlim <- c(as.numeric(strptime(paste0("13.01.", year_i), format = "%d.%m.%Y")),
           as.numeric(strptime(paste0("20.12.", year_i), format = "%d.%m.%Y")))

lischt <- c(db.bahole.lvl1$UTC[format(strptime(db.bahole.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
            db.bahole.lvl1$UTC[length(db.bahole.lvl1$UTC)])

db.bahole.lvl2 <- db.bahole.lvl1#[, -c(10, 11)]
for (val in 1:9) {# set data to NA if flag is not 0
  db.bahole.lvl2[which(as.numeric(db.bahole.lvl2[, (val * 2) + 1]) >= 1), (val * 2)] <- NA
}
db.bahole.extra <- db.bahole.lvl2[, c(1, c(1:9) * 2)]
db.bahole.extra$monate <- format(as.Date(db.bahole.extra[, 1]), format = "%m")
stats.db <- aggregate(db.bahole.extra[, 2:10], by = list(db.bahole.extra$monate), FUN = mean, na.rm = TRUE)[2:10]
y.values <- c(0, -.5, -1, -1.5, -2.5, -3.5, -5.5, -7.5, -9)

# plot trompete ----
png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/BaHole2015_trompete_", year_i, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(c(stats.db[1, ]), y.values, type = "n", xlim = c(-16, 8), ylim = c(-9, 2),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex.axis = 3)

# define grid
# horizontal lines
abline(h = seq(-10, 0, 1), col = "gray80")
# vertical lines
abline(v = seq(-15, 15, 2.5), col = "gray80")

for (qqq in 1:12) {
  lines(c(stats.db[qqq, 1:9]), y.values[1:9], col = mon.cols[qqq], lwd = 2)
}

text(seq(-15, 15, 2.5), rep(1.5, 13), labels = seq(-15, 15, 2.5), las = 2, cex = 4)
axis(2, at = seq(-10, 0, 1), labels = seq(-10, 0, 1), las = 2, cex.axis = 4)
#legend(10, -10, 1:12, col = mon.cols, lty = 1, cex = .5, lwd = 4)
text(5, -8, year_i, las = 2, cex = 6)
dev.off()

#  plot soil temperature ----
###.........................................................................
tair_gut <- which(db.bahole.lvl1$Tair_50_fl == 0)


png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/BaHole2015_Ts_", year_i, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 6, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bahole.lvl1$UTC[tair_gut], format = "%Y-%m-%d %H:%M")),
     db.bahole.lvl1$Tair_50[tair_gut], pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-25, 15), xlab = "Date", ylab = "",#ylab = "[K]",
     xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(year_i)


# define grid
# horizontal lines
abline(h = seq(-30, 30, 10), col = "gray80")
# vertical lines
abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

diefe <- c(1, 5, 10, 15, 25, 35, 55, 75, 90)

for (qq in 1:9) {
  points(as.numeric(strptime(db.bahole.lvl1[((db.bahole.lvl1[, qq * 2 - 1]) == 0), 1], format = "%Y-%m-%d %H:%M")),
         db.bahole.lvl1[((db.bahole.lvl1[, qq * 2 - 1]) == 0), qq * 2], col = soil.cols[diefe[qq]], pch = 20)
  #points(as.numeric(strptime(db.bahole.lvl1[((db.bahole.lvl1[, qq*2+1])> = 1), 1], format = "%Y-%m-%d %H:%M")), db.bahole.lvl1[((db.bahole.lvl1[, qq*2+1])> = 1), qq*2], col = "red", pch = ".")
  #cat(db.bahole.lvl1[1, qq*2], "\n")
}
axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 9, year_i, las = 2, cex = 6)

dev.off()

cat("#\n# level1 BaHole2015", year_i, " plot done!\n#\n")

}
