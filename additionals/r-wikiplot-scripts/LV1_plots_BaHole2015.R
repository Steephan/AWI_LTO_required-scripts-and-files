#############################################################################
##
##   BaHole2015         Level1
##
##   no extremes,
##
##   by: Stephan.Lange@awi.de
##   modified: 2015/05/22
##
##
#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
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

########
# to run this script separately, you have to set run.year:
# run.year <- first.year:recent.year
#
#######


for (year in run.year) {

  db.bahole <- read.table(paste(path$w[path$n == "LV1.p"], "BaHole2015/00_full_dataset/BaHole2015_", year, "_lv1_noflag.dat", sep = ""),
                          sep = ",", dec = ".", header = T, fill = TRUE)
  db.bahole.lvl1 <- read.table(paste(path$w[path$n == "LV1.p"], "BaHole2015/00_full_dataset/BaHole2015_", year, "_lv1.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
  db.bahole <- db.bahole[, -c(6)]
  db.bahole.lvl1 <- db.bahole.lvl1[, -c(10, 11)]
## yearly LEVEL 0 plots
#   =========================================================================================================

xxlim <- c(as.numeric(strptime(paste0("13.01.", year), format = "%d.%m.%Y")),
           as.numeric(strptime(paste0("20.12.", year), format = "%d.%m.%Y")))

lischt <- c(db.bahole.lvl1$UTC[format(strptime(db.bahole.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
            db.bahole.lvl1$UTC[length(db.bahole.lvl1$UTC)])

# if (year =  = 2015) {
#   db.ba2009     <-read.table(paste(path$w[path$n =  = "LV1.p"], "BaHole2009/00_full_dataset/BaHole2009_", year, "_lv1_noflag.dat", sep = ""), sep = ", ", dec = ".", header = T, fill = TRUE)
#   db.ba2009.lvl1<-read.table(paste(path$w[path$n =  = "LV1.p"], "BaHole2009/00_full_dataset/BaHole2009_", year, "_lv1.dat", sep = ""), sep = ", ", dec = ".", header = T, fill = TRUE)
#   db.bahole[1:5850, 1:10]<-  db.ba2009[1:5850, c(1, 3:11)]
#   db.bahole.lvl1[1:5850, 1:19]<-  db.ba2009.lvl1[1:5850, c(1, 4:21)]
# }

db.bahole.lvl2 <- db.bahole.lvl1#[, -c(10, 11)]
for (val in 1:9) {# set data to NA if flag is not 0
  db.bahole.lvl2[which(as.numeric(db.bahole.lvl2[, (val * 2) + 1]) >= 1), (val * 2)] <- NA
}
db.bahole.extra <- db.bahole.lvl2[, c(1, c(1:9) * 2)]
db.bahole.extra$monate <- format(as.Date(db.bahole.extra[, 1]), format = "%m")
stats.db <- aggregate(db.bahole.extra[, 2:10], by = list(db.bahole.extra$monate), FUN = mean, na.rm = TRUE)[2:10]
y.values <- c(0, -.5, -1, -1.5, -2.5, -3.5, -5.5, -7.5, -9)


png(paste(path$w[path$n == "plot.p"], year, "/BaHole2015_trompete_", year, ".png", sep = ""),
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
text(5, -8, year, las = 2, cex = 6)
dev.off()

#  soil temperature
# -----------------------------------------------------
tair_gut <- which(db.bahole.lvl1$Tair_50_fl == 0)


png(paste(path$w[path$n == "plot.p"], year, "/BaHole2015_moustache_", year, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 6, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.bahole.lvl1$UTC[tair_gut], format = "%Y-%m-%d %H:%M")),
     db.bahole.lvl1$Tair_50[tair_gut], pch = 20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
     xlim = xxlim, ylim = c(-25, 15), xlab = "Date", ylab = "",#ylab = "[K]",
     xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(year)


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
#
# points(as.numeric(strptime(db.bahole.lvl1$UTC[air_zero], format = "%Y-%m-%d %H:%M")), db.bamet.lvl1$Tair_200[air_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7,
#        col = "paleturquoise")
#
# points(as.numeric(strptime(db.bahole.lvl1$UTC[air_flags], format = "%Y-%m-%d %H:%M")), db.bahole.lvl1$Tair_200[air_flags], pch = 20, cex.lab = 1.5,
#        col = "red")
axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 9, year, las = 2, cex = 6)

dev.off()

cat("#\n# level1 Mr. Moustache ", year, " plot done!\n#\n")

}
