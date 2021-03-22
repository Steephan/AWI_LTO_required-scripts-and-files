#############################################################################
##
##   snow water equivalent (SWE) plotter       WIKI Level1
##   snow water equivalent (SWE) product       see Level2
##
##
##   by: Stephan.Lange@awi.de
##       christian.lehr@awi.de
##   last modified: 2019-10-14
##
##
#############################################################################

############################################################################
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
#############################################################################

options(scipen = 100, stringsAsFactors = F, digits = 2, scientific = T) # for non-exponential display of numeric values
origin = "1970-01-01"

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


p.width <- 420 * 3.5
p.height <- 280 * 3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

########
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# run.year <- c(2019:2020) # c(1998:2019)
# run.year <- first.year:recent.year
#######


for (jahr in run.year) {
  start.date <- as.POSIXct(paste0("01.01.",jahr," 00:30"), format = '%d.%m.%Y %H:%M', tz = "UTC")
  end.date <- as.POSIXct(paste0("31.12.",jahr," 18:30"), format = '%d.%m.%Y %H:%M', tz = "UTC")
  db.product_1 <- db.product  <- as.data.frame(matrix(ncol = 3, nrow = length(seq(start.date, end.date, by = "6 hours"))))
  db.complete_1 <- db.complete <- as.data.frame(matrix(ncol = 5, nrow = length(seq(start.date, end.date, by = "6 hours"))))

if (jahr >= 2019) {
  db.basnow.cs.lvl1 <- read.table(paste0(paste0(p.1$w[p.1$n == "LV1.p"]),
                                         "BaSnow2019cs/00_full_dataset/BaSnow2019cs_", jahr, "_lv1.dat"),
                              sep = ",", dec = ".", header = T, fill = TRUE)[, c("UTC", "SWE_K", "SWE_K_fl", "SWE_Tl", "SWE_Tl_fl")]
  db.complete_1[, 2] <- db.basnow.cs.lvl1[, "SWE_K"]
  db.complete_1[, 3] <- db.basnow.cs.lvl1[, "SWE_K_fl"]
  db.complete_1[, 4] <- db.basnow.cs.lvl1[, "SWE_Tl"]
  db.complete_1[, 5] <- db.basnow.cs.lvl1[, "SWE_Tl_fl"]
}

xxlim = c(as.numeric(strptime(paste0("13.01.", jahr), format = "%d.%m.%Y")),
          as.numeric(strptime(paste0("20.12.", jahr), format = "%d.%m.%Y")))
db.product_1[, 1] <- as.numeric(as.POSIXct(seq(start.date,end.date, by = "6 hours"),format = '%Y-%m-%d %H:%M'))
db.product_1[, 1] <- format(as.POSIXct(as.numeric(db.product_1[, 1]), origin = "1970-01-01", tz = "UTC"), format = '%Y-%m-%d %H:%M')

colnames(db.product_1) <- c("UTC","SWE","SWE_fl")

# -----------------
# some boundaries
lischt <- c(db.product_1$UTC[format(strptime(db.product_1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:30"],
                         db.product_1$UTC[length(db.product_1$UTC)])

db.product_1[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "6 hours"), format = '%Y-%m-%d %H:%M'))

# #  snow water equivalent (SWE)
# # -----------------------------------------------------
if (jahr >= 2019) {
  swe_k_basnow.cs_zero <- which(as.numeric(db.basnow.cs.lvl1$SWE_K_fl) == 0)
  swe_k_basnow.cs_flags <- which(as.numeric(db.basnow.cs.lvl1$SWE_K_fl) > 0)
  swe_tl_basnow.cs_zero <- which(as.numeric(db.basnow.cs.lvl1$SWE_Tl_fl) == 0)
  swe_tl_basnow.cs_flags <- which(as.numeric(db.basnow.cs.lvl1$SWE_Tl_fl) > 0)
  }

# set min, max values and the interval for the y-axis of the plots
ymin <- 0
ymax <- 600
yint <- 50


png(paste(p.1$w[p.1$n == "plot.p"], jahr,"/BaSnow2019cs_swe_", jahr, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.product_1$UTC, format = "%Y-%m-%d %H:%M")), db.product_1$SWE, pch = 20,
      xlim = xxlim, ylim = c(ymin,ymax), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(jahr)

for (ll in seq(ymin, ymax, yint)) {abline(h = ll, col = "gray80")} # horizontal lines
for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-0.1, 1.82), col = "gray80")} # vertical lines

## CS725 BaSnow2019cs
if (jahr >= 2019) {
  points(as.numeric(strptime(db.basnow.cs.lvl1$UTC[swe_k_basnow.cs_zero], format = "%Y-%m-%d %H:%M")),
         db.basnow.cs.lvl1$SWE_K[swe_k_basnow.cs_zero], pch = 20, cex.lab = 1.5, col = "blue")

	points(as.numeric(strptime(db.basnow.cs.lvl1$UTC[swe_tl_basnow.cs_zero], format = "%Y-%m-%d %H:%M")),
         db.basnow.cs.lvl1$SWE_Tl[swe_tl_basnow.cs_zero], pch = 20, cex.lab = 1.5, col = "red")
}


axis(2, at = seq(ymin, ymax, yint), labels = seq(ymin, ymax, yint), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("","","","","","","","","","",""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep((ymax - ymin) * 0.95, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, jahr, las = 2, cex = 6)
if (jahr == 2012) {
  text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 1,
       "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")
}

dev.off()

#####################
#### db.product #####
db.product[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "6 hours"), format = '%Y-%m-%d %H:%M'))
db.product <- as.data.frame(db.product)
colnames(db.product) <- c("UTC", "SWE", "SWE_fl")

# set conditions
if (jahr >= 2019) {
   db.product[, 2] <- db.basnow.cs.lvl1$SWE_K
   db.product[, 3] <- db.basnow.cs.lvl1$SWE_K_fl
 } #else if(jahr==2013){
#   ...
# }

db.product <- as.data.frame(db.product)
colnames(db.product) <- c("UTC", "SWE", "SWE_fl")
db.product[, 1] <- format(as.POSIXct(as.numeric(db.product[, 1]), origin = "1970-01-01", tz = "UTC"), format = '%Y-%m-%d %H:%M')
sh_product_zero <- which(as.numeric(db.product$SWE_fl) == 0)
sh_product_flags <- which(as.numeric(db.product$SWE_fl) > 0)


png(paste(p.1$w[p.1$n == "plot.p"], jahr, "/BaSnow2019cs_product_", jahr, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.product$UTC, format = "%Y-%m-%d %H:%M")), db.product$SWE, pch = 20,
    xlim = xxlim, ylim = c(ymin, ymax), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(jahr)

# horizontal lines
for (ll in seq(ymin, ymax, yint)) {
  abline(h = ll, col = "gray80")
}
# vertical lines
for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
  lines(c(pp, pp), c(-0.1, 1.82), col = "gray80")
}
  lines(as.numeric(strptime(db.product$UTC[sh_product_zero], format = "%Y-%m-%d %H:%M")),
         db.product$SWE[sh_product_zero], pch = 20, cex.lab = 1.5, col = "snow4")      # type="o"  ,
  #   points(as.numeric(strptime(db.product$UTC[sh_product_flags],format="%Y-%m-%d %H:%M")),
  #          db.product$snowheight[sh_product_flags], pch = 20, cex.lab = 1.5,       col="red")
# if(jahr %in% c(2009,2012,2013)){
#  for(hui in 1:length(cuts)){
#    lines(c(as.numeric(strptime(db.product$UTC[cuts[hui]],format="%Y-%m-%d %H:%M")),
#            as.numeric(strptime(db.product$UTC[cuts[hui]],format="%Y-%m-%d %H:%M"))),c(0.1,1.5),lwd=2,col="maroon")
#  }}
axis(2, at = seq(ymin, ymax, yint), labels = seq(ymin, ymax, yint), las = 2,cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
     labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep((ymax - ymin) * 0.95, 12),
     labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, jahr, las = 2,cex = 6)

dev.off() #


#################
db.complete[, 1] <- format(as.POSIXct(seq(start.date,end.date, by = "6 hours"), format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
db.complete_1[, 1] <- format(as.POSIXct(seq(start.date,end.date, by = "6 hours"), format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
colnames(db.complete) <- c("UTC", "SWE_K", "SWE_K_fl", "SWE_Tl", "SWE_Tl_fl")
colnames(db.complete_1) <- c("UTC", "SWE_K", "SWE_K_fl", "SWE_Tl", "SWE_Tl_fl")

write.table(db.product, paste0(paste0(p.1$w[p.1$n == "LV2.p"]) ,"Bayelva/SWE/BaSnow_product_", jahr, ".dat"),
            quote = F, dec = ".", sep = "," , row.names = F)
write.table(db.complete_1, paste0(paste0(p.1$w[p.1$n == "LV2.p"]) ,"Bayelva/SWE/BaSnow_complete_", jahr, ".dat"),
            quote = F, dec = ".", sep = ",", row.names = F)

##################
cat("#\n# level1 BaSnow2019 ", jahr, " plot done!\n#\n")
}
