#############################################################################
##
##   Snow plotter       WIKI Level1
##   Snow product       see Level2
##
##   written by: Stephan.Lange@awi.de
##               christian.lehr@awi.de
##   last modified: 2019-10-10
##
##   last check: 2020-01-30
##   checked by: christian.lehr@awi.de
##
#############################################################################
##
## open issues:
##
##
##
#############################################################################
##
## last modification:
##
##
#############################################################################
##
## comments:
##
#############################################################################
# set path settings for different operating systems: linux vs. windoof
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

# for non-exponential display of numeric values
options(scipen = 100, stringsAsFactors = F, digits = 2, scientific = T)
origin <- "1970-01-01"

########
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# run.year <- 2009:2021 # aktuell # c(2019) # c(1998:2019)
#######

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# plot options
p.width <- 420 * 3.5
p.height <- 280 * 3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

# loop for annual plots
for (jahr in run.year) {
  start.date <- as.POSIXct(paste0("01.01.", jahr, " 00:00"), format = '%d.%m.%Y %H:%M', tz  =  "UTC")
  end.date <- as.POSIXct(paste0("31.12.", jahr, " 23:30"), format = '%d.%m.%Y %H:%M', tz  =  "UTC")
  db.product_1 <- db.product  <- as.data.frame(matrix(ncol = 3, nrow = length(seq(start.date, end.date, by = "30 min"))))
  db.complete_1 <- db.complete <- as.data.frame(matrix(ncol = 9, nrow = length(seq(start.date, end.date, by = "30 min"))))
  #if(jahr  ==  2012){raw.dist = 1.5}else{raw.dist = 1.45}
  ##################################################################
  ## cross check this again!!! why why why!
  ##################################################################
if (jahr >= 2019) {
  db.basnow.sr.lvl1 <- read.table(paste0(paste0(p.1$w[p.1$n == "LV1.p"]),
                                   "BaSnow2019sr/00_full_dataset/BaSnow2019sr_", jahr, "_lv1.dat"),
                                  sep = ",", dec = ".", header = T, fill = TRUE)[, c("UTC", "Dsn", "Dsn_fl")]
  db.complete_1[seq(1, 2 * length(db.basnow.sr.lvl1[, 1]), 2), 6] <- db.basnow.sr.lvl1[, "Dsn"]
  db.complete_1[seq(1, 2 * length(db.basnow.sr.lvl1[, 1]), 2), 7] <- db.basnow.sr.lvl1[, "Dsn_fl"]
}

if (jahr >= 2009) {
  db.bamet.lvl1 <- read.table(paste0(paste0(p.1$w[p.1$n == "LV1.p"]),
      "BaMet2009/00_full_dataset/BaMet2009_", jahr, "_lv1.dat"),
      sep = ",", dec = ".", header = T, fill  =  TRUE)[, c("UTC", "Dsn", "Dsn_fl")]
# db.complete_1[, 4]<- round(1.45-db.bamet.lvl1[, 4], 3)
# db.complete_1[, 5]<-      db.bamet.lvl1[, 2]
db.complete_1[, 4] <- db.bamet.lvl1[, 2]
db.complete_1[, 5] <- db.bamet.lvl1[, 3]
}

if (jahr >= 2013) {
  db.snow.lvl1 <- read.table(paste0(paste0(p.1$w[p.1$n == "LV1.p"]),
      "BaSnow2013/00_full_dataset/BaSnow2013_", jahr, "_lv1.dat"),
      sep = ",", dec = ".", header = T)
  db.complete_1[, 2:3] <- db.snow.lvl1[, 2:3]
}

if ((jahr >= 2007) && (jahr <= 2017)) {
  db.eddy.lvl1 <- read.table(paste0(paste0(p.1$w[p.1$n == "LV1.p"]),
     #"BaEddy2007/snowheight/stephans/BaEddy2007snowheight", jahr, ".dat"), sep = ",", dec = ".", header = T, na.strings = "NaN")
     "BaEddy2007/00_full_dataset/BaEddy2007_", jahr, "_lv1.dat"),
     sep = ",", dec = ".", header = T)
  db.complete_1[seq(1, 2 * length(db.eddy.lvl1[, 1]), 2), 6:7] <- db.eddy.lvl1[, 2:3]
}

if ((jahr >= 1998) && (jahr <= 2009)) {
  db.snow98.lvl1 <- read.table(paste(p.1$w[p.1$n == "LV1.p"],
    "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
    sep = ",", dec = ".", header = T, fill  =  TRUE)[, c("UTC", "Dsn", "Dsn_fl")]
  db.complete_1[seq(1, 2 * length(db.snow98.lvl1[, 1]), 2), 4:5] <- db.snow98.lvl1[, 2:3]
}

if (jahr == 2009) {
  db.complete_1[seq(1, 14000, 2), 4:5] <- db.snow98.lvl1[1:7000, 2:3]
  db.complete_1[14001:17520, 4] <- round(1.45 - db.bamet.lvl1[14001:17520, 2], 3)
  db.complete_1[14001:17520, 5] <- db.bamet.lvl1[14001:17520, 3]
}

xxlim <- c(as.numeric(strptime(paste0("13.01.", jahr), format = "%d.%m.%Y")),
           as.numeric(strptime(paste0("20.12.", jahr), format = "%d.%m.%Y")))
db.product_1[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M'))
db.product_1[, 1] <- format(as.POSIXct(as.numeric(db.product_1[, 1]), origin = "1970-01-01", tz = "UTC"), format = '%Y-%m-%d %H:%M')

colnames(db.product_1) <- c("UTC", "Dsn", "Dsn_fl")

# -----------------
# some boundaries
lischt <- c(db.product_1$UTC[format(strptime(db.product_1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
            db.product_1$UTC[length(db.product_1$UTC)])


db.product_1[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M'))


# #  snow depth
# # -----------------------------------------------------
if (jahr >= 2019) {
  sh_basnow.sr_zero <- which(as.numeric(db.basnow.sr.lvl1$Dsn_fl) == 0)
  sh_basnow.sr_flags <- which(as.numeric(db.basnow.sr.lvl1$Dsn_fl) > 0)
}

if (jahr >= 2013) {
  sh_shm_zero <- which(as.numeric(db.snow.lvl1$Dsn_fl) == 0)
  sh_shm_flags <- which(as.numeric(db.snow.lvl1$Dsn_fl) > 0)
}

if (jahr >= 2009) {
  sh_bamet_zero <- which(as.numeric(db.bamet.lvl1$Dsn_fl) == 0)
  sh_bamet_flags <- which(as.numeric(db.bamet.lvl1$Dsn_fl) > 0)
}

if ((jahr >= 2007) && (jahr <= 2017)) {
  sh_baeddy_zero <- which(as.numeric(db.eddy.lvl1$Dsn_fl) == 0)
  sh_baeddy_flags <- which(as.numeric(db.eddy.lvl1$Dsn_fl) > 0)
}

if ((jahr >= 1998) && (jahr <= 2009)) {
  sh_snow98_zero <- which(as.numeric(db.snow98.lvl1$Dsn_fl) == 0)
  sh_snow98_flags <- which(as.numeric(db.snow98.lvl1$Dsn_fl) > 0)
}

png(paste(p.1$w[p.1$n == "plot.p"], jahr, "/BaSnow_compare_", jahr, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.product_1$UTC, format = "%Y-%m-%d %H:%M")), db.product_1$Dsn, pch  =  20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
      xlim = xxlim, ylim = c(0, 1.8), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(jahr)

for (ll in seq(0, 1.8, 0.2)) {
  abline(h = ll, col = "gray80")
} # horizontal lines

for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
  lines(c(pp, pp), c(-0.1, 1.82), col = "gray80")
} # vertical lines

## SR50 BaSnow2019sr
if (jahr >= 2019) {
    points(as.numeric(strptime(db.basnow.sr.lvl1$UTC[sh_basnow.sr_zero], format = "%Y-%m-%d %H:%M")),
           db.basnow.sr.lvl1$Dsn[sh_basnow.sr_zero], pch = 20, cex.lab = 1.5, col = "blue")
}

## SHM-50 Laser
if (jahr >= 2013) {
points(as.numeric(strptime(db.snow.lvl1$UTC[sh_shm_zero], format = "%Y-%m-%d %H:%M")),
       db.snow.lvl1$Dsn[sh_shm_zero], pch = 20, cex.lab = 1.5, col = "mediumpurple4")
# points(as.numeric(strptime(db.snow.lvl1$UTC[sh_shm_flags], format = "%Y-%m-%d %H:%M")),
#        db.snow.lvl1$sh[sh_shm_flags], pch  =  20, cex.lab  =  1.5,       col = "gold3")
}

## SR50 BaMet2010
if (jahr >= 2009) {
points(as.numeric(strptime(db.bamet.lvl1$UTC[sh_bamet_zero], format = "%Y-%m-%d %H:%M")),
       db.bamet.lvl1$Dsn[sh_bamet_zero], pch = 20, cex.lab = 1.5, col = "darkolivegreen4")
# points(as.numeric(strptime(db.bamet.lvl1$UTC[sh_bamet_flags], format = "%Y-%m-%d %H:%M")),
#        1.45-db.bamet.lvl1$snowh[sh_bamet_flags], pch  =  20, cex.lab  =  1.5,       col = "indianred1")
}

## SR50 BaEddy2007
if ((jahr >= 2007) && (jahr <= 2017)) {
  points(as.numeric(strptime(db.eddy.lvl1$UTC[sh_baeddy_zero], format = "%Y-%m-%d %H:%M")),
         db.eddy.lvl1$Dsn[sh_baeddy_zero], pch = 20, cex.lab = 1.5, col = "chocolate4")
#   points(as.numeric(strptime(db.eddy.lvl1$UTC[sh_baeddy_flags], format = "%Y-%m-%d %H:%M")),
#          db.eddy.lvl1$snowheight[sh_baeddy_flags], pch  =  20, cex.lab  =  1.5,       col = "red")
}

if ((jahr >= 1998) && (jahr <= 2009)) {
  points(as.numeric(strptime(db.snow98.lvl1$UTC[sh_snow98_zero], format = "%Y-%m-%d %H:%M")),
         db.snow98.lvl1$Dsn[sh_snow98_zero], pch = 20, cex.lab = 2.5, col = "cyan4")
  #   points(as.numeric(strptime(db.eddy.lvl1$UTC[sh_snow98_flags], format = "%Y-%m-%d %H:%M")),
  #          db.eddy.lvl1$snowheight[sh_snow98_flags], pch  =  20, cex.lab  =  1.5,       col = "red")
}

#colorRampPalette(c("mediumpurple4", "darkolivegreen4", "chocolate4", "snow4"))(4)
axis(2, at = seq(0, 1.8, 0.2), labels = seq(0, 1.8, 0.2), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(1.7, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, jahr, las = 2, cex = 6)

if (jahr == 2012) {
  text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 1,
       "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")
}

dev.off() # ;rm(pr_zero, pr_zero_zero, sh_zero, pr_flags, sh_flags)


#### db.product #####
db.product[, 1] <- as.numeric(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M'))
db.product <- as.data.frame(db.product)
colnames(db.product) <- c("UTC", "Dsn", "Dsn_fl")
if (jahr >= 2014) {
  db.product[, 2] <- db.snow.lvl1$Dsn
  db.product[, 3] <- db.snow.lvl1$Dsn_fl
} else if (jahr == 2013) {
  cuts <- c(11300)

  db.eddy.lvl1[, 1] <- as.numeric(as.POSIXct(db.eddy.lvl1[, 1], format = '%Y-%m-%d %H:%M'))
  newdf.c <- merge(db.product, db.eddy.lvl1, all.x = T, by = "UTC")
  db.product[1:cuts[1], 2] <- newdf.c[1:cuts[1], 4]
  is.na(newdf.c[1:cuts[1], 5]) <- 1
  db.product[1:cuts[1], 3] <- newdf.c[1:cuts[1], 5]

  db.product[(cuts[1] + 1):17520, 2] <- db.snow.lvl1[(cuts[1] + 1):17520, 2]
  db.product[(cuts[1] + 1):17520, 3] <- db.snow.lvl1[(cuts[1] + 1):17520, 3]
} else if (jahr == 2012) {
  cuts <- c(6000, 12000)

  db.eddy.lvl1[, 1] <- as.numeric(as.POSIXct(db.eddy.lvl1[, 1], format = '%Y-%m-%d %H:%M'))
  newdf.c <- merge(db.product, db.eddy.lvl1, all.x = T, by = "UTC")
  db.product[c(1:cuts[1], (cuts[2] + 1):17568), 2] <- newdf.c[c(1:cuts[1], (cuts[2] + 1):17568), 4]
  is.na(newdf.c[c(1:cuts[1], (cuts[2] + 1):17568), 5]) <- 1
  db.product[c(1:cuts[1], (cuts[2] + 1):17568), 3] <- newdf.c[c(1:cuts[1], (cuts[2] + 1):17568), 5]

  db.bamet.lvl1[, 1] <- as.numeric(as.POSIXct(db.bamet.lvl1[, 1], format = '%Y-%m-%d %H:%M'))
  newdf.a <- merge(db.product, db.bamet.lvl1, all.x = T, by = "UTC")
  db.product[((cuts[1] + 1):cuts[2]), 2] <- newdf.a[((cuts[1] + 1):cuts[2]), 4]
  is.na(newdf.a[((cuts[1] + 1):cuts[2]), 5]) <- 1
  db.product[((cuts[1] + 1):cuts[2]), 3] <- newdf.a[((cuts[1] + 1):cuts[2]), 5]

} else if (jahr %in% c(2010, 2011)) {
  db.bamet.lvl1[, 1] <- as.numeric(as.POSIXct(db.bamet.lvl1[, 1], format = '%Y-%m-%d %H:%M', origin = "1970-01-01", tz = "UTC"))
  newdf.a <- merge(db.product, db.bamet.lvl1, all.x = T, by = "UTC")
  #db.product[, 2]<-raw.dist-newdf.a[, 8]+(mean(newdf.a[12000:13000, 8])-raw.dist)
  db.product[, 2] <- newdf.a[, 4]
  is.na(newdf.a[, 5]) <- 1
  db.product[, 3] <- newdf.a[, 5]

} else if (jahr == 2009) {
  cuts <- c(14000)
  db.snow98.lvl1[, 1] <- as.numeric(as.POSIXct(db.snow98.lvl1[, 1], format = '%Y-%m-%d %H:%M', origin = "1970-01-01", tz = "UTC"))
  newdf.a <- merge(db.product, db.snow98.lvl1, all.x = T, by = "UTC")
  db.product[1:cuts[1], 2] <- newdf.a[1:cuts[1], 4]
  is.na(newdf.a[1:cuts[1], 5]) <- 1
  db.product[1:cuts[1], 3] <- newdf.a[1:cuts[1], 5]

  db.bamet.lvl1[, 1] <- as.numeric(as.POSIXct(db.bamet.lvl1[, 1], format = '%Y-%m-%d %H:%M', origin = "1970-01-01", tz = "UTC"))
  newdf.a <- merge(db.product, db.bamet.lvl1, all.x = T, by = "UTC")
  db.product[cuts[1]:17520, 2] <- newdf.a[cuts[1]:17520, 4] # raw.dist minus newdf.a[cuts[1]:17520, 4]
  is.na(newdf.a[cuts[1]:17520, 5]) <- 1
  db.product[cuts[1]:17520, 3] <- newdf.a[cuts[1]:17520, 5]

} else if (jahr < 2009) {
  db.snow98.lvl1[, 1] <- as.numeric(as.POSIXct(db.snow98.lvl1[, 1], format = '%Y-%m-%d %H:%M', origin = "1970-01-01", tz = "UTC"))
  #db.snow98.lvl1[, 1]<-as.numeric(as.POSIXct(db.snow98.lvl1[, 1], format = '%Y-%m-%d %H:%M', origin = "1970-01-01"))
  newdf.a <- merge(db.product, db.snow98.lvl1, all.x = T, by = "UTC")
  db.product[, 2] <- newdf.a[, 4]
  is.na(newdf.a[, 5]) <- 1
  db.product[, 3] <- newdf.a[, 5]
}

db.product <- as.data.frame(db.product)
colnames(db.product) <- c("UTC", "Dsn", "Dsn_fl")
db.product[, 1] <- format(as.POSIXct(as.numeric(db.product[, 1]), origin = "1970-01-01", tz = "UTC"), format = '%Y-%m-%d %H:%M')
sh_product_zero <- which(as.numeric(db.product$Dsn_fl) == 0)
sh_product_flags <- which(as.numeric(db.product$Dsn_fl) > 0)

##
png(paste(p.1$w[p.1$n == "plot.p"], jahr, "/BaSnow_product_", jahr, ".png", sep = ""),
    width = p.width, height = p.height, pointsize = 8)
par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
plot(as.numeric(strptime(db.product$UTC, format = "%Y-%m-%d %H:%M")), db.product$Dsn, pch  =  20, # cex.lab = 1.7, cex.axis = 1.5,   # albedo from file
    xlim = xxlim, ylim = c(0, 1.8), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
plot_maintenance(jahr)

# horizontal lines
for (ll in seq(0, 1.8, 0.2)) {
  abline(h = ll, col = "gray80")
}

# vertical lines
for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
  lines(c(pp, pp), c(-0.1, 1.82), col = "gray80")
}

lines(as.numeric(strptime(db.product$UTC[sh_product_zero], format = "%Y-%m-%d %H:%M")),
      db.product$Dsn[sh_product_zero], pch = 20, cex.lab = 1.5, col = "snow4")      # type = "o"  ,
  #   points(as.numeric(strptime(db.product$UTC[sh_product_flags], format = "%Y-%m-%d %H:%M")),
  #          db.product$snowheight[sh_product_flags], pch  =  20, cex.lab  =  1.5,       col = "red")
if (jahr %in% c(2009, 2012, 2013)) {
  for (hui in 1:length(cuts)) {
    lines(c(as.numeric(strptime(db.product$UTC[cuts[hui]], format = "%Y-%m-%d %H:%M")),
            as.numeric(strptime(db.product$UTC[cuts[hui]], format = "%Y-%m-%d %H:%M"))), c(0.1, 1.5), lwd = 2, col = "maroon")
  }
}
axis(2, at = seq(0, 1.8, 0.2), labels = seq(0, 1.8, 0.2), las = 2, cex.axis = 4)
axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
    labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(1.7, 12), labels = Months, las = 2, cex = 4)
text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, jahr, las = 2, cex = 6)

dev.off()

#################
db.complete[, 1] <- format(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
db.complete_1[, 1] <- format(as.POSIXct(seq(start.date, end.date, by = "30 min"), format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
colnames(db.complete) <- c("UTC", "SHM_Dsn", "SHM_Dsn_fl", "SR50_a_Dsn", "SR50_a_Dsn_fl", "SR50_b_Dsn", "SR50_b_Dsn_fl", "SR50_c_Dsn", "SR50_c_Dsn_fl")
colnames(db.complete_1) <- c("UTC", "SHM_Dsn", "SHM_Dsn_fl", "SR50_a_Dsn", "SR50_a_Dsn_fl", "SR50_b_Dsn", "SR50_b_Dsn_fl", "SR50_c_Dsn", "SR50_c_Dsn_fl")

write.table(db.product, paste0(paste0(p.1$w[p.1$n == "LV2.p"]), "Bayelva/Snowheight/BaSnow_product_", jahr, ".dat"),
            quote = F, dec = ".", sep = ",", row.names = F)
write.table(db.complete_1, paste0(paste0(p.1$w[p.1$n == "LV2.p"]), "Bayelva/Snowheight/BaSnow_complete_", jahr, ".dat"),
            quote = F, dec = ".", sep = ",", row.names = F)

##################
cat("#\n# level1 BaSnow2013 ", jahr, " plot done!\n#\n")

}
