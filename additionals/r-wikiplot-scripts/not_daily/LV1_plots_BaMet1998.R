#############################################################################
##
##   Level1 plots to wiki
##   --------------------
##   BaMet1998
##
##   by: Stephan.Lange@awi.de -2200
##   updated by: christian.lehr@awi.de
##   last modified: 2020-04-15
##
#############################################################################
#
#  last modification:
#  2020-04-15 CL update format of script and plot labels analogue to BaMet2009
#
#############################################################################
# to run this script separately, you have to uncomment the next 10 lines!
rm(list = ls())
if (.Platform$OS.type == "windows") {
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

# for non-exponential display of numeric values
options(scipen = 100, stringsAsFactors = F, scientific = T) # digits = 2,
origin <- "1970-01-01"

zack <- 1

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

p.width <- 420 * 3.5
p.height <- 280 * 3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

########
# to run this script separately, you have to set run.year:
run.year <- 1998:2009
#
########


for (jahr in run.year) {
  if (zack == 1) {
    #   db.bamet <- read.table(paste(path$w[path$n == "BaMet.lv1.p"], "00_complete/BaMet2010_", jahr, "_noflag.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
    #   db.bamet.lvl1 <- read.table(paste(path$w[path$n == "BaMet.lv1.p"], "00_complete/BaMet2010_", jahr, ".dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
    db.bamet <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                           sep = ",", dec = ".", header = T, fill = TRUE)

    if (jahr >= 1998 && jahr <= 2009) {#
     # db.temp98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/01_air_temp/BaMet1998_temp_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
     # db.hum98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/04_humidity/BaMet1998_hum_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
     # db.wind98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/03_wind/BaMet1998_wind_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
     # db.prec98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/02_prec/BaMet1998_prec_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
     # db.rad98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/06_radiation/BaMet1998_rad_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
     # db.snowt98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/01_air_temp/BaMet1998_temp_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
     # db.soil98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/07_soil/BaMet1998_ts_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
     db.temp98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)[1:3]
     db.hum98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)[c(1, 6, 7)]
     db.wind98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)[c(1, 16:21)]
     db.prec98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)[c(1, 4, 5)]
     db.rad98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)[c(1, 10:15)]
     db.snowt98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                                sep = ",", dec = ".", header = T, fill = TRUE)[c("UTC", "Tair_5", "Tair_5_fl", "Tair_20", "Tair_20_fl",
                                                                                 "Tair_35", "Tair_35_fl", "Tair_48", "Tair_48_fl", "Tair_100", "Tair_100_fl")]
     db.soil98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)[c("UTC", "Ts_252_2", "Ts_252_2_fl", "Ts_252_12",
                                                                               "Ts_252_12_fl", "Ts_252_32", "Ts_252_32_fl", "Ts_252_62", "Ts_252_62_fl",
                                                                               "Ts_252_102", "Ts_252_102_fl", "Ts_252_152", "Ts_252_152_fl", "Ts_203_2",
                                                                               "Ts_203_2_fl", "Ts_203_5", "Ts_203_5_fl", "Ts_203_23", "Ts_203_23_fl",
                                                                               "Ts_203_53", "Ts_203_53_fl", "Ts_203_93", "Ts_203_93_fl", "Ts_203_143",
                                                                               "Ts_203_143_fl")]
  } else if (jahr >= 1999 && jahr <= 2010) {#
      #db.prec98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/02_prec/BaMet1998_prec_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
      # db.snowt98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/08_snow_temp/BaMet1998_snowt_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
      db.prec98 <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/00_full_dataset/BaMet1998_", jahr, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)[c(1, 4, 5)]

    } else {
      cat(" no data for ", jahr, "\n")
      next
    }

    if (jahr == 2009) {#
      # db.snowt98_a <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/08_snow_temp/BaMet1998_snowt_", jahr, ".dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
      db.met <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet2009/00_full_dataset/BaMet2009_", jahr, "_lv1.dat", sep = ""),
                           sep = ",", dec = ".", header = T, fill = TRUE)
    }
    xxlim = c(as.numeric(strptime(paste0("13.01.", jahr), format = "%d.%m.%Y")), as.numeric(strptime(paste0("20.12.", jahr), format = "%d.%m.%Y")))
    lischt <- c(db.prec98$UTC[format(strptime(db.prec98$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
                db.prec98$UTC[length(db.prec98$UTC)])
  }# Load data

  if (zack == 0) {

  }# Albedo (off)

  if (zack == 1) {
    #  radiation Netto
    # -----------------------------------------------------
    net_zero <- which(db.bamet$RadNet_fl == 0)
    net_flags <- which(db.bamet$RadNet_fl > 0)

    png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_rad_net_", jahr, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M")), db.bamet$RadNet, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(-200, 720), xlab = " ", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    plot_maintenance(jahr)

    # horizontal lines
    abline(h = seq(-200, 600, 100), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

    points(as.numeric(strptime(db.bamet$UTC[net_zero], format = "%Y-%m-%d %H:%M")), db.bamet$RadNet[net_zero],
           pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "maroon4")
    if (jahr == 2009) {#
      points(as.numeric(strptime(db.met$UTC, format = "%Y-%m-%d %H:%M")),
             db.met$SwIn - db.met$SwOut + db.met$LwIn - db.met$LwOut, #SwOut-SwIn + LwOut-LwIn
             pch = 20, cex.lab = 1.5, cex.axis = 1.7,
             col = "maroon4")
    }
    axis(2, at = seq(-200, 600, 100), labels = seq(-200, 600, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, jahr, las = 2, cex = 6)
    #if (jahr == 2012) {text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 350, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")}

    # legend("topleft", cex = 0.8, title = "", lty = 1, lwd = 3, col = c("mediumpurple3", "coral3"), y.intersp = 0.8,
    #        box.col = "white", inset = 0.05, seg.len = 0.8, c("    in", "  out"), bg = "white")
    dev.off()
  }# Radiation netto

  if (zack == 1) {
    #  radiation global
    # -----------------------------------------------------
    gl_zero <- which(db.bamet$SwIn_fl == 0)
    gl_flags <- which(db.bamet$SwIn_fl > 0)

    png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_rad_gl_", jahr, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M")), db.bamet$SwIn, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
         xlim = xxlim, ylim = c(-5, 720), xlab = " ", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
    plot_maintenance(jahr)

    # horizontal lines
    abline(h = seq(0, 600, 100), col = "gray80")
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")

    points(as.numeric(strptime(db.bamet$UTC[gl_zero], format = "%Y-%m-%d %H:%M")), db.bamet$SwIn[gl_zero],
           pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightsalmon4")
    if (jahr == 2009) {#
      points(as.numeric(strptime(db.met$UTC, format = "%Y-%m-%d %H:%M")), db.met$SwIn,
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightsalmon4")
    }
    axis(2, at = seq(0, 600, 100), labels = seq(0, 600, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, jahr, las = 2, cex = 6)

    dev.off()
  }# radiation global

  if (zack == 1) {
    # #  air temperature
    # # -----------------------------------------------------
    # #
    if (jahr >= 1998 && jahr <= 2009) {#
      db.temp98 <- db.bamet[, c( "UTC", "Tair_100", "Tair_100_fl", "Tair_200", "Tair_200_fl")]
      if (jahr == 2009) {#

        db.temp98_a <- db.bamet[, c( "UTC", "Tair_100", "Tair_100_fl", "Tair_200", "Tair_200_fl")]
        db.temp98_b <- read.table(paste0(path$w[path$n == "LV1.p"] , "BaMet2009/00_full_dataset/BaMet2009_", jahr, "_lv1.dat"),
                                     sep = ",", dec = ".", header = T, fill = TRUE)[seq(1, 17520, by = 2), c( "UTC", "Tair_100", "Tair_100_fl", "Tair_200", "Tair_200_fl")]
        db.temp98[7285:8760, 2:5] <- db.temp98_b[7285:8760, 2:5]

      }

      air_zero <- which(as.numeric(db.temp98$Tair_200_fl) == 0)
      air_flags <- which(as.numeric(db.temp98$Tair_200_fl) > 0)
      air100_zero <- which(as.numeric(db.temp98$Tair_100_fl) == 0)
      air100_flags <- which(as.numeric(db.temp98$Tair_100_fl) > 0)
      if (length(air_zero) > 0) {
        rr <- length(aggregate(db.temp98$Tair_200[air_zero]~format(strptime(db.temp98$UTC[air_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
        murr <- matrix(ncol = 4, nrow = rr, 1)
        murr[, 1] <- aggregate(db.temp98$Tair_200[air_zero]~format(strptime(db.temp98$UTC[air_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
        murr[, 2] <- aggregate(db.temp98$Tair_200[air_zero]~format(strptime(db.temp98$UTC[air_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]
        # murr[, 3] <- aggregate(db.bamet.lvl1$Tair_200~format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = min)[, 2]
        # murr[, 4] <- aggregate(db.bamet.lvl1$Tair_200~format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = max)[, 2]

        png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_Tair200_", jahr, ".png", sep = ""),
            width = p.width, height = p.height, pointsize = 8)
        par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
        plot(as.numeric(strptime(db.temp98$UTC, format = "%Y-%m-%d %H:%M")), db.temp98$Tair_200, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
             xlim = xxlim, ylim = c(-30, 25), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
        plot_maintenance(jahr)

        for (ll in seq(-30, 30, 10)) {abline(h = ll, col = "gray80")} # horizontal lines
        for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-30, 30), col = "gray80")} # vertical lines

        points(as.numeric(strptime(db.temp98$UTC[air_zero], format = "%Y-%m-%d %H:%M")), db.temp98$Tair_200[air_zero],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightgoldenrod3")
        points(as.numeric(strptime(murr[, 1], format = "%Y-%m-%d")) + 43200, murr[, 2],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "darkorange3")
        # points(as.numeric(strptime(db.temp98$UTC[air_flags], format = "%Y-%m-%d %H:%M")),
        #        db.temp98$Tair_200[air_flags], pch = 20, cex.lab = 1.5,     col = "red")
        axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
        axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
             labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
        text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
        text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, jahr, las = 2, cex = 6)

        dev.off()
      } else {
        cat(" no air200 temp data for", jahr, "\n")
      }
      if (length(air100_zero) > 0) {
        rr <- length(aggregate(db.temp98$Tair_100[air100_zero]~format(strptime(db.temp98$UTC[air100_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
        murr <- matrix(ncol = 4, nrow = rr, 1)
        murr[, 1] <- aggregate(db.temp98$Tair_100[air100_zero]~format(strptime(db.temp98$UTC[air100_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
        murr[, 2] <- aggregate(db.temp98$Tair_100[air100_zero]~format(strptime(db.temp98$UTC[air100_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]
        # murr[, 3] <- aggregate(db.bamet.lvl1$Tair_200~format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = min)[, 2]
        # murr[, 4] <- aggregate(db.bamet.lvl1$Tair_200~format(strptime(db.bamet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = max)[, 2]

        png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_Tair100_", jahr, ".png", sep = ""),
            width = p.width, height = p.height, pointsize = 8)
        par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
        plot(as.numeric(strptime(db.temp98$UTC, format = "%Y-%m-%d %H:%M")), db.temp98$Tair_100, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
             xlim = xxlim, ylim = c(-30, 25), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
        plot_maintenance(jahr)

        # horizontal lines
        for (ll in seq(-30, 30, 10)) {abline(h = ll, col = "gray80")}
        # vertical lines
        for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-30, 30), col = "gray80")}

        points(as.numeric(strptime(db.temp98$UTC[air100_zero], format = "%Y-%m-%d %H:%M")), db.temp98$Tair_100[air100_zero],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "lightgoldenrod3")
        points(as.numeric(strptime(murr[, 1], format = "%Y-%m-%d")) + 43200, murr[, 2],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "darkorange3")
        # points(as.numeric(strptime(db.temp98$UTC[air_flags], format = "%Y-%m-%d %H:%M")),
        #        db.temp98$Tair_200[air_flags], pch = 20, cex.lab = 1.5,     col = "red")
        axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
        axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
             labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
        text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
        text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, jahr, las = 2, cex = 6)

        dev.off()
      } else {cat(" no air100 temp data for", jahr, "\n")}}
  }# Air temp

  if (zack == 1) {
    #  precipitation
    # -----------------------------------------------------
    if (jahr >= 1998 && jahr <= 2010) {#

        #db.prec98_a <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/02_prec/BaMet1998_prec_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
        db.prec98 <- db.bamet[, c( "UTC", "prec", "prec_fl")]
      if (jahr == 2010) {#
        db.prec98_a <- db.bamet[, c( "UTC", "prec", "prec_fl")]
        db.prec98_b <- read.table(paste0(path$w[path$n == "LV1.p"] , "BaMet2009/00_full_dataset/BaMet2009_", jahr, "_lv1.dat"),
                                     sep = ",", dec = ".", header = T, fill = TRUE)[, c("UTC", "prec", "prec_fl")]
        db.prec98_c <- db.prec98_b
        newdf.a <- merge(db.prec98_b, db.prec98_a, all.x = T, by = "UTC")
        newdf.a[(newdf.a[, 3] == 0), 5] <- 0
        for (k in 2) {
          db.prec98_c[, k] <- rowMeans(cbind(db.prec98_b[, k], newdf.a[, k + 2]), na.rm = T)#
          db.prec98_c[, k + 1] <- newdf.a[, k + 3];db.prec98_c[is.na(db.prec98_c[, k + 1]), k + 1] <- 1
          db.prec98_c[is.nan(as.numeric(db.prec98_c[, k])), k] <- NA
        }
        db.prec98 <- db.prec98_c
      }


      pr_zero <- which(as.numeric(db.prec98$prec_fl) == 0 & db.prec98$prec > 0)
      pr_zero_zero <- which(as.numeric(db.prec98$prec_fl) == 0 & db.prec98$prec == 0)
      pr_flags <- which(as.numeric(db.prec98$prec_fl) > 0)
      if (length(pr_zero) > 0) {
        pr <- length(aggregate(db.prec98$prec~format(strptime(db.prec98$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
        precr <- matrix(ncol = 4, nrow = pr, 1)
        precr[, 1] <- aggregate(db.prec98$prec~format(strptime(db.prec98$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
        precr[, 2] <- aggregate(db.prec98$prec~format(strptime(db.prec98$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = sum)[, 2]



        png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_prec_", jahr, ".png", sep = ""),
            width = p.width, height = p.height, pointsize = 8)
        par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
        plot(as.numeric(strptime(db.prec98$UTC, format = "%Y-%m-%d %H:%M")), db.prec98$prec, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
             xlim = xxlim, ylim = c(0, 2.3), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
        plot_maintenance(jahr)

        # horizontal lines
        for (ll in seq(0, 1.5, 0.250)) {abline(h = ll, col = "gray80")}
        for (ll in seq(1.7, 2.2, 0.1)) {abline(h = ll, col = "gray80")}
        text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")), 2, "hourly", las = 2, srt = 20, cex = 5, col = "gray80")
        text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")), 1, "daily sum", las = 2, srt = 20, cex = 5, col = "gray80")
        # vertical lines
        for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 1.5), col = "gray80")}
        for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(1.7, 2.3), col = "gray80")}

        points(as.numeric(strptime(db.prec98$UTC[pr_zero_zero], format = "%Y-%m-%d %H:%M")), 2.2 - (db.prec98$prec[pr_zero_zero] / 40),
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "gray80")
        for (qq in pr_zero) {# real values in upper part
          lines(c(as.numeric(strptime(db.prec98$UTC[qq], format = "%Y-%m-%d %H:%M")),
                  as.numeric(strptime(db.prec98$UTC[qq], format = "%Y-%m-%d %H:%M"))),
                c(2.2, 2.2 - (db.prec98$prec[qq] / 40)), lwd = 2, cex.lab = 1.5, cex.axis = 1.7, col = "turquoise4")
        }
        for (dd in 1:length(precr[, 1])) {# real values in upper part
          lines(c(as.numeric(strptime(precr[dd, 1], format = "%Y-%m-%d")) + 43200,
                  as.numeric(strptime(precr[dd, 1], format = "%Y-%m-%d")) + 43200),
                c(1.5, (1.5 - as.numeric(precr[dd, 2]) / 20)), lwd = 3, cex.lab = 1.5, cex.axis = 1.7, col = "steelblue4")
        }

        axis(2, at = seq(0, 1.5, 0.25) , labels = rev(seq(0, 30, 5)), las = 2, cex.axis = 4)
        axis(2, at = seq(1.7, 2.2, 0.1), labels = rev(seq(0, 20, 4)), las = 2, cex.axis = 4)

        axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
             labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
        text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(2.3, 12), labels = Months, las = 2, cex = 4)
        text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, jahr, las = 2, cex = 6)
        if (jahr == 2012) {
          text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 1, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")
        }

        # legend("center", cex = 0.8, title = "", lty = 1, lwd = 3, col = c("darkblue", "green3", "red"), y.intersp = 0.8,
        #        box.col = "white", inset = 0.05, seg.len = 0.8, c("precipitation", "snowheight", "flagged"), bg = "white")
        dev.off()
        rm(pr_zero, pr_zero_zero, pr_flags)
      } else {
        cat(" no prec data for", jahr, "\n")
      }
    }
  }# Precipitation

  if (zack == 1) {
    #  windspeed and direction
    # -----------------------------------------------------
    if (jahr >= 1998 && jahr <= 2009) {#
      db.wind98 <- db.bamet[, c(1, 16:21)]
      if (jahr == 2009) {#
        #db.wind98_a <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/03_wind/BaMet1998_wind_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
        db.wind98_a <- db.bamet[, c(1, 16:21)]
        db.wind98_b <- db.met[, c(1, 12:17)]
        db.wind98_c <- db.wind98_b
        newdf.a <- merge(db.wind98_b, db.wind98_a, all.x = T, by = "UTC")
        newdf.a[which(newdf.a[, 3] == 0), c(  5, 7, 9, 11, 13)] <- 0  ##
        newdf.a[which(newdf.a[, 9] == 0), c(3, 5, 7, 11, 13)] <- 0  ##
        for (k in c(2, 4)) {
          db.wind98_c[, k] <- rowMeans(cbind(db.wind98_b[, k], newdf.a[, k + 6]), na.rm = T)#
          db.wind98_c[, k + 1] <- newdf.a[, k + 7]
          db.wind98_c[is.na(db.wind98_c[, k + 1]), k + 1] <- 1
          db.wind98_c[is.nan(as.numeric(db.wind98_c[, k])), k] <- NA
        }
        db.wind98 <- db.wind98_c
      }


      w_v_zero <- which(as.numeric(db.wind98$wind_v_200_fl) == 0 )
      w_d_zero <- which(as.numeric(db.wind98$wind_deg_200_fl) == 0)
      w_v_flags <- which(as.numeric(db.wind98$wind_v_200_fl) > 0)
      w_d_flags <- which(as.numeric(db.wind98$wind_deg_200_fl) > 0)
      day <- paste0(substr(as.character(db.wind98$UTC), 6, 7), substr(as.character(db.wind98$UTC), 9, 10))

      wind.meanz <- aggregate(db.wind98$wind_v_200 ~ day, FUN = mean, na.action = NULL)
      day.mitte <- db.wind98$UTC[substr(as.character(db.wind98$UTC), 12, 16) == "12:00"]
      weg <- data.frame(day.mitte, wind.meanz)
      weg <- weg[complete.cases(weg), ]
      png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_wind_", jahr, ".png", sep = ""),
          width = p.width, height = p.height, pointsize = 8)
      par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
      plot(as.numeric(strptime(db.wind98$UTC, format = "%Y-%m-%d %H:%M")), db.wind98$wind_v_200, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
           xlim = xxlim, ylim = c(0, 32), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
      plot_maintenance(jahr)

      # horizontal lines
      for (ll in seq(0, 24, 2)) {abline(h = ll, col = "gray80")}
      for (ll in seq(26, 32, 1)) {abline(h = ll, col = "gray80")}
      # vertical lines
      for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 24.3), col = "gray80")}
      for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(25.7, 32), col = "gray80")}

      points(as.numeric(strptime(db.wind98$UTC[w_d_zero], format = "%Y-%m-%d %H:%M")), (db.wind98$wind_deg_200[w_d_zero] / 60) + 26,
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "darkblue")
      points(as.numeric(strptime(db.wind98$UTC[w_v_zero], format = "%Y-%m-%d %H:%M")), db.wind98$wind_v_200[w_v_zero], pch = 20, cex.lab = 1.5,
             col = "green3")

      axis(2, at = seq(0, 24, 2), labels = seq(0, 24, 2), las = 2, cex.axis = 4)
      axis(2, at = seq(26, 32, 1), labels = seq(0, 360, 60), las = 2, cex.axis = 4)
      #mtext("snowheihgt[m]                        precipitation[mm]", side = 2, cex = 2)
      axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
           labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
      text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
      text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 22, jahr, las = 2, cex = 6)
      if (jahr == 2012) {
        text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 10, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")
      }
      dev.off()

      #  windspeed and direction (2) windrose
      # -----------------------------------------------------

      db.wind <- db.wind98[w_v_zero, c(1, 2, 4)]# bamet2010 c(1, 3, 5, 6)
      db.wind <- db.wind[complete.cases(db.wind), ]

      png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_wrose_", jahr, ".png", sep = ""), width = p.width * 0.8, height = p.width * 0.8, pointsize = 8)
      par(mar = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
      wind.rose(wind.freq(db.wind$wind_v_200, db.wind$wind_deg_200), key = F, 6, 4, ang = -3 * pi / 16, main = "", text.cex = 6)#paste(jahr)
      legend("topright", cex = 6, title = "", lty = 1, lwd = 3, col = c("transparent"), y.intersp = 0.8,
             box.col = "white", inset = 0.05, seg.len = 0.8, c(paste(jahr)), bg = "transparent")

      dev.off()
      png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_wrose_monthly_", jahr, ".png", sep = ""),
          width = p.width * 0.8, height = p.width * 0.8, pointsize = 8)
      par(mar = c(1, 1, 1, 1), omi = c(0, 0, 0, 0), mfrow = c(3, 4))
      for (monz in 1:12) {
        db.windity <- db.wind[which(format(as.Date(db.wind$UTC), format = "%m") == months[monz]), ]
        wind.rose.3(wind.freq(db.windity$wind_v_200, db.windity$wind_deg_200), key = F, 6, 4, ang = -3 * pi / 16, main = "", text.cex = 4)
      }
      dev.off()

    }
  }# Wind

  if (zack == 1) {
    #
    #
    #
    # #  humidity
    # # -----------------------------------------------------
    #
    #
    if (jahr >= 1998 && jahr <= 2009) {#
      db.hum98 <- db.bamet[, c("UTC", "RH_200", "RH_200_fl")]
      if (jahr == 2009) {#
        #db.hum98_a <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/04_humidity/BaMet1998_hum_", jahr, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
        db.hum98_a <- db.bamet[, c("UTC", "RH_200", "RH_200_fl")]
        db.hum98_b <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet2009/00_full_dataset/BaMet2009_", jahr, "_lv1.dat", sep = ""),
                                 sep = ",", dec = ".", header = T, fill = TRUE)[, c("UTC", "RH_200", "RH_200_fl")]
        db.hum98_c <- db.hum98_b
        newdf.b <- merge(db.hum98_b, db.hum98_a, all.x = T, by = "UTC")
        newdf.b[(newdf.b[, 3] == 0), 5] <- 0
        for (k in 2) {
          db.hum98_c[, k] <- rowMeans(cbind(db.hum98_b[, k], newdf.b[, k + 2]), na.rm = T)#
          db.hum98_c[, k + 1] <- newdf.b[, k + 3]
          db.hum98_c[is.na(db.hum98_c[, k + 1]), k + 1] <- 1
          db.hum98_c[is.nan(as.numeric(db.hum98_c[, k])), k] <- NA
        }
        db.hum98 <- db.hum98_c
      }

      hum_zero <- which(as.numeric(db.hum98$RH_200_fl) == 0)
      hum_flags <- which(as.numeric(db.hum98$RH_200_fl) > 0)
      if (length(hum_zero) > 0) {
        hr <- length(aggregate(db.hum98$RH_200[hum_zero]~format(strptime(db.hum98$UTC[hum_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
        humr <- matrix(ncol = 4, nrow = hr, 1)
        humr[, 1] <- aggregate(db.hum98$RH_200[hum_zero]~format(strptime(db.hum98$UTC[hum_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
        humr[, 2] <- aggregate(db.hum98$RH_200[hum_zero]~format(strptime(db.hum98$UTC[hum_zero], format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2]



        png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_hum_", jahr, ".png", sep = ""),
            width = p.width, height = p.height, pointsize = 8)
        par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
        plot(as.numeric(strptime(db.hum98$UTC, format = "%Y-%m-%d %H:%M")), db.hum98$RH_200, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
             xlim = xxlim, ylim = c(0, 105), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
        plot_maintenance(jahr)

        # horizontal lines
        for (ll in seq(0, 100, 10)) {abline(h = ll, col = "gray80")}
        # vertical lines
        for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 100), col = "gray80")}

        points(as.numeric(strptime(db.hum98$UTC[hum_zero], format = "%Y-%m-%d %H:%M")), db.hum98$RH_200[hum_zero],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "aquamarine3")
        points(as.numeric(strptime(humr[, 1], format = "%Y-%m-%d")) + 43200, humr[, 2],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, cex = 2.5, col = "aquamarine4")

        # points(as.numeric(strptime(db.hum98$UTC[hum_flags], format = "%Y-%m-%d %H:%M")),
        #        db.hum98$RH_200[hum_flags], pch = 20, cex.lab = 1.5,     col = "red")

        axis(2, at = seq(0, 100, 10), labels = seq(0, 100, 10), las = 2, cex.axis = 4)
        axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
             labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
        if (jahr == 2012) {
          text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M")) + 1000000, 50, "reindeer", las = 2, srt = 60, cex = 4, col = "gray80")
        }
        text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(103, 12), labels = Months, las = 2, cex = 4)
        text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 7, jahr, las = 2, cex = 6)
        dev.off()
        rm(hum_zero, hum_flags)
      } else {
        cat(" no humidity data for", jahr, "\n")
      }
    }
  }# Humidity

  if (zack == 1) {
    #
    #  soil temperature
    # -----------------------------------------------------
    #
    if (jahr == 2009) {#
      db.bamet.lvl1 <- read.table(paste(path$w[path$n == "LV1.p"], "BaMet2009/00_full_dataset/BaMet2009_", jahr, "_lv1.dat", sep = ""),
                                  sep = ",", dec = ".", header = T, fill = TRUE)
    }
    db.soil98 <- db.bamet[, c("UTC", "Ts_252_2", "Ts_252_2_fl", "Ts_252_12", "Ts_252_12_fl", "Ts_252_32", "Ts_252_32_fl", "Ts_252_62", "Ts_252_62_fl",
                           "Ts_252_102", "Ts_252_102_fl", "Ts_252_152", "Ts_252_152_fl", "Ts_203_2", "Ts_203_2_fl", "Ts_203_5", "Ts_203_5_fl",
                           "Ts_203_23", "Ts_203_23_fl", "Ts_203_53", "Ts_203_53_fl", "Ts_203_93", "Ts_203_93_fl", "Ts_203_143", "Ts_203_143_fl")]

    soil.cols <- colorRampPalette(c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4"))(150)
    png(file = paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_Ts_203_", jahr, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.soil98$UTC, format = "%Y-%m-%d %H:%M")), db.soil98$Ts_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, #
         xlim = xxlim, ylim = c(-15, 18), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
    plot_maintenance(jahr)

    # horizontal lines
    for (ll in seq(-15, 15, 5)) {abline(h = ll, col = "gray80")}
    # vertical lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-16, 18), col = "gray80")}
    right <- c(2, 5, 23, 53, 93, 143)#c(2, 5, 23, 53, 93, 143)
    for (qq in 12:7) {
      points(as.numeric(strptime(db.soil98[(db.soil98[, qq * 2 + 1] == 0), 1], format = "%Y-%m-%d %H:%M")),
             db.soil98[(db.soil98[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[right[qq-6]])
    }
    if (jahr == 2009) {#
      for (qq in 28:23) {
        points(as.numeric(strptime(db.bamet.lvl1[(db.bamet.lvl1[, qq * 2 + 1] == 0), 1], format = "%Y-%m-%d %H:%M")),
               db.bamet.lvl1[(db.bamet.lvl1[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[right[qq-22]])
      }
    }

    axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, jahr, las = 2, cex = 6)
    dev.off()#close pdf

    png(file = paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_Ts_253_", jahr, ".png", sep = ""),
        width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.soil98$UTC, format = "%Y-%m-%d %H:%M")), db.soil98$Ts_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, #
         xlim = xxlim, ylim = c(-15, 18), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.lab = 3)
    plot_maintenance(jahr)

    # horizontal lines
    for (ll in seq(-15, 15, 5)) {abline(h = ll, col = "gray80")}
    # vertical lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-16, 18), col = "gray80")}
    left <- c(2, 12, 32, 62, 102, 150)#c(2, 12, 32, 62, 102, 150)
    for (qq in 6:1) {
      points(as.numeric(strptime(db.soil98[(db.soil98[, qq * 2 + 1] == 0), 1], format = "%Y-%m-%d %H:%M")),
             db.soil98[(db.soil98[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[left[qq]])
    }
    if (jahr == 2009) {#
      for (qq in 22:17) {
        points(as.numeric(strptime(db.bamet.lvl1[(db.bamet.lvl1[, qq * 2 + 1] == 0), 1], format = "%Y-%m-%d %H:%M")),
               db.bamet.lvl1[(db.bamet.lvl1[, qq * 2 + 1] == 0), qq * 2], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[left[qq-16]])
      }
    }

    axis(2, at = seq(-15, 15, 5), labels = seq(-15, 15, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
         labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(18, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, jahr, las = 2, cex = 6)
    dev.off()#close pdf
  }# Soil temperature

  if (zack == 1) {
    #
    #
    #  snow temperature
    # -----------------------------------------------------
    if (jahr >= 1998 && jahr <= 2009) {#

      if (jahr == 2009) {#
        # db.snowt98_a <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet1998/01_air_temp/BaMet1998_snowt_", jahr, ".dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
        db.snowt98_b <- read.table(paste(path$w[path$n == "LV1.p"] , "BaMet2009/00_full_dataset/BaMet2009_", jahr, "_lv1.dat", sep = ""),
                                   sep = ",", dec = ".", header = T, fill = TRUE)
        snowt20_zero_b <- which(as.numeric(db.snowt98_b$Tair_20_fl) == 0)
        snowt20_flags_b <- which(as.numeric(db.snowt98_b$Tair_20_fl) == 8)
        snowt05_zero_b <- which(as.numeric(db.snowt98_b$Tair_4_fl) == 0)
        snowt05_flags_b <- which(as.numeric(db.snowt98_b$Tair_4_fl) == 8)
      }

      snowt20_zero <- which(as.numeric(db.bamet$Tair_20_fl) == 0)
      snowt20_flags <- which(as.numeric(db.bamet$Tair_20_fl) == 8)
      snowt05_zero <- which(as.numeric(db.bamet$Tair_5_fl) == 0)
      snowt05_flags <- which(as.numeric(db.bamet$Tair_5_fl) == 8)

      png(paste(path$w[path$n == "plot.p"], jahr, "/BaMet1998_Tsn_", jahr, ".png", sep = ""),
          width = p.width, height = p.height, pointsize = 8)
      par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
      plot(as.numeric(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M")), db.bamet$Tair_20, pch = 20, # cex.lab = 1.7, cex.axis = 1.5, # albedo from file
           xlim = xxlim, ylim = c(-30, 25), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3)
      plot_maintenance(jahr)

      # horizontal lines
      for (ll in seq(-30, 30, 10)) {abline(h = ll, col = "gray80")}
      # vertical lines
      for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-30, 30), col = "gray80")}

      points(as.numeric(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M")), db.bamet$Tair_20,
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "olivedrab3")
      points(as.numeric(strptime(db.bamet$UTC, format = "%Y-%m-%d %H:%M")), db.bamet$Tair_5,
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "sienna2")
      points(as.numeric(strptime(db.bamet$UTC[snowt20_flags], format = "%Y-%m-%d %H:%M")), db.bamet$Tair_20[snowt20_flags],
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "olivedrab4")
      points(as.numeric(strptime(db.bamet$UTC[snowt05_flags], format = "%Y-%m-%d %H:%M")), db.bamet$Tair_5[snowt05_flags],
             pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "sienna4")

      if (jahr == 2009) {
        points(as.numeric(strptime(db.snowt98_b$UTC, format = "%Y-%m-%d %H:%M")), db.snowt98_b$Tair_20,
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "olivedrab3")
        points(as.numeric(strptime(db.snowt98_b$UTC, format = "%Y-%m-%d %H:%M")), db.snowt98_b$Tair_4,
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "sienna2")
        points(as.numeric(strptime(db.snowt98_b$UTC[snowt20_flags_b], format = "%Y-%m-%d %H:%M")), db.snowt98_b$Tair_20[snowt20_flags_b],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "olivedrab4")
        points(as.numeric(strptime(db.snowt98_b$UTC[snowt05_flags_b], format = "%Y-%m-%d %H:%M")), db.snowt98_b$Tair_4[snowt05_flags_b],
               pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "sienna4")

      }

      axis(2, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 2, cex.axis = 4)
      axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
           labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
      text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
      text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, jahr, las = 2, cex = 6)
      dev.off()#
    }# snow temperature
  }# Snow temperature

  cat(paste0(" create new BaMet1998 plots for ", jahr, "\n"))
}
