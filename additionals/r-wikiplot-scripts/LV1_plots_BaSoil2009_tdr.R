#############################################################################
##
##  Plots to wiki
##
##
##
##
##  by: stephan.lange@awi.de
##  last modified: 2016-02-04
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

options(scipen = 100, stringsAsFactors = F) # for non-exponential display of numeric values
origin <- "1970-01-01"
soil.cols <- c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4")
ice.cols  <- colorRampPalette(c("palegreen1", "ivory1", "lightblue1", "slateblue4"))(30)
ice2.cols <- c("seagreen4", "palegreen3", "yellow3", "khaki", "sandybrown", "peru", "mistyrose3", "peachpuff4")
files2read <- list.files(paste0(p.1$w[p.1$n == "BaS.lv1.p"], "/TDR_complete"), pattern = ".dat")
Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)
#p.width = 420;p.height = 280
#p.width = 420*1.5;p.height = 280*1.5
p.width <- 420 * 3.5
p.height <- 280 * 3.5

########
# to run this script separately, you have to set run.year:
#
# recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# run.year <- 2009:2019
#######


for (year in run.year) {

 # dada<-read.table(paste0(p.1$w[p.1$n == "LV1.p"], "BaSoil2009/02_tdr/BaSoil2009_tdr_", year, "_lv1.dat"), sep = ",", dec = ".", header = T, na.strings = "NA")
 # ice.dada<-read.table(paste0(p.1$w[p.1$n == "LV2.p"], "Bayelva/VWC/BaSoil2009Ice_", year, ".dat"), sep = ",", dec = ".", header = T, na.strings = "NA")
 # vwc.data<-read.table(paste0(p.1$w[p.1$n == "LV1.p"], "BaSoil2009/05_vwc_roth/BaSoil2009_vwc_roth_", year, "_lv1.dat"), sep = ",", dec = ".", header = T, na.strings = "NA")
 dada <- read.table(paste0(p.1$w[p.1$n == "LV1.p"], "BaSoil2009/00_full_dataset/BaSoil2009_", year, "_lv1.dat"),
                    sep = ",", dec = ".", header = T, na.strings = "NA")[, -c(2:17)]
 #ice.dada<-read.table(paste0(p.1$w[p.1$n == "LV2.p"], "Bayelva/VWC/BaSoil2009Ice_", year, ".dat"), sep = ",", dec = ".", header = T, na.strings = "NA")

 lischt <- c(dada$UTC[format(strptime(dada$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"],
           dada$UTC[length(dada$UTC)])

 dada[, 1] <- as.numeric(as.POSIXct(dada[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'))
 #ice.dada[, 1]<-as.numeric(as.POSIXct(ice.dada[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'))
# vwc.data[, 1]<-as.numeric(as.POSIXct(vwc.data[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'))
 xxlim = c(as.numeric(strptime(paste0("13.01.", year), format = "%d.%m.%Y")),
           as.numeric(strptime(paste0("20.12.", year), format = "%d.%m.%Y")))

 ###############################################################################################################################
 ###############################################################################################################################
 png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/BaSoil2009_tdr_cond_", year, ".png", sep = ""),
     width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
 par(mar = c(1, 10, 1, 1), omi = c(0, 0, 0, 0))
 plot(as.numeric(strptime(dada$UTC, format = "%Y-%m-%d %H:%M")), dada$cond_h_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  #
    xlim = xxlim, ylim = c(0, 0.03), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
 plot_maintenance(year)

 # horizontal lines
 abline(h = seq(0, 0.025, .005), col = "gray80")
 # vertical lines
 abline(v = as.numeric(seq(as.POSIXct("2009-01-01 01:00:00"),
                           as.POSIXct(paste(as.numeric(year) + 1, "-01-01 01:00:00", sep = "")),
                           by = "month")), col = "gray80")

 for (qq in seq(18, 31, 2) ) {
     points(dada[dada[, qq + 1] == 0, 1], dada[dada[, qq + 1] == 0, qq],
            pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[(qq / 2) - 8])
 }

 for (qq in seq(18, 31, 2) ) {
     points(dada[dada[, qq + 1] > 0, 1], dada[dada[, qq + 1] > 0, qq],
            pch = 1 , cex.lab = 1.5, cex.axis = 1.7, col = "darkmagenta")
 }
 axis(2, at = seq(0, 0.025, .005), labels = format(seq(0, 0.025, .005), digits = 4), las = 2, cex.axis = 4)
 #axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.03, 12),
      labels = Months, las = 2, cex = 4)
 text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 0.02,
      year, las = 2, cex = 6)
 dev.off()#close pdf
 ###############################################################################################################################
 ###############################################################################################################################
 png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/BaSoil2009_tdr_vwc_", year, ".png", sep = ""),
     width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
 par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
 plot(as.numeric(strptime(dada$UTC, format = "%Y-%m-%d %H:%M")), dada$vwc_h_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  #
    xlim = xxlim, ylim = c(-0.03, 0.52), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
 plot_maintenance(year)

 # horizontal lines
 abline(h = seq(0, 0.5, .05), col = "gray80")
 # vertical lines
 abline(v = as.numeric(seq(as.POSIXct("2009-01-01 01:00:00"),
                           as.POSIXct(paste(as.numeric(year) + 1, "-01-01 01:00:00", sep = "")),
                           by = "month")), col = "gray80")

 for (qq in seq(34, 46, 2) ) {
     points(dada[dada[, qq + 1] == 0, 1], dada[dada[, qq + 1] == 0, qq],
            pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[(qq / 2) - 16])
 }
 for (qq in seq(34, 46, 2) ) {
     points(dada[dada[, qq + 1] > 0, 1], dada[dada[, qq + 1] > 0, qq],
            pch = 1 , cex.lab = 1.5, cex.axis = 1.7, col = "darkmagenta")
 }

 axis(2, at = seq(0, 0.5, .05), labels = format(seq(0, 0.5, .05), digits = 2), las = 2, cex.axis = 4)
 #axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.52, 12),
      labels = Months, las = 2, cex = 4)
 text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 0.3,
      year, las = 2, cex = 6)
 dev.off()#close pdf
 ###############################################################################################################################
 ###############################################################################################################################
 # png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/BaSoil2009_tdr_vwcII_", year, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
 # par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0)) ###  ROTH  #####
 # plot(as.numeric(strptime(vwc.data$UTC, format = "%Y-%m-%d %H:%M")), vwc.data$vwc_h_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  #
 #   xlim = xxlim, ylim = c(-0.03, 0.52), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
 # plot_maintenance(year)
 # for (ll in seq(0, 0.5, .05)) {abline(h = ll, col = "gray80")} # horizontal lines
 # for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 0.5), col = "gray80")} # vertical lines
 # for (qq in seq(2, 14, 2) ) {points(vwc.data[vwc.data[, qq+1] == 0, 1], vwc.data[vwc.data[, qq+1] == 0, qq], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[qq-1])}
 # for (qq in seq(2, 14, 2) ) {points(vwc.data[vwc.data[, qq+1]> 0, 1], vwc.data[vwc.data[, qq+1]> 0, qq], pch = 1, cex.lab = 1.5, cex.axis = 1.7, col = "darkmagenta")}
 # #for (qq in seq(34, 49, 2) ) {points(dada[dada[, qq+1] == 0, 1], dada[dada[, qq+1] == 0, qq], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[(qq/2)-16])}
 #
 # axis(2, at = seq(0, 0.5, .05), labels = seq(0, 0.5, .05), las = 2, cex.axis = 4)
 # #axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 # text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M"))-1300000, rep(0.52, 12), labels = Months, las = 2, cex = 4)
 # text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M"))+2000000, 0.3, year, las = 2, cex = 6)
 # dev.off()#close pdf
 ###############################################################################################################################
 ###############################################################################################################################
 png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/BaSoil2009_tdr_diel_", year, ".png", sep = ""),
     width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
 par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
 plot(as.numeric(strptime(dada$UTC, format = "%Y-%m-%d %H:%M")), dada$vwc_h_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  #
    xlim = xxlim, ylim = c(0, 32), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
 plot_maintenance(year)

 for (ll in seq(0, 30, 5)) {abline(h = ll, col = "gray80")} # horizontal lines
 for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 31), col = "gray80")} # vertical lines
 for (qq in seq(2, 15, 2) ) {points(dada[dada[, qq + 1] == 0, 1], dada[dada[, qq + 1] == 0, qq],
                                    pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[(qq/2)])}
 for (qq in seq(2, 15, 2) ) {points(dada[dada[, qq + 1] > 0, 1], dada[dada[, qq + 1] > 0, qq],
                                    pch = 1 , cex.lab = 1.5, cex.axis = 1.7, col = "darkmagenta")}

 axis(2, at = seq(0, 30, 5), labels = seq(0, 30, 5), las = 2, cex.axis = 4)
 #axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(32, 12), labels = Months, las = 2, cex = 4)
 text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 15, year, las = 2, cex = 6)
 dev.off()#close pdf
 ###############################################################################################################################
 ###############################################################################################################################
 png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/BaSoil2009_snow_diel_", year, ".png", sep = ""),
     width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
 par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
 plot(as.numeric(strptime(dada$UTC, format = "%Y-%m-%d %H:%M")), dada$E2_sn_v_0, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  #
    xlim = xxlim, ylim = c(0, 10), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
 plot_maintenance(year)

 for (ll in seq(0, 10, 2)) {abline(h = ll, col = "gray80")} # horizontal lines
 for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 31), col = "gray80")} # vertical lines
 #for (qq in seq(2, 15, 2) ) {points(dada[dada[, qq+1] == 0, 1], dada[dada[, qq+1] == 0, qq], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[(qq/2)])}
 # for (qq in seq(2, 17, 2) ) {points(dada[dada[, qq+1] == 0, 1], dada[dada[, qq+1] == 0, qq], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[(qq/2)])}
 points(dada$UTC, dada$E2_sn_v_0, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "turquoise")
 points(dada$UTC[ which(as.numeric(dada$E2_sn_v_0_fl) > 0)], dada$E2_sn_v_0[ which(as.numeric(dada$E2_sn_v_0_fl) > 0)],
        pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "red")
 axis(2, at = seq(0, 10, 2), labels = seq(0, 10, 2), las = 2, cex.axis = 4)
 #axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(10, 12), labels = Months, las = 2, cex = 4)
 text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, 8, year, las = 2, cex = 6)
 dev.off()#close pdf
 ###############################################################################################################################
 ###############################################################################################################################
 # png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/BaSoil2009_ice_", year, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
 # par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
 #
 #
 # plot(as.numeric(strptime(ice.dada$UTC, format = "%Y-%m-%d %H:%M")), ice.dada$vwc_h_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  #
 #   xlim = xxlim, ylim = c(0, 45), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
 # plot_maintenance(year)
 # diefen<-c(1, 11, 21, 37, 55, 71, 89)
 # for (ll in seq(0, 40, 5)) {abline(h = ll, col = "gray80")} # horizontal lines
 # for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(0, 42), col = "gray80")} # vertical lines
 # #for (qq in 2:8 ) {points(ice.dada[, 1], rep(diefen[qq-1], length(ice.dada[, 1]))/(-100), cex = 8, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = ice.cols[100*ice.dada[, qq]])}
 # for (qq in 2:8 ) {points(ice.dada[, 1], ice.dada[, qq]*100, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = soil.cols[qq-1])}
 #
 # axis(2, at = seq(0, 40, 5), labels = (seq(0, 40, 5)), las = 2, cex.axis = 4)
 # #axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 # text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M"))-1300000, rep(45, 12), labels = Months, las = 2, cex = 4)
 # text(as.numeric(strptime(lischt[3], format = "%Y-%m-%d %H:%M"))+2000000, 8, year, las = 2, cex = 6)
 #
 # dev.off()#close pdf
 ###############################################################################################################################
 ###############################################################################################################################
 # png(file = paste(p.1$w[p.1$n == "plot.p"], year, "/BaSoil2009_icegrid_", year, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)#, A4, landscape)
 # par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
 #
 #
 # plot(as.numeric(strptime(ice.dada$UTC, format = "%Y-%m-%d %H:%M")), ice.dada$vwc_h_1, pch = 20, # cex.lab = 1.7, cex.axis = 1.5,  #
 #   xlim = xxlim, ylim = c(-1, 0.1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
 # plot_maintenance(year)
 #
 # oben <-c(0, 8, 18, 34, 52, 68, 86)/(-100)
 # diefen<-c(1, 11, 21, 37, 55, 71, 89)
 # unten <-c(4, 14, 24, 40, 58, 74, 92)/(-100)
 # for (ll in seq(-1, 0, 0.1)) {abline(h = ll, col = "gray80")} # horizontal lines
 # for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {lines(c(pp, pp), c(-1, 0), col = "gray80")} # vertical lines
 # # for (qq in 2:8 ) {points(ice.dada[, 1], rep(diefen[qq-1], length(ice.dada[, 1]))/(-100), cex = 8, pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = ice.cols[100*ice.dada[, qq]])}
 # for (qq in 2:8 ) {
 #for (rr in 1:length(ice.dada[, 1])) {
 #   lines(c(ice.dada[rr, 1], ice.dada[rr, 1]),
 #      c(unten[qq-1], oben[qq-1]),
 #      cex.lab = 1.5, cex.axis = 1.7, col = ice.cols[100*ice.dada[rr, qq]])}}
 #
 # axis(2, at = seq(-1, 0, 0.1), labels = (seq(-100, 0, 10)*(-1)), las = 2, cex.axis = 4)
 # axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
 # text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M"))-1300000, rep(0.1, 12), labels = Months, las = 2, cex = 4)
 # text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M"))+2000000, -0.3, year, las = 2, cex = 6)
 #
 # dev.off()#close pdf
 cat("#\n# level1 BaSoil_tdr ", year, " plot done!\n#\n")
}


