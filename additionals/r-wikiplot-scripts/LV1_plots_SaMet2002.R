###...........................................................................
##
#     wikiplots    SaMet2002       --------------------
##   
##   see results here
##   http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:samoylov:met:rad:samet2002
##
##   by: Stephan.Lange@awi.de
##   modified: 2021-05-07
##
##   changes:
##   2021-05-07 SL adapted to refresh app and git
##   
###...........................................................................
## to run this script separately, you have to uncomment the next 10 lines!
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
###...........................................................................


options(scipen = 100, stringsAsFactors = F, digits = 2, scientific = T) # for non-exponential display of numeric values
origin <- "1970-01-01"
# run.year <- 2018:2021 #2002:2021


zack <- 1
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Months <- c("Jan", " Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


p.width <- 420 * 3.5
p.height <- 280 * 3.5
color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)


for (year_i in run.year) {
  
  if (zack == 1) {
    #  load data ----------
    ###...........................................................................
    
    db.samet <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "SaMet2002/00_full_dataset/SaMet2002_", year_i, "_lv1_noflag.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
    db.samet.lvl1 <- read.table(paste(p.1$w[p.1$n == "LV1.p"], "SaMet2002/00_full_dataset/SaMet2002_", year_i, "_lv1.dat", sep = ""), sep = ",", dec = ".", header = T, fill = TRUE)
    # db.lasyear_i <-read.table(paste(p.1$w[p.1$n=="LV1.p"],"SaMet2009/00_full_dataset/samet2009_",year_i-1,"_lv1.dat",sep=""),sep=",",dec=".",header=T, fill = TRUE)
    # last.von     <-length(na.omit(db.samet.lvl1[,2]))
    # last.bis     <-length(db.lasyear_i[,1])


    xxlim <- c(as.numeric(strptime(paste0("13.01.", year_i), format = "%d.%m.%Y")), as.numeric(strptime(paste0("20.12.", year_i), format = "%d.%m.%Y")))

    # plotting analysis
    ###...........................................................................
    # some boundaries
    # ylim_sw <- plot_bounderies(db.samet.lvl1$SwOut,db.samet.lvl1$SwIn)      # get plotting bounderies shortwave
    # ylim_lw <- plot_bounderies(db.samet.lvl1$LwOut_cor,db.samet.lvl1$LwIn_cor)  # get plotting bounderies longwave
    lischt <- c(db.samet.lvl1$UTC[format(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%d %H:%M") == "01 00:00"], db.samet.lvl1$UTC[length(db.samet.lvl1$UTC)])
  } # Load data

  if (zack == 1) {
    #  albedo (from file vs. calculated (Out/In)) ----------
    ###...........................................................................
    albedo.good <- which(db.samet.lvl1$Albedo_fl == 0)
    albedo.bad <- which(db.samet.lvl1$Albedo_fl > 1)
    ohne.albedo <- which(db.samet.lvl1$Albedo_fl == 1)
    alb.outside <- which(db.samet.lvl1$Albedo < 0 | db.samet.lvl1$Albedo > 1)

    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_albedo_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Albedo,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-1, 2), xlab = "Date", ylab = "[W / m]", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(-4, 4, 1)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-5, 5), col = "gray80")
    } # vertical lines

    # points(as.numeric(strptime(db.samet.lvl1$UTC[ohne.albedo],format="%Y-%m-%d %H:%M")),db.samet.lvl1$SwIn[ohne.albedo]/db.samet.lvl1$SwOut[ohne.albedo], pch = 20, cex.lab = 1.5, cex.axis=1.7,
    #        xlim=xxlim, ylim=c(-5,5),col="plum")             # albedo = out/in
    # points(as.numeric(strptime(db.samet.lvl1$UTC[albedo.good],format="%Y-%m-%d %H:%M")),db.samet.lvl1$Albedo[albedo.good], pch = 20, cex.lab = 1.5,
    #        xlim=xxlim, ylim=c(-5,5),col="purple4")             #
    # points(as.numeric(strptime(db.samet.lvl1$UTC[alb.outside],format="%Y-%m-%d %H:%M")),db.samet.lvl1$Albedo[alb.outside], pch = 20, cex.lab = 1.5,
    #        xlim=xxlim, ylim=c(-5,5),col="red")
    points(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$SwOut / db.samet.lvl1$SwIn,
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      xlim = xxlim, ylim = c(-5, 5), col = "plum"
    ) # albedo = out/in

    axis(2, at = c(0, 0.5, 1), labels = c(0, 50, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(5, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, -4.7, year_i, las = 2, cex = 6)
    dev.off()
    rm(albedo.bad, ohne.albedo)
  } # Albedo (implemented, but crumpy data)

  if (zack == 1) {
    # pressure ----------
    ###...........................................................................
    pa_zero <- which(db.samet.lvl1$PA_fl == 0)
    pa_flags <- which(db.samet.lvl1$PA_fl > 0)



    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_pa_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$PA,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(96, 108), xlab = " ", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(96, 110, 2)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-210, 720), col = "gray80")
    } # vertical lines

    points(as.numeric(strptime(db.samet.lvl1$UTC[pa_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$PA[pa_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "maroon4")

    axis(2, at = seq(96, 110, 2), labels = seq(96, 110, 2), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(107), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 97, year_i, las = 2, cex = 6)
    dev.off()
  } # Pressure athmospheric (implemented, but crumpy data)

  if (zack == 1) {
    #  radiation Netto ----------
    ###...........................................................................
    net_zero <- which(db.samet.lvl1$RadNet_fl == 0)
    net_flags <- which(db.samet.lvl1$RadNet_fl > 0)



    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_rad_net_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$SwOut,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-200, 720), xlab = " ", ylab = "", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(-200, 600, 100)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-210, 720), col = "gray80")
    } # vertical lines
    ###
    ###  please change to the upper points() and remove the RadNet-column
    ###
    # points(as.numeric(strptime(db.samet.lvl1$UTC[net_zero],format="%Y-%m-%d %H:%M")),
    #        db.samet.lvl1$SwOut[net_zero]-db.samet.lvl1$SwIn[net_zero]+db.samet.lvl1$LwOut_cor[net_zero]-db.samet.lvl1$LwIn_cor[net_zero],
    #        pch = 20, cex.lab = 1.5, cex.axis=1.7, col="maroon4")
    points(as.numeric(strptime(db.samet.lvl1$UTC[net_zero], format = "%Y-%m-%d %H:%M")),
      db.samet.lvl1$RadNet[net_zero],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "maroon4"
    )

    axis(2, at = seq(-200, 600, 100), labels = seq(-200, 600, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, year_i, las = 2, cex = 6)
    dev.off()
  } # Radiation netto (implemented, but crumpy data and wrong column)

  if (zack == 1) {
    #  radiation Global ----------
    ###...........................................................................
    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_rad_gl_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$SwIn,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-5, 720), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(0, 600, 100)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-210, 720), col = "gray80")
    } # vertical lines

    points(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$SwIn,
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "lightsalmon4"
    )
    axis(2, at = seq(0, 600, 100), labels = seq(0, 600, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, year_i, las = 2, cex = 6)
    dev.off()
  } # Radiation global (implemented, but crumpy data)

  if (zack == 1) {
    #  radiation shortwave ----------
    ###...........................................................................
    sw_in <- which(db.samet.lvl1$SwIn_fl == 0)
    sw_out <- which(db.samet.lvl1$SwOut_fl == 0)
    sw_bad_in <- which(db.samet.lvl1$SwIn_fl > 0)
    sw_bad_out <- which(db.samet.lvl1$SwOut_fl > 0)


    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_rad_sw_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$SwOut,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-5, 720), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(0, 600, 100)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-20, 720), col = "gray80")
    } # vertical lines

    points(as.numeric(strptime(db.samet.lvl1$UTC[sw_in], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$SwIn[sw_in],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "mediumpurple3"
    )
    points(as.numeric(strptime(db.samet.lvl1$UTC[sw_out], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$SwOut[sw_out],
      pch = 20, cex.lab = 1.5,
      col = "khaki3"
    )
    axis(2, at = seq(0, 600, 100), labels = seq(0, 600, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(700, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 600, year_i, las = 2, cex = 6)
    dev.off()
  } # Radiation shortwave (implemented, but crumpy data)

  if (zack == 1) {

    #  radiation longwave ----------
    ###...........................................................................
    lw_in <- which(db.samet.lvl1$LwOut_fl == 0)
    lw_out <- which(db.samet.lvl1$LwIn_fl == 0)
    lw_bad_in <- which(db.samet.lvl1$LwOut_fl > 0)
    lw_bad_out <- which(db.samet.lvl1$LwIn_fl > 0)

    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_rad_lw_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$LwOut,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(100, 500), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(100, 500, 100)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-20, 720), col = "gray80")
    } # vertical lines

    points(as.numeric(strptime(db.samet.lvl1$UTC[lw_in], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$LwOut[lw_in],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "mediumpurple3"
    )
    points(as.numeric(strptime(db.samet.lvl1$UTC[lw_out], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$LwIn[lw_out],
      pch = 20, cex.lab = 1.5,
      col = "khaki3"
    )
    axis(2, at = seq(100, 500, 100), labels = seq(100, 500, 100), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(500, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 450, year_i, las = 2, cex = 6)
    dev.off()
  } # Radiation longwave (implemented)

  if (zack == 1) {
    # air temperature ----------
    ###...........................................................................
    

    Tair1_zero <- which(as.numeric(db.samet.lvl1$Tair_a_200_fl) == 0)
    Tair1_flags <- which(as.numeric(db.samet.lvl1$Tair_a_200_fl) > 0)
    Tair2_zero <- which(as.numeric(db.samet.lvl1$Tair_b_200_fl) == 0)
    Tair2_flags <- which(as.numeric(db.samet.lvl1$Tair_b_200_fl) > 0)
    Tair3_zero <- which(as.numeric(db.samet.lvl1$Tair_a_50_fl) == 0)
    Tair3_flags <- which(as.numeric(db.samet.lvl1$Tair_a_50_fl) > 0)
    Tair4_zero <- which(as.numeric(db.samet.lvl1$Tair_b_50_fl) == 0)
    Tair4_flags <- which(as.numeric(db.samet.lvl1$Tair_b_50_fl) > 0)

    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_Tair1_200_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_a_200,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-40, 25), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(-40, 30, 5)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-40, 30), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.samet.lvl1$UTC[Tair1_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_a_200[Tair1_zero],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "lightgoldenrod3"
    )
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year_i, las = 2, cex = 6)

    dev.off()

    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_Tair2_200_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_b_200,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-40, 25), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(-40, 30, 5)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-40, 30), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.samet.lvl1$UTC[Tair2_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_b_200[Tair2_zero],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "lightgoldenrod3"
    )
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year_i, las = 2, cex = 6)

    dev.off()
    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_Tair1_50_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_a_50,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-40, 25), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(-40, 30, 5)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-40, 30), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.samet.lvl1$UTC[Tair3_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_a_50[Tair3_zero],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "lightgoldenrod3"
    )
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year_i, las = 2, cex = 6)

    dev.off()
    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_Tair2_50_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_b_50,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(-40, 25), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(-40, 30, 5)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-40, 30), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.samet.lvl1$UTC[Tair4_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Tair_b_50[Tair4_zero],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "lightgoldenrod3"
    )
    axis(2, at = seq(-40, 30, 5), labels = seq(-40, 30, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 18, year_i, las = 2, cex = 6)

    dev.off()
  } # Air temperature (1,2,3,4) (implemented, but crumpy data)

  if (zack == 1) {
    #  precipitation ----------
    ###...........................................................................
    # NEW plot (with monthly and annual sums)

    # only precipitation values with flag zero
    prec.zero <- db.samet.lvl1$prec
    prec.zero[which(as.numeric(db.samet.lvl1$prec_fl) > 0)] <- NA
    # only precipitation values with flags other than zero
    # prec.flags <- db.samet.lvl1$prec
    # prec.flags[which(as.numeric(db.samet.lvl1$prec_fl) == 0)] <- NA

    prec.daily <- aggregate(prec.zero ~ format(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = sum)
    prec.monthly <- aggregate(prec.zero ~ format(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m"), FUN = sum)

    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_prec_", year_i, ".png", sep = ""),
      width = p.width, height = p.height, pointsize = 8
    )
    par(mfrow = c(3, 1), mar = c(0, 8, 0, 0), oma = c(1, 0, 1, 1))

    ###...........................................................................
    # hourly sums
    # plot all precipitation values (including the ones with flags other than zero)
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$prec,
      t = "h",
      xlim = xxlim, xlab = "", ylab = "", xaxt = "n", ylim = c(0, 20), cex.axis = 4, lwd = 3, col = "red"
    )
    # overplot with flag zero precipitation values
    points(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), prec.zero, t = "h", lwd = 3, col = "steelblue4")
    # plot maintenance
    plot_maintenance(year_i)
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    # horizontal lines
    abline(h = seq(0, 15, 5), lty = 1, col = "gray80")
    # add labels and axis
    mtext(text = Months, side = 3, line = -5, at = as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, cex = 4)
    mtext(text = "hourly sum", side = 3, line = -10, cex = 4, col = "steelblue4")
    mtext(text = "flags > 0", side = 3, line = -10, cex = 4, col = "red", adj = 0.98)
    axis(3,
      at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))),
      labels = FALSE, tcl = 0.5, cex.axis = 4
    )

    ###...........................................................................
    # daily sums
    plot(as.numeric(strptime(prec.daily[, 1], format = "%Y-%m-%d")), prec.daily[, 2],
      t = "h",
      xlim = xxlim, xlab = "", ylab = "", xaxt = "n", cex.axis = 4, lwd = 4, col = "steelblue3", ylim = c(0, max(prec.daily[, 2]))
    )
    plot_maintenance(year_i)
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    # horizontal lines
    grid(nx = NA, ny = NULL, lty = 1)

    # add labels
    mtext(text = "daily sum (only flag 0)", side = 3, line = -5, cex = 4, col = "steelblue3")

    ###...........................................................................
    # monthly sums
    # number of days in run.year ==> see: https://r.789695.n4.nabble.com/Count-days-of-current-year-td875319.html
    # days.run.year <- as.numeric(format(as.Date(paste(run.year, "12", "31", sep = "-")), "%j"))
    # last.day.month <- as.numeric(as.yearmon(prec.monthly[, 1]) + 1 / 12) - 1 / days.run.year
    # get last day of the month of the time vector of the aggregation of the monthly sums
    # ==> convert Year-Month format from aggregation results to POSIXct, add one month (1 / 12) and substract 86400 seconds for one day
    last.day.month <- as.POSIXct(as.yearmon(strptime(paste(prec.monthly[, 1], "-01 00:00", sep = ""), format = "%Y-%m-%d %H:%M")) + 1 / 12) - 86400

    # plot(as.numeric(strptime(prec.monthly[, 1], format = "%Y-%m-%d")), prec.monthly[, 2], t = "h",
    plot(last.day.month, prec.monthly[, 2],
      t = "h",
      xlim = xxlim, xlab = "", ylab = "", xaxt = "n", cex.axis = 4, lwd = 7, col = "steelblue1", ylim = c(0, max(prec.monthly[, 2]))
    )
    plot_maintenance(year_i)
    # vertical lines
    abline(v = as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M")), col = "gray80")
    # horizontal lines
    grid(nx = NA, ny = NULL, lty = 1)

    # add labels
    mtext(text = "monthly sum (only flag 0)", side = 3, line = -5, cex = 4, col = "steelblue1")

    ###...........................................................................
    # total sum of complete year
    mtext(
      text = c(year_i, paste("total (only flag 0): ", round(sum(prec.zero, na.rm = TRUE), 0), sep = "")),
      side = 3, line = c(-5, -12), cex = 4, adj = 0.98, col = c("black", "turquoise4")
    )

    dev.off()


    ###...........................................................................
    # OLD plot
    pr <- length(aggregate(db.samet$prec ~ format(strptime(db.samet$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 2])
    precr <- matrix(ncol = 4, nrow = pr, 1)
    precr[, 1] <- aggregate(db.samet$prec ~ format(strptime(db.samet$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = mean)[, 1]
    precr[, 2] <- aggregate(db.samet$prec ~ format(strptime(db.samet$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = sum)[, 2]

    pr_zero <- which(as.numeric(db.samet.lvl1$prec_fl) == 0 & db.samet.lvl1$prec > 0)
    pr_zero_zero <- which(as.numeric(db.samet.lvl1$prec_fl) == 0 & db.samet.lvl1$prec == 0)
    pr_flags <- which(as.numeric(db.samet.lvl1$prec_fl) > 0)

    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_OLD_PREC_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$prec,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(0, 2.3), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(0, 1.5, 0.250)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (ll in seq(1.7, 2.2, 0.1)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")), 2, "hourly", las = 2, srt = 20, cex = 5, col = "gray80")
    text(as.numeric(strptime(lischt[8], format = "%Y-%m-%d %H:%M")), 1, "daily sum", las = 2, srt = 20, cex = 5, col = "gray80")
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(0, 1.5), col = "gray80")
    } # vertical lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(1.7, 2.3), col = "gray80")
    } # vertical lines

    points(as.numeric(strptime(db.samet.lvl1$UTC[pr_zero_zero], format = "%Y-%m-%d %H:%M")), 2.2 - (db.samet.lvl1$prec[pr_zero_zero] / 20),
      pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "gray80"
    )
    for (qq in pr_zero) { # real values in upper part
      lines(c(
        as.numeric(strptime(db.samet.lvl1$UTC[qq], format = "%Y-%m-%d %H:%M")),
        as.numeric(strptime(db.samet.lvl1$UTC[qq], format = "%Y-%m-%d %H:%M"))
      ),
      c(2.2, 2.2 - (db.samet.lvl1$prec[qq] / 20)),
      lwd = 2, cex.lab = 1.5, cex.axis = 1.7, col = "turquoise4"
      )
    }

    for (dd in 1:length(precr[, 1])) { # mean/sum values in bottom part
      lines(c(
        as.numeric(strptime(precr[dd, 1], format = "%Y-%m-%d")) + 43200,
        as.numeric(strptime(precr[dd, 1], format = "%Y-%m-%d")) + 43200
      ),
      c(1.5, (1.5 - as.numeric(precr[dd, 2]) / 20)),
      lwd = 3, cex.lab = 1.5, cex.axis = 1.7, col = "steelblue4"
      )
    }

    axis(2, at = seq(0, 1.5, 0.25), labels = rev(seq(0, 30, 5)), las = 2, cex.axis = 4)
    axis(2, at = seq(1.7, 2.2, 0.1), labels = rev(seq(0, 10, 2)), las = 2, cex.axis = 4)

    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(2.3, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 0.1, year_i, las = 2, cex = 6)
    dev.off()
    rm(pr_zero, pr_zero_zero, pr_flags)
  } # Precipitation (implemented, but crumpy data)

  if (zack == 1) {
    #  windspeed and direction ----------
    ###...........................................................................

    w_v_zero <- which(as.numeric(db.samet.lvl1$wind_v_300_fl) == 0)
    w_d_zero <- which(as.numeric(db.samet.lvl1$wind_deg_300_fl) == 0)
    w_v_flags <- which(as.numeric(db.samet.lvl1$wind_v_300_fl) > 0)
    w_d_flags <- which(as.numeric(db.samet.lvl1$wind_deg_300_fl) > 0)
    day <- paste0(substr(as.character(db.samet.lvl1$UTC), 6, 7), substr(as.character(db.samet.lvl1$UTC), 9, 10))

    wind.mean <- aggregate(db.samet.lvl1$wind_v_300 ~ day, FUN = mean, na.action = NULL)
    day.mitte <- db.samet.lvl1$UTC[substr(as.character(db.samet.lvl1$UTC), 12, 16) == "12:00"]
    weg <- data.frame(day.mitte, wind.mean)
    weg <- weg[complete.cases(weg[]), ]
    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_wind_", year_i, ".png", sep = ""), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$wind_v_300,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(0, 32), xlab = "Date", ylab = "speed[m/s]                        direction[deg]", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(0, 24, 2)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (ll in seq(26, 32, 1)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(0, 24.3), col = "gray80")
    } # vertical lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(25.7, 32), col = "gray80")
    } # vertical lines

    points(as.numeric(strptime(db.samet.lvl1$UTC[w_d_zero], format = "%Y-%m-%d %H:%M")), (db.samet.lvl1$wind_deg_300[w_d_zero] / 60) + 26,
      pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "darkblue"
    )

    points(as.numeric(strptime(db.samet.lvl1$UTC[w_v_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$wind_v_300[w_v_zero],
      pch = 20, cex.lab = 1.5,
      col = "green3"
    )
    points(as.numeric(strptime(weg$day.mitte, format = "%Y-%m-%d %H:%M")), weg$db.samet.lvl1.wind_v_300, col = "darkgreen", pch = 20, cex = 1.5)
    axis(2, at = seq(0, 24, 2), labels = seq(0, 24, 2), las = 2, cex.axis = 4)
    axis(2, at = seq(26, 32, 1), labels = seq(0, 360, 60), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(25, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 22, year_i, las = 2, cex = 6)
    dev.off()
    rm(w_v_zero, w_d_zero, w_v_flags, w_d_flags, day, wind.mean, day.mitte)

    #  windspeed and direction (2) windrose ----------
    ###...........................................................................

    db.wind <- db.samet[, c(1, 8, 9)] # 1,3,5,6
    db.wind <- db.wind[complete.cases(db.wind), ]




    png(paste(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_wrose_", year_i, ".png", sep = ""), width = p.width * 0.8, height = p.width * 0.8, pointsize = 8)
    par(mar = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
    wind.rose(wind.freq(db.wind$wind_v_300, db.wind$wind_deg_300), key = F, 6, 4, ang = -3 * pi / 16, main = "") # paste(year_i)
    legend("topright",
      cex = 3, title = "", lty = 1, lwd = 3, col = c("transparent"), y.intersp = 0.8,
      box.col = "white", inset = 0.05, seg.len = 0.8, c(paste(year_i)), bg = "transparent"
    )

    dev.off()
    rm(db.wind)
  } # Wind (implemented, but crumpy data)

  if (zack == 1) {

    #  humidity ----------
    ###...........................................................................


    hum_zero <- which(as.numeric(db.samet.lvl1$RH_200_fl) == 0)
    hum_flags <- which(as.numeric(db.samet.lvl1$RH_200_fl) > 0)
    hum2_zero <- which(as.numeric(db.samet.lvl1$RH_50_fl) == 0)
    hum2_flags <- which(as.numeric(db.samet.lvl1$RH_50_fl) > 0)


    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_RH_200_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$RH_200,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(0, 105), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(0, 100, 10)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(0, 100), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.samet.lvl1$UTC[hum_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$RH_200[hum_zero],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "aquamarine3"
    )
    axis(2, at = seq(0, 100, 10), labels = seq(0, 100, 10), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(103, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 7, year_i, las = 2, cex = 6)
    dev.off()

    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_RH_50_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$RH_50,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   # albedo from file
      xlim = xxlim, ylim = c(0, 105), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.axis = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(0, 100, 10)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(0, 100), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.samet.lvl1$UTC[hum2_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$RH_50[hum2_zero],
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "aquamarine3"
    )
    axis(2, at = seq(0, 100, 10), labels = seq(0, 100, 10), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(103, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 7, year_i, las = 2, cex = 6)
    dev.off()
  } # Humidity (implemented, but crumpy data)

  if (zack == 1) {
    #  soil temperature ----------
    # erst ab 2014 sinnvolle Werte!!!!!!
    ###...........................................................................

    # soil.cols<-colorRampPalette(c("seagreen4","palegreen3","yellow3","khaki","sandybrown","peru","mistyrose3","peachpuff4"))(150)
    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_Ts_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8) # ,A4, landscape)
    par(mar = c(1, 8, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Ts_1,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   #
      xlim = xxlim, ylim = c(-32, 22), xlab = " ", ylab = " ", xaxt = "n", yaxt = "n", type = "n", cex.lab = 3
    )
    plot_maintenance(year_i)
    for (ll in seq(-30, 20, 5)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-30, 20), col = "gray80")
    } # vertical lines

    points(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Ts_1,
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "palegreen3"
    )
    points(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Ts_3,
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "sandybrown"
    )
    points(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$Ts_6,
      pch = 20, cex.lab = 1.5, cex.axis = 1.7,
      col = "peachpuff4"
    )
    axis(2, at = seq(-30, 20, 5), labels = seq(-30, 20, 5), las = 2, cex.axis = 4)
    axis(3, at = c(as.numeric(strptime(lischt[-c(1, 13)], format = "%Y-%m-%d %H:%M"))), labels = c("", "", "", "", "", "", "", "", "", "", ""), las = 2, tcl = 0.5, cex.axis = 4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(22, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[11], format = "%Y-%m-%d %H:%M")) + 2000000, 14, year_i, las = 2, cex = 6)
    dev.off() # close pdf
  } # Soil temperature (implemented, but crumpy data)

  if (zack == 1) {
    #  water table ----------
    ###...........................................................................
    WT_zero <- which(db.samet.lvl1$WT_fl == 0)
    WT_eight <- which(db.samet.lvl1$WT_fl == 8)

    png(paste0(p.1$w[p.1$n == "plot.p"], year_i, "/SaMet2002_wt_", year_i, ".png"), width = p.width, height = p.height, pointsize = 8) # ,A4, landscape)
    par(mar = c(1, 5, 1, 1), omi = c(0, 0, 0, 0))
    plot(as.numeric(strptime(db.samet.lvl1$UTC, format = "%Y-%m-%d %H:%M")), db.samet.lvl1$WT,
      pch = 20, # cex.lab=1.7, cex.axis=1.5,   #
      xlim = xxlim, ylim = c(-0.1, 0.31), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n"
    )
    plot_maintenance(year_i)
    for (ll in seq(-0.2, 0.3, 0.05)) {
      abline(h = ll, col = "gray80")
    } # horizontal lines
    abline(h = 0, col = "gray90", lwd = 5) # horizontal lines
    for (pp in as.numeric(strptime(lischt, format = "%Y-%m-%d %H:%M"))) {
      lines(c(pp, pp), c(-20, 20), col = "gray80")
    } # vertical lines
    points(as.numeric(strptime(db.samet.lvl1$UTC[WT_zero], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$WT[WT_zero], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "royalblue4")
    points(as.numeric(strptime(db.samet.lvl1$UTC[WT_eight], format = "%Y-%m-%d %H:%M")), db.samet.lvl1$WT[WT_eight], pch = 20, cex.lab = 1.5, cex.axis = 1.7, col = "peru")

    axis(2, at = seq(-0.2, 0.3, 0.05), labels = seq(-0.2, 0.3, 0.05), las = 2, cex.axis = 4)
    # axis(3, at=c(as.numeric(strptime(lischt[-c(1,13)],format="%Y-%m-%d %H:%M"))),labels=c("","","","","","","","","","",""), las=2,tcl=0.5,cex.axis=4)
    text(as.numeric(strptime(lischt[-1], format = "%Y-%m-%d %H:%M")) - 1300000, rep(0.3, 12), labels = Months, las = 2, cex = 4)
    text(as.numeric(strptime(lischt[2], format = "%Y-%m-%d %H:%M")) + 2000000, -12, year_i, las = 2, cex = 6)
    dev.off() # close pdf
  } # water table (implemented)

  ## also missing: PA, vwc
  ###...........................................................................
  cat("#\n# level1 SaMet2002 ", year_i, " plot done!\n#\n")
}
