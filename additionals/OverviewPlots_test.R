###.............................................................................
##
##   Overview Plots       Pangaea Selection and Level1
##
##
##
##   by: christian.lehr@awi.de
##
## modifications:
##  2021-02-03 SL add linux path
##  2020-10-29 CL replaced t.year with year_i
##  2020-10-07 CL add special conditions for the early years of the Bayelva Pangaea "soil" dataset (in particular for variable "Ts_b")
##
###.............................................................................

#path <- read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt", sep = "\t", header = T)
###.............................................................................
# # this part is necessary to run this script seperate 
# rm(list=ls())
if (.Platform$OS.type == "windows") {
p.1 <<- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
p.1maint <<- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)

yearlyDatasetPaths <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv",
                               stringsAsFactors = FALSE, strip.white = TRUE)
allowedVariables   <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
                               stringsAsFactors = FALSE, strip.white = TRUE)
tmp.path <- "N:/sparc/LTO/R_database/database_plots/OverviewPlots/Tmp_png/"
filterbasepath     <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
checkbasepath      <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
}else{
  p.1<-read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  p.1maint<-read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt",sep="\t",header=T)
  yearlyDatasetPaths <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  allowedVariables   <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  tmp.path <- "/sparc/LTO/R_database/database_plots/OverviewPlots/Tmp_png/"
  filterbasepath     <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
  checkbasepath      <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
  
}
# ###.............................................................................
#station <<- "BaSnow2013"
#run.year <<-  2019
# for format of time axis
Sys.setlocale("LC_TIME", "C")

###.............................................................................
# print several png?s and merge them with "magick" in one pdf file
# http://www.cookbook-r.com/Graphs/Output_to_a_file/
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html
#
# because of the file size of the longterm plots (many data points in vector format pdf)
# and because the wikiplots are in png format already
###.............................................................................
# tmp.path <- "N:/sparc/LTO/R_database/database_plots/OverviewPlots/Tmp_png/"
# set resolution of png
ppi <- 300
## requires package magick
library(magick)

###.............................................................................
###.............................................................................
# set temp folder for png files of longterm plots
#tmp.path <- "N:/sparc/LTO/R_database/database_plots/OverviewPlots/Tmp_png/"

# the plotting procedure is relatively cumbersome. It can be help to set a local tmp folder to speed up the process and reduce errors.
###.............................................................................
###.............................................................................

###.............................................................................
#k=8
origin <- "1970-01-01"
  ###.............................................................................
  # vector of datasets
  dataset <- c("met", "soil", 
               "BaHole2009", "BaHole2015", "BaMet2009","BaSnow2013", "BaSoil2009", "BaSoil2017", 
               "TVCSoil2016",
               "SaMet2002","SaSnow2012","SaSoil2002")
  k <- which(dataset==station)
  # vector years for the longterm plots
  years <- yearlyDatasetPaths$year[yearlyDatasetPaths$dataset==station]
  # ATTENTION:
  # 1) for all datasets there has to be some years and
  # 2) datasets and years has to be in the same order
  ###.............................................................................
 
# set the year(s) to process
#run.year <- 2020

for (year_i in run.year) {
  # years <- list(2009:year_i, 2009:year_i, 2009:year_i, 2009:year_i, 2017:year_i, 2009:year_i, 2015:2019, 2013:year_i, 2016:2019)

  ############################
  # collect data from legend png?s
  path.legends <- paste(p.1$w[p.1$n == "plot.p"], "Legends", sep = "")
  # collect data from wikiplot png?s
  path.wiki.pngs <- paste(p.1$w[p.1$n == "plot.p"], year_i, "/", sep = "")
  # vector for selecting the corresponding wikiplot
  names.wikiplots <- read.table(paste0(p.1$w[p.1$n == "settings.p"],"wikiplots_variables.csv"), header = TRUE, sep = ",")
  # names.wikiplots
  
 
  ###.............................................................................
  # loop through k datasets
  #
  # k 1 and 2: Pangaea data sets
  # k > 2: level 1 data sets
  
 # for (k in c(8)) {
    
    # run the loop only if the running year is not later than the last year of dataset k
 #   if (max(years[[k]]) >= year_i) {
      
      # k: 1 and 2 Pangaea data sets
      if (k <= 2) {
        lv1.data <- read.table(paste(p.1$w[p.1$n == "LV2.p"], "Bayelva/ESSD_Pangaea/LV1/", station, "_lv1_", year_i, ".dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
        ############################
        # pdf path
        # pdf.path <- paste(p.1$w[p.1$n == "plot.p"], "OverviewPlots/Pangaea/", station, "_", year_i, ".pdf", sep = "")
        # pdf.path <- paste(tmp.path, "pdf/Pangaea/", station, "_", year_i, ".pdf", sep = "")
      }
      
      # k > 2 level 1 data sets
      if (k > 2) {
        lv1.data <- read.table(paste(p.1$w[p.1$n == "LV1.p"], station, "/00_full_dataset/", station, "_", year_i, "_lv1.dat", sep = ""),
                               sep = ",", dec = ".", header = T, fill = TRUE)
        ###.............................................................................
        # pdf path
        #
        #pdf.path <- paste(p.1$w[p.1$n == "plot.p"], "OverviewPlots/LV1/", station, "_", year_i, ".pdf", sep = "")
      }
      
      ###.............................................................................
      ###.............................................................................
      # pdf path
      pdf.path <- paste(p.1$w[p.1$n == "plot.p"], "OverviewPlots/LV1/", year_i,"/", station, "_", year_i, ".pdf", sep = "")
      # the plotting procedure is relatively cumbersome. It can be help to set a local tmp folder to speed up the process and reduce errors.
      # pdf.path <- paste(tmp.path, "pdf/", station, "_", year_i, ".pdf", sep = "")
      ###.............................................................................
      ###.............................................................................
      
      # use format "UTC" because this is not adjusted for summer / winter (standard) time
      lv1.data$UTC <- as.POSIXct(lv1.data$UTC, format = "%Y-%m-%d %H:%M", "UTC")
      
      # vector with number of columns formatted with leading zeros for the first 9 columns ("01", "02", ...)
      # for correct ordering in the merging of the png?s (img1234)
      # ==> see at the end of the loop
      n.col.f <- formatC(seq(1, ncol(lv1.data)), width = 2, flag = 0)
      
      ###.............................................................................
      # vector for labeling of y-axis with units
      # names(lv1.data)
      y.lab <- rep(NA, length(lv1.data))
      y.lab <- as.character(y.lab)
      y.lab[grep("Ts", names(lv1.data))] <- "?C"
      y.lab[grep("E2", names(lv1.data))] <- ""
      y.lab[grep("cond", names(lv1.data))] <- "S/m"
      y.lab[grep("vwc", names(lv1.data))] <- "m^3 / m^3"# "%"
      y.lab[grep("Tair", names(lv1.data))] <- "?C"
      y.lab[grep("Prec", names(lv1.data))] <- "mm"
      y.lab[grep("prec", names(lv1.data))] <- "mm"
      y.lab[grep("RH", names(lv1.data))] <- "%"
      y.lab[grep("RadNet", names(lv1.data))] <- "W/m^2"
      y.lab[grep("Sw", names(lv1.data))] <- "W/m^2"
      y.lab[grep("Lw", names(lv1.data))] <- "W/m^2"
      y.lab[grep("Vwind", names(lv1.data))] <- "m/s"
      y.lab[grep("Dirwind", names(lv1.data))] <- "deg"
      y.lab[grep("Dsn", names(lv1.data))] <- "m"
      y.lab[grep("G", names(lv1.data))] <- "W/m^2"
      # Pangaea
      y.lab[grep("Cond", names(lv1.data))] <- "S/m"
      y.lab[grep("Vwc", names(lv1.data))] <- "m^3 / m^3"# "%"
      # y.lab
      ###.............................................................................
      # var cats -----
      # vector of variable categories for assigning wikiplots
      lv1.var.cat <- rep(NA, length(lv1.data))
      lv1.var.cat <- as.character(lv1.var.cat)
      lv1.var.cat[grep("Ts", names(lv1.data))] <- "Ts"
      lv1.var.cat[grep("Ts_203", names(lv1.data))] <- "Ts_203"
      lv1.var.cat[grep("Ts_252", names(lv1.data))] <- "Ts_252"
      lv1.var.cat[grep("E2_sn", names(lv1.data))] <- "E2_sn"
      lv1.var.cat[grep("E2_h", names(lv1.data))] <- "E2_h"
      lv1.var.cat[grep("cond", names(lv1.data))] <- "cond"
      lv1.var.cat[grep("vwc", names(lv1.data))] <- "vwc"
      lv1.var.cat[grep("Tair_4", names(lv1.data))] <- "Tair_4"
      lv1.var.cat[grep("Tair_100", names(lv1.data))] <- "Tair_100"
      lv1.var.cat[grep("Tair_20", names(lv1.data))] <- "Tair_20"
      lv1.var.cat[grep("Tair_200", names(lv1.data))] <- "Tair_200"
      lv1.var.cat[grep("Tair_a_200", names(lv1.data))] <- "Tair1_200"
      lv1.var.cat[grep("Tair_b_200", names(lv1.data))] <- "Tair2_200"
      lv1.var.cat[grep("Tair_a_50", names(lv1.data))] <- "Tair1_50"
      lv1.var.cat[grep("Tair_b_50", names(lv1.data))] <- "Tair2_50"
      lv1.var.cat[grep("Prec", names(lv1.data))] <- "prec"
      lv1.var.cat[grep("prec", names(lv1.data))] <- "prec"
      lv1.var.cat[grep("RH", names(lv1.data))] <- "RH"
      lv1.var.cat[grep("RH_50", names(lv1.data))] <- "RH_50"
      lv1.var.cat[grep("RH_200", names(lv1.data))] <- "RH_200"
      lv1.var.cat[grep("RadNet", names(lv1.data))] <- "RadNet"#"rad_net"
      lv1.var.cat[grep("SwNet", names(lv1.data))] <- "SwNet"
      lv1.var.cat[grep("LwNet", names(lv1.data))] <- "LwNet"
      lv1.var.cat[grep("SwIn", names(lv1.data))] <- "SwIn"
      lv1.var.cat[grep("LwIn", names(lv1.data))] <- "LwIn"
      lv1.var.cat[grep("SwOut", names(lv1.data))] <- "SwOut"
      lv1.var.cat[grep("LwOut", names(lv1.data))] <- "LwOut"
      lv1.var.cat[grep("Albedo", names(lv1.data))] <- "Albedo"
      lv1.var.cat[grep("wind", names(lv1.data))] <- "wind"
      lv1.var.cat[grep("Dsn", names(lv1.data))] <- "Dsn"
      lv1.var.cat[grep("G", names(lv1.data))] <- "G"
      if (station == "SaSoil2002") {
        lv1.var.cat[grep("Tgs", names(lv1.data))] <- "Tgs"
        lv1.var.cat[grep("Ts_rim", names(lv1.data))] <- "Ts_rim"
        lv1.var.cat[grep("Ts_center", names(lv1.data))] <- "Ts_center"
        lv1.var.cat[grep("Ts_icewedge", names(lv1.data))] <- "Ts_icewedge"
        lv1.var.cat[grep("Ts_slope", names(lv1.data))] <- "Ts_slope"
        lv1.var.cat[grep("E2_rim", names(lv1.data))] <- "E2_rim"
        lv1.var.cat[grep("E2_center", names(lv1.data))] <- "E2_center"
        lv1.var.cat[grep("E2_slope", names(lv1.data))] <- "E2_slope"
        lv1.var.cat[grep("vwc_rim", names(lv1.data))] <- "vwc_rim"
        lv1.var.cat[grep("vwc_center", names(lv1.data))] <- "vwc_center"
        lv1.var.cat[grep("vwc_slope", names(lv1.data))] <- "vwc_slope"
        lv1.var.cat[grep("cond_rim", names(lv1.data))] <- "cond_rim"
        lv1.var.cat[grep("cond_center", names(lv1.data))] <- "cond_center"
        lv1.var.cat[grep("cond_slope", names(lv1.data))] <- "cond_slope"
      }
      if (station == "SaMet2002") {
        lv1.var.cat[grep("WT", names(lv1.data))] <- "WT"
      }
      
      # for TVC
      if (station == "TVCSoil2016") {
        lv1.var.cat[grep("vwc_h", names(lv1.data))] <- "vwc_h"
        lv1.var.cat[grep("vwc_v", names(lv1.data))] <- "vwc_v"
        lv1.var.cat[grep("vwc_a", names(lv1.data))] <- "vwc_v"
        lv1.var.cat[grep("vwc_b", names(lv1.data))] <- "vwc_v"
      }
      # special cases Pangaea met
      if (station == "met") {
        lv1.var.cat[grep("Tair_5", names(lv1.data))] <- "Tair_5"
        lv1.var.cat[grep("Dsn_a", names(lv1.data))] <- "Dsn_a"
        lv1.var.cat[grep("Dsn_b", names(lv1.data))] <- "Dsn_b"
      }
      # special cases Pangaea soil
      if (station == "soil") {
        lv1.var.cat[grep("Cond", names(lv1.data))] <- "Cond"
        lv1.var.cat[grep("E2", names(lv1.data))] <- "E2"
        lv1.var.cat[grep("Vwc", names(lv1.data))] <- "Vwc"
        lv1.var.cat[grep("Ts_a", names(lv1.data))] <- "Ts_a"
        lv1.var.cat[grep("Ts_b", names(lv1.data))] <- "Ts_b"
        lv1.var.cat[grep("Ts_c", names(lv1.data))] <- "Ts_c"
        lv1.var.cat[grep("Ts_d", names(lv1.data))] <- "Ts_d"
      }
      
      # vector with names for assignment of selection of data sets for long term plots of Pangaea "met"
      if (station == "met") {
        dummy.names.met <- names(lv1.data)
        dummy.names.met <- sub(pattern = "Prec", replacement = "prec", dummy.names.met)
        dummy.names.met <- sub(pattern = "Vwind_200", replacement = "wind_v_200", dummy.names.met)
        dummy.names.met <- sub(pattern = "Dirwind_sd_200", replacement = "wind_sddeg_200", dummy.names.met)
        dummy.names.met <- sub(pattern = "Dirwind_200", replacement = "wind_deg_200", dummy.names.met)
        dummy.names.met <- sub(pattern = "Dsn_a", replacement = "Dsn", dummy.names.met)
        dummy.names.met <- sub(pattern = "Dsn_b", replacement = "Dsn", dummy.names.met)
        dummy.names.met <- sub(pattern = "E2_sn", replacement = "E2_sn_v_0", dummy.names.met)
        dummy.names.met <- sub(pattern = "Tair_5", replacement = "Tair_4", dummy.names.met)
      }
      
      # vector with names for assignment of selection of data sets for long term plots of Pangaea "soil"
      if (station == "soil") {
        dummy.names.soil <- names(lv1.data)
        dummy.names.soil <- sub(pattern = "Ts_a", replacement = "Ts", dummy.names.soil)
        dummy.names.soil <- sub(pattern = "Ts_b", replacement = "Ts_203", dummy.names.soil)
        dummy.names.soil <- sub(pattern = "Ts_c", replacement = "Ts", dummy.names.soil)
        dummy.names.soil <- sub(pattern = "Ts_d", replacement = "Ts", dummy.names.soil)
        dummy.names.soil <- sub(pattern = "_c_", replacement = "_h_", dummy.names.soil)
        dummy.names.soil <- sub(pattern = "Cond", replacement = "cond", dummy.names.soil)
        dummy.names.soil <- sub(pattern = "Vwc", replacement = "vwc", dummy.names.soil)
      }
      ###.............................................................................
      # create 4 plots for each variable, all with range of y-axis set to level 0 limits:
      ###.............................................................................
      # excluded:
      # OLD: 1) All data without marking any flags,
      ###.............................................................................
      # 1) All data with flag 0 colored in grey and all other flags colored in red,
      # 2) Only data with quality flag 0,
      # 3) Long term plots of all available years of the variable,
      # 4) the plot from the wiki, which sometimes contains other related variables.
      ###.............................................................................
      ## loop over plots -----
      cat("wait for ",(ncol(lv1.data)-1)/2," variables:\n")
      for (i in seq(2, ncol(lv1.data), by = 2)) {
        cat("variable",i/2,lv1.var.cat[i],"\n")
        ###.............................................................................
        # index of values with flag 0
        ind <- which(lv1.data[, i + 1] == 0)
        # index of manual flags 3 and 6
        ind.36 <- which(lv1.data[, i + 1] %in% c(3, 6))
        
        # index of manual flags 2, 3 and 6
        #ind.236 <- which(lv1.data[, i + 1] %in% c(2, 3, 6))
        ###.............................................................................
        
        # # plot 1: all flags
        # # if there are no values in this year_i ==> print "NO VALUES",
        # # other than that plot the time series of the year_i
        # if (sum(!is.na(lv1.data[, i])) == 0) {
        #   plot(0, 0,
        #        t = "n", xlab = "", ylab = "",
        #        xaxt = "n", yaxt = "n",
        #        main = paste(names(lv1.data[i])))
        #   text(0, 0, "NO VALUES", cex = 2)
        # } else {
        #   plot(lv1.data$UTC, lv1.data[, i],
        #   t = "n", xlab = "", ylab = y.lab[i], ylim = c(lv1.data[ind, i]),
        #   main = paste(names(lv1.data[i])), xaxt = "n")
        #   time.ticks <- axis.POSIXct(side = 1, x = lv1.data$UTC, cex.lab = 1.5, cex.axis = 1.5, mgp = c(1.8, 0.7, 0))
        #   # time.ticks <- axis.POSIXct(side = 1, x = lv1.data$UTC, labels = FALSE)
        #   grid(nx = NA, ny = NULL, col = "lightgray", lty = 1, lwd = 0.2)
        #   abline(v = time.ticks, col = "lightgray", lty = 1, lwd = 0.2)
        #   abline(h = 0, col = "lightgray", lty = 1, lwd = 2)
        #   points(lv1.data$UTC, lv1.data[, i], pch = 1, cex = 0.5)
        # }
        
        ###.............................................................................
        # plot 1: All data with flag 0 colored in grey and all other flags colored in red ----
        # if there are no values with flag = 0 in this year_i ==> print "NO VALUES",
        # other than that plot the time series of the year_i
        cat("plot 1\n")
        png(file = paste(tmp.path, "p1-", i, ".png", sep = ""),
            width = 2*6*ppi, height = 6*ppi, res = ppi)
        
        par(mar = c(2.2, 3.2, 2, 1), mgp = c(1.8, 0.5, 0), tck = -0.01,
            cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
        
        if (sum(!is.na(lv1.data[, i])) == 0) {
          plot(0, 0,
               t = "n", xlab = "", ylab = "",
               xaxt = "n", yaxt = "n",
               main = paste(names(lv1.data[i])))
          text(0, 0, "NO VALUES", cex = 2)
        } else {
          # check if enough values with flag 0 exist to use the flag 0 values for the scaling of the y-axis
          # if not perform the scaling for all values of variable i
          if (length(ind) <= 2) {
            y.lim <- range(lv1.data[, i], na.rm = TRUE)
          } else {
            y.lim <- range(lv1.data[ind, i], na.rm = TRUE)
          }
          plot(lv1.data$UTC, lv1.data[, i],
               t = "n", xlab = "", ylab = y.lab[i], ylim = y.lim,
               main = paste(names(lv1.data[i]), "- flags 3 and 6 in red, all other flags > 0 in blue"), xaxt = "n")
          time.ticks <- axis.POSIXct(side = 1, x = lv1.data$UTC, cex.lab = 1.5, cex.axis = 1.5, mgp = c(1.8, 0.7, 0))
          # time.ticks <- axis.POSIXct(side = 1, x = lv1.data$UTC, labels = FALSE)
          grid(nx = NA, ny = NULL, col = "lightgray", lty = 1, lwd = 0.2)
          abline(v = time.ticks, col = "lightgray", lty = 1, lwd = 0.2)
          abline(h = 0, col = "lightgray", lty = 1, lwd = 2)
          points(lv1.data$UTC[ind], lv1.data[ind, i], pch = 1, cex = 0.5, col = "grey")
          points(lv1.data$UTC[-ind], lv1.data[-ind, i], pch = 1, cex = 0.5, col = "blue", lwd = 2)
          points(lv1.data$UTC[ind.36], lv1.data[ind.36, i], pch = 1, cex = 0.5, col = "red", lwd = 2)
        }
        
        dev.off()
        
        # read png for merging later
        pngs <- list.files(path = tmp.path, pattern = paste("p1-", i, ".png", sep = ""), full.names = TRUE)
        img1 <- image_read(pngs)
        ###.............................................................................
        # delete all plot 1 png from tmp folder for next dataset
        file.remove(pngs)
        
        ###.............................................................................
        # plot 2: Only data with quality flag 0, ----
        # if there are no values with flag = 0 in this year_i ==> print "NO VALUES",
        # other than that plot the time series of the year_i
        cat("plot 2\n")
        png(file = paste(tmp.path, "p2-", i, ".png", sep = ""),
            width = 2*6*ppi, height = 6*ppi, res = ppi)
        
        par(mar = c(2.2, 3.2, 2, 1), mgp = c(1.8, 0.5, 0), tck = -0.01,
            cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
        
        if (sum(!is.na(lv1.data[ind, i])) == 0) {
          plot(0, 0,
               t = "n", xlab = "", ylab = "",
               xaxt = "n", yaxt = "n",
               main = paste(names(lv1.data[i])))
          text(0, 0, "NO VALUES", cex = 2)
        } else {
          plot(lv1.data$UTC[ind], lv1.data[ind, i],
               t = "n", xlab = "", ylab = y.lab[i],
               main = paste(names(lv1.data[i]), "- only flag 0"), xaxt = "n",
               xlim = range(lv1.data$UTC))
          time.ticks <- axis.POSIXct(side = 1, x = lv1.data$UTC, cex.lab = 1.5, cex.axis = 1.5, mgp = c(1.8, 0.7, 0))
          #time.ticks <- axis.POSIXct(side = 1, x = lv1.data$UTC, labels = FALSE)
          grid(nx = NA, ny = NULL, col = "lightgray", lty = 1, lwd = 0.2)
          abline(v = time.ticks, col = "lightgray", lty = 1, lwd = 0.2)
          abline(h = 0, col = "lightgray", lty = 1, lwd = 2)
          points(lv1.data$UTC[ind], lv1.data[ind, i], pch = 1, cex = 0.5)
        }
        dev.off()
        
        # read png for merging later
        pngs <- list.files(path = tmp.path, pattern = paste("p2-", i, ".png", sep = ""), full.names = TRUE)
        img2 <- image_read(pngs)
        ###.............................................................................
        # delete all plot 2 png from tmp folder for next dataset
        file.remove(pngs)
        
        ###.............................................................................
        # plot 3: Long term plots of all available years of the variable ----
        ###.............................................................................
        
        
        
        
        ## ATTENTION: this requires that the columns are always in the same order!!
        ## in the level 1 data sets, the order of the columns of a data set is consistent throughout the years
        ## The Pangaea data sets are a selection of variables from different level 1 data sets
        ## because of this a new index ii is introduced.
        ## ==> for the Pangaea data sets every variable i has to be related to the corresponding variable ii of the correct level 1 data set
        ## ==> for the level 1 data ii is the same index i
        
        
        
        ###.............................................................................
        # for Pangaea selection data set
        # select row index of dataset k and variable i in names.wikiplots table
        if (lv1.var.cat[i] %in% c("Tair_20", "Tair_200")) {
          ind.wiki.plots <- intersect(which(names.wikiplots$dataset == station), which(names.wikiplots$variable_category == lv1.var.cat[i]))
        } else {
          ind.wiki.plots <- intersect(which(names.wikiplots$dataset == station), grep(lv1.var.cat[i], names.wikiplots$variable_category))
        }
        
        # special condition for Pangaea dataset "soil" because
        # "Ts_b" is from "BaMet1998" between 1998 to 2009,
        # and from "BaMet2009" between 2009 to 2018
        if ((year_i <= 2009) & (station == "soil") & (lv1.var.cat[i] == "Ts_b")) {
          ind.wiki.plots <- intersect(intersect(which(names.wikiplots$dataset == station),
                                                which(names.wikiplots$sparc_dataset == "BaMet1998")),
                                      grep(lv1.var.cat[i], names.wikiplots$variable_category))
        }
        
        if ((year_i > 2009) & (station == "soil") & (lv1.var.cat[i] == "Ts_b")) {
          ind.wiki.plots <- intersect(intersect(which(names.wikiplots$dataset == station),
                                                which(names.wikiplots$sparc_dataset == "BaMet2009")),
                                      grep(lv1.var.cat[i], names.wikiplots$variable_category))
        }
        
        ###.............................................................................
        # 1) select the related sparc data set
        # 2) select the number of available years of this data set
        #
        # for k <= 2 ==> Pangaea data sets:
        if (k <= 2) {
          sparc.dataset <- as.character(names.wikiplots$sparc_dataset[ind.wiki.plots])
          all.years <- years[[which(dataset == sparc.dataset)]]
        }
        
        # # for k > 2 ==> sparc data sets:
        # if (k > 2) {
          sparc.dataset <- station
         # all.years <- years[[k]]
          all.years <- years
        # } 
        # special condition for Pangaea dataset "soil" because
        # "Ts_b" is from "BaMet1998" between 1998 to 2009,
        # and from "BaMet2009" between 2009 to 2018
        if ((station == "soil") & (sparc.dataset == "BaMet2009")) {
          all.years <- 2010:year_i
        }
        
        
        ###.............................................................................
        
        ###.............................................................................
        # loop to read all years of the sparc data set,
        # concatenate them in one data frame, and
        # 3) set the correct colum variable ii for the long term plot
        
        for (j in 1:length(all.years)) {
          year <- all.years[j]#
          
          # read year j of the selected sparc dataset
          dummy <- read.table(paste(p.1$w[p.1$n == "LV1.p"], sparc.dataset, "/00_full_dataset/", sparc.dataset, "_", year, "_lv1.dat", sep = ""),
                              sep = ",", dec = ".", header = T, fill = TRUE)
          
          # 3) set index ii
          # for Pangaea data sets:
          # k == 1: "met"
          if (k == 1) {
            
            ii <- which(names(dummy) == dummy.names.met[i])
            
          }
          
          # k == 2: "soil"
          if (k == 2) {
            ii <- which(names(dummy) == dummy.names.soil[i])
          }
          
          # for k > 2 ==> sparc data sets:
          if (k > 2) {
            ii <- i
          }
          
          if (ii != 0) {
            dummy$UTC <- as.POSIXct(dummy$UTC, format = "%Y-%m-%d %H:%M", "UTC")
            # in the first year copy the table to dummy2
            # in all other years concatenate the tables below dummy2
            if (j == 1) {
              dummy2 <- dummy
            } else {
              dummy2 <- rbind(dummy2, dummy)
            }
          }
        }
        
        # index of data points with flag 0 for the long term plot
        if (ii != 0) {
          long.lv1.data <- dummy2
          ind.long <- which(long.lv1.data[, ii + 1] == 0)
        }
        
        if(colnames(lv1.data[i])=="prec"){
          prec.zero <- dummy2$prec
          prec.zero[which(as.numeric(dummy2$prec_fl) > 0)] <- NA
          prec.daily <- aggregate(prec.zero ~ format(strptime(dummy2$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), FUN = sum)
          prec.monthly <- aggregate(prec.zero ~ format(strptime(dummy2$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y-%m"), FUN = sum)
          prec.yearly <- aggregate(prec.zero ~ format(strptime(dummy2$UTC, format = "%Y-%m-%d %H:%M"), format = "%Y"), FUN = sum)
          prec.yearly[,1]<-paste0(prec.yearly[,1],"-07-01")
          prec.monthly[,1]<-paste0(prec.monthly[,1],"-15")
        }
          cat("plot 3\n")
        png(file = paste(tmp.path, "p3-", i, ".png", sep = ""),
            width = 2*6*ppi, height = 6*ppi, res = ppi)
        
        par(mar = c(2.2, 3.2, 2, 1), mgp = c(1.8, 0.5, 0), tck = -0.01,
            cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
        
        # if there is no long term data OR
        # if there are no values with flag = 0 in this year_i ==> print "NO VALUES",
        # other than that plot the time series of the year_i
        ###.............................................................................
        # ATTENTION: use y.lab[i] because this is still the loop of variable i
        # ==> ii is just the index for the long data set, that is all available years from the level 1 sparc data sets,
        # which might be different for the Pangaea data sets
        ###.............................................................................
        if ((ii == 0) | (sum(!is.na(long.lv1.data[ind.long, ii])) == 0)) {
          plot(0, 0,
               t = "n", xlab = "", ylab = "",
               xaxt = "n", yaxt = "n",
               main = paste(names(lv1.data[i])))
          text(0, 0, "NO VALUES", cex = 2)
        } else {
          plot(long.lv1.data$UTC[ind.long], long.lv1.data[ind.long, ii],
               t = "n", xlab = "", ylab = y.lab[i],
               main = paste(names(long.lv1.data[ii]), "- longterm only flag 0"), xaxt = "n",
               xlim = range(long.lv1.data$UTC))
          if(colnames(lv1.data[i])=="prec"){
            points(strptime(prec.yearly[, 1], format = "%Y-%m-%d"), prec.yearly[, 2]/50, t = "h", lwd = 20, col = "#e4e4e4")
            points(strptime(prec.monthly[, 1], format = "%Y-%m-%d"), prec.monthly[, 2]/20, t = "h", lwd = 6, col = "#ffd6da")
            points(strptime(dummy2$UTC, format = "%Y-%m-%d %H:%M"), prec.zero, t = "h", lwd = 2, col = "steelblue4")
            text(strptime(prec.yearly[, 1], format = "%Y"), rep(11, length(prec.yearly[, 1])), labels = prec.yearly[, 2], las = 2, cex = 1,srt=45)
            #axis(2, at = seq(0,10,2), labels = seq(0, 20, 4), las = 1, cex.axis = 1.5,tcl = -1, col.axis = "#ffaeb6")
            
          }
          time.ticks <- axis.POSIXct(side = 1, x = long.lv1.data$UTC,
                                     cex.lab = 1.5, cex.axis = 1.5, mgp = c(1.8, 0.7, 0))
          #time.ticks <- axis.POSIXct(side = 1, x = long.lv1.data$UTC, labels = FALSE)
          grid(nx = NA, ny = NULL, col = "lightgray", lty = 1, lwd = 0.2)
          #abline(v = time.ticks, col = "lightgray", lty = 1, lwd = 0.2)
          abline(v = seq(from = long.lv1.data$UTC[1], to = long.lv1.data$UTC[length(long.lv1.data$UTC)], by = "year"),
                 col = "lightgray", lty = 1, lwd = 0.2)
          abline(v = c(long.lv1.data$UTC[which(paste0(run.year,"-01-01 01:00:00 UTC")==long.lv1.data$UTC)],
                       long.lv1.data$UTC[which(paste0(run.year+1,"-01-01 01:00:00 UTC")==long.lv1.data$UTC)]),
                 col = "red", lty = 3, lwd = 1.6)
          
          abline(h = 0, col = "lightgray", lty = 1, lwd = 2)
          
          points(long.lv1.data$UTC[ind.long], long.lv1.data[ind.long, ii], pch = 1, cex = 0.2, lwd = 0.2)
          box()
        }
        dev.off()
        
        
        # read png for merging later
        pngs <- list.files(path = tmp.path, pattern = paste("p3-", i, ".png", sep = ""), full.names = TRUE)
        img3 <- image_read(pngs)
        ###.............................................................................
        # delete all plot 3 png from tmp folder for next dataset
        file.remove(pngs)
        
        # remove index ii for next dataset
        rm(ii)
        
        ###.............................................................................
        # plot 4: the plot from the wiki, which sometimes contains other related variables.
        ###.............................................................................
        # selection now already before plot 3, because of Pangaea plots
        # select row index of dataset k and variable i in names.wikiplots table
        # ind.wiki.plots <- intersect(which(names.wikiplots$dataset == station), grep(names(lv1.data)[i], names.wikiplots$variable_category))
        # ind.wiki.plots <- intersect(which(names.wikiplots$dataset == station), grep(lv1.var.cat[i], names.wikiplots$variable_category))
        ###.............................................................................
        # plot 4: merge the wikiplots with their legend and print as png -----
        # and merge the complete wikiplot with plot 3
        # if no wikiplot is existing, omit plot 4
        cat("plot 4\n")
        if (length(ind.wiki.plots) > 0) {
          
          legend.files <- list.files(path = path.legends, pattern = as.character(paste(names.wikiplots$legend_name[ind.wiki.plots], ".png", sep = "")), full.names = TRUE)
          #legend.names <- list.files(path = path.legends, pattern = station)
          #legend.pattern <- as.character(strsplit(x = legend.names, split = ".png"))
          legend2 <- image_read(legend.files)
          
          png.files <- list.files(path = path.wiki.pngs, pattern = as.character(names.wikiplots$wikiplot_name[ind.wiki.plots]), full.names = TRUE)
          #png.names <- list.files(path = path.wiki.pngs, pattern = station)
          #png.pattern <- as.character(strsplit(x = png.names, split = paste("_", year_i, ".png", sep = "")))
          pngs2 <- image_read(png.files)
          
          img4 <- c(pngs2, legend2)
          img4 <- image_append(img4)
          #image_write(img, path = paste(tmp.path, png.pattern[i], ".png", sep = ""), format = "png")
          img4 <- image_scale(img4, "3600x1800")
          img34 <- image_append(c(img3, img4))
        } else {
          img34 <- img3
        }
        
        # merge plots 1 to 2
        img12 <- image_append(c(img1, img2))
        
        # write plots 1 to 4 (or plots 1 to 3, if not plot 4 (wikiplot) exists) and write as one png
        img1234 <- image_append(c(img12, img34), stack = TRUE)
        # use vector with number of first 9 columns formated with leading zeros ("01", "02", ...) to achieve correct order of the png (img1234)
        image_write(img1234, path = paste(tmp.path, "1to4_p", n.col.f[i], ".png", sep = ""), format = "png")
        
      }
      # read collection of all 1 to 4 png
      # and merge all the plots of dataset k in one pdf
      pngs <- list.files(path = tmp.path, pattern = "1to4_p", full.names = TRUE)
      pngs2 <- image_read(pngs)
      cat("write pdf\n")
      image_write(pngs2, path = pdf.path, format = "pdf")
      
      ###.............................................................................
      # delete all png?s from tmp folder for next dataset
      file.remove(pngs)
      ###.............................................................................
      cat("#\n# overview plot ", station,": ", year_i, " done!\n#\n")
    }
#  }
  
#}
