###############################################################################
#
#   LEVEL1 calculation of the volumetric water content based on Roth
#
#   copied in most parts from '1_master_calc_TDR_BaSoil2009' by Christian Budach
#   by: inge.gruenberg@awi.de
#
#   last modifications:
#     2020-10-29 CL replaced t.year with year_i
#     2020-09-24 CL replace difference plot with soil temperature plot in the overview plots of the compartments of the vwc calculation
#     2020-09-24 CL add theta_tot for to the overview plots of the compartments of the vwc calculation
#     2020-09-14 CL add the option to print overview plots of the compartments of the vwc calculation
#                   ==> this is to check whether the compartments of the vwc calculation correctly sum up to 100 %
#     2020-09-03 CL removed the "_sn" rows from the threshold files (to read theta_tot_prior) of BaSoil1998 and BaSoil2009. The old ones are stored in archive folders at N:/sparc/data/LTO/level2/Bayelva/VWC
#     2020-08-31 CL table vwc_calc_columns_TSoil+E2.csv with parameters Ts, E2, phi and theta_tot_prior for the vwc calculation introduced
#     2020-07-20 CL comments updated
#     2017-08-08
#
## definition of parameters used in the main function 'diel_to_vwc'
####
####
####
####
####
####
####
###
##
#

compute.vwc <- function(lv1.data, col.cat, station, years, mw.width, path.input, tab.vwc_calc) {
  # vwc_overview_plots: define whether overview plots of the compartments of the vwc calculation shall be plotted
  vwc_overview_plots <- TRUE

  # year_i: year to be processed
  # For the application of some functions using moving windows the week before and after the processed year is added in the Lv 0 to Lv 1 script.
  # Because of this, year_i is calculated here as the mean of all years rounded to zero digits (because there is no built in mode function in R).
  year_i <- round(mean(as.numeric(format(as.Date(lv1.data$UTC, format = '%Y-%m-%d %H:%M'),'%Y'))))
  # path for threshold table of the processed year
  path.output <- paste(path.input, year_i, ".dat", sep = "")
  # path of threshold table of the year before the processed year
  # ==> this is needed for setting the water content (theta_tot_prior) in step b) of the calculation
  path.input <- paste(path.input, year_i - 1, ".dat", sep = "")

  #
  #
  # !!! attention, the depth of the soil temperatures and dielectricities must be in the same order ==> Ts_depth1 ~ E2_depth1 !!!
  #

######################
### !!! this is not the case for BaSoil1998
### names(temp)
###  [1] "Ts_67_4"      "Ts_67_16"     "Ts_95_17"     "Ts_95_33"     "Ts_80_38"     "Ts_95_66"     "Ts_95_91"     "Ts_95_99"     "Ts_95_108"    "Ts_95_117_v"  "Ts_124_8"
###  [12] "Ts_124_25"    "Ts_115_60"    "Ts_145_6"     "Ts_145_24"    "Ts_145_40"    "Ts_145_62"    "Ts_151_76"    "Ts_145_99"    "Ts_135_112"   "Ts_135_125_v" "Ts_169_9"
###  [23] "Ts_177_19"    "Ts_185_52"    "Ts_204_5"     "Ts_205_13"    "Ts_205_31"    "Ts_210_50"    "Ts_205_61"    "Ts_205_90"    "Ts_200_114"   "Ts_200_118_v"
###  names(diel)
###  [1] "E2_80_1"      "E2_sn_67_0_v" "E2_90_16"     "E2_90_32"     "E2_85_39"     "E2_90_66"     "E2_90_90"     "E2_95_99"     "E2_95_113"    "E2_95_120_v"  "E2_114_6"
###  [12] "E2_115_22"    "E2_110_59"    "E2_140_6"     "E2_140_23"    "E2_140_39"    "E2_140_62"    "E2_146_76"    "E2_140_98"    "E2_135_112"   "E2_135_127_v" "E2_174_8"
###  [23] "E2_172_20"    "E2_180_54"    "E2_204_5"     "E2_200_13"    "E2_200_31"    "E2_205_51"    "E2_200_61"    "E2_200_90"    "E2_200_114"   "E2_200_121_v"
###
### BaSoil2009:  ==> check last column !!!
###  names(temp)
###   "Ts_1"   "Ts_11"  "Ts_21"  "Ts_37"  "Ts_55"  "Ts_71"  "Ts_89"  "Ts_141"
### names(diel)
### "E2_h_1"    "E2_h_11"   "E2_h_21"   "E2_h_37"   "E2_h_55"   "E2_h_71"   "E2_h_89"   "E2_sn_v_0"
###
### TVC2016: ok:
### names(temp)
###  [1] "Ts_2"  "Ts_5"  "Ts_10" "Ts_20"
###  names(diel)
###  [1] "E2_h_2"  "E2_h_5"  "E2_h_10" "E2_h_20"
###
### SaSoil2002 ok
###  > names(temp)
###  [1] "Ts_center_5"  "Ts_center_10" "Ts_center_20" "Ts_center_30" "Ts_center_40" "Ts_slope_7"   "Ts_slope_16"  "Ts_slope_22"  "Ts_slope_32"  "Ts_slope_42"  "Ts_rim_6"
###  [12] "Ts_rim_11"    "Ts_rim_16"    "Ts_rim_21"    "Ts_rim_27"    "Ts_rim_33"    "Ts_rim_38"    "Ts_rim_51"    "Ts_rim_61"    "Ts_rim_71"
###  > names(diel)
###  [1] "E2_center_8"  "E2_center_13" "E2_center_23" "E2_center_33" "E2_center_43" "E2_slope_5"   "E2_slope_14"  "E2_slope_23"  "E2_slope_33"  "E2_slope_43"  "E2_rim_5"
###  [12] "E2_rim_12"    "E2_rim_15"    "E2_rim_22"    "E2_rim_26"    "E2_rim_34"    "E2_rim_37"    "E2_rim_50"    "E2_rim_60"    "E2_rim_70"  ###
###
### SaSoil2012 ok, aber in LVo_to_LV1_SaAll.R auskommentiert ?? warum??
### names(temp)
###  "Ts_cen_15"  "Ts_cen_45"  "Ts_cen_72"  "Ts_cen_100" "Ts_rim_7"   "Ts_rim_22"  "Ts_rim_38"  "Ts_rim_52"  "Ts_rim_104"
###  names(diel)
###  "E2_cen_14"  "E2_cen_44"  "E2_cen_72"  "E2_cen_95"  "E2_rim_9"   "E2_rim_24"  "E2_rim_38"  "E2_rim_52"  "E2_rim_100"
######################

#   #
#   I <- col.cat[col.cat[,'Ts'] != 0, 'Ts']
#   I2 <- col.cat[col.cat[,'E2'] != 0, 'E2']
#   tmp <- col.cat[col.cat[,'E2sn'] != 0, 'E2sn']
#   I2 <- sort(c(I2, tmp))
#
# ##################################
# ##################################
# # *** For the re-ordering of the columns in the following section,
# # I suggest to change the selection of the columns from numerical values
# # to the names of the columns, like in the longer comment above.
# # This is easier readable to assure the correct assignment of
# # Ts and E2 sensors / columns of the same height. ***
# ##################################
# ##################################
#
#   if (length(I) > length(I2)) {
#     if (station == "SaSoil2002") {
#       # remove all upper Ts sensors and icewedge sensors
#       I <- I[-c(1, 7, 13, 24:33)]
#     } else if (station == "SaSoil2012") {
#       # selection of Ts sensors
#       I <- I[c(2, 4, 6, 8, 10, 11, 12, 13, 16)]
#       # selection of E2 sensors
#       I2 <- I2[c(4, 5, 6, 7, 9, 10, 11, 12, 13)]
#     } else {
#       I <- I[1:length(I2)]
#     }
#   }
#
#   if (length(I) < length(I2)) {
#     if (station == "TVCSoil2016") {
#       # remove all other Ts sensors
#       I <- col.cat[col.cat[, 'Ts'] != 0, 'Ts']
#       I2 <- I2[c(1:4)]
#     }
#   }

  ##############
  # NEW:
  # tab.vwc_calc: table with column names of soil temperature (Ts) and dielectricity (E2) for the the calculation of vwc
  # I: index of Ts columns of the selected station in lv1.data
  # I2: index of E2 columns of the selected station in lv1.data

  # this is implemented here with a loop to ensure that only entries of the same row
  # from the table tab.vwc_calc are assigned to each other

  ind.stat <- which(tab.vwc_calc$dataset == station)
  I <- I2 <- phi <- theta_tot_prior <- rep(NA, length(ind.stat))

  for (ind.ds in 1:length(ind.stat)) {
    I[ind.ds] <- which(names(lv1.data) == tab.vwc_calc$Ts[ind.stat[ind.ds]])
    I2[ind.ds] <- which(names(lv1.data) == tab.vwc_calc$E2[ind.stat[ind.ds]])
    #which(names(lv1.data) %in% tab.vwc_calc$E2[which(tab.vwc_calc$dataset == station)])

    # define parameters
    # a) station specific: porosity (phi) and volumetric water content (vwc) of the first year (theta_tot_prior) of the soil for each depth
    phi[ind.ds] <- tab.vwc_calc$phi[ind.stat[ind.ds]]
    #tab.vwc_calc$phi[which(tab.vwc_calc$dataset == station)]
    theta_tot_prior[ind.ds] <- tab.vwc_calc$theta_tot_prior[ind.stat[ind.ds]]
  }

  temp <- lv1.data[, I]
  diel <- lv1.data[, I2]

  # check:
  # names(temp)
  # names(diel)


###########
# VERSION 2: exclude the two following conditions

# the effect of the below two conditions is, that the vwc values are set to NA
# for all dates at which the soil temperature and / or the dielectricity
# of a certain depth was flagged with one of the flags between 1 and 6
# ==> that is all flags except flag 7, 8 and flag 100 (the later 0)
# This has the effect that the corresponding vwc values are set to NA
# and finally flagged with the smallest non-zero flag at the end of LV0_to_LV1_BaAll.R
# and not with flag 1 for "no data" !

  # remove data with flags between 1 and 6
  temp[(lv1.data[, I + 1] < 7) & (lv1.data[, I + 1] > 0)] <- NA
  diel[(lv1.data[, I2 + 1] < 7) & (lv1.data[, I2 + 1] > 0)] <- NA
####################

  # permittivities (dielectricities) without time column
  eps_bulk <- diel

  # add time
  temp <- data.frame(UTC = lv1.data$UTC, temp, stringsAsFactors = FALSE)
  diel <- data.frame(UTC = lv1.data$UTC, diel, stringsAsFactors = FALSE)

  # define parameters
  # a) station specific: porosity (phi) and volumetric water content (vwc) of the first year (theta_tot_prior) of the soil for each depth

  ##############
  # OLD
  # # add time
  # temp$UTC <- lv1.data$UTC
  # diel$UTC <- lv1.data$UTC
  #
  # # sort columns such, that first column contains time (UTC) and other columns the soil temperature, respectively the permittivities (dielectricities)
  # temp <- temp[, c(ncol(temp), 1:(ncol(temp) - 1))]
  # diel <- diel[, c(ncol(diel), 1:(ncol(diel) - 1))]

  # permittivities (dielectricities) without time column
  #eps_bulk <- diel[, -1]

  # define parameters
  # a) station specific: porosity (phi) and volumetric water content (vwc) of the first year (theta_tot_prior) of the soil for each depth
  #
  #
  #
#   if (station == 'BaSoil2009') {
#     # porosity of soil
#     phi <- c(0.5, 0.283, 0.434, 0.409, 0.420, 0.387, 0.439, 1)
#     # vwc values from year 2009 as 2008 has no data
#     theta_tot_prior <- c(0.012, 0.24, 0.22, 0.24, 0.26, 0.33, 0.36, 0.01)
#   } else if (station == 'BaSoil1998') {
#     # porosity of soil
#     phi <- c(0.288, 0.032, 0.334, 0.317, 0.272, 0.248, 0.224, 0.191, 0.211, 0.268,
#              0.233, 0.294, 0.355, 0.312, 0.275, 0.293, 0.330, 0.379, 0.334, 0.246,
#              0.260, 0.217, 0.219, 0.308, 0.378, 0.071, 0.225, 0.316, 0.373, 0.242,
#              0.258, 0.254)
#     # vwc values for 1998 (taken from first year that sensor was thawed in summer)
#     theta_tot_prior <- c(0.28, 0.01, 0.35, 0.32, 0.30, 0.30, 0.24, 0.26, 0.18, 0.21,
#                          0.32, 0.32, 0.36, 0.32, 0.30, 0.30, 0.33, 0.39, 0.33, 0.17,
#                          0.25, 0.31, 0.31, 0.33, 0.40, 0.05, 0.28, 0.36, 0.38, 0.25,
#                          0.22, 0.20)
#   } else if (station == 'SaSoil2002') {
#     # porosity of soil
#     phi <- c(0.98, 0.99, 0.78, 0.99, 0.99, 0.93, 0.93, 0.93, 1.00, 1.00, 0.67, 0.67,
#              0.67, 0.67, 0.73, 0.72, 0.63, 0.60, 0.55, 0.64)
#     # vwc values for 2002 (taken from first year that sensor was thawed in summer)
#     theta_tot_prior <- c(0.9337, 0.9358, 0.7070, 0.7665, 0.7751, 0.1975, 0.3694, 0.6835, 0.5776, 0.77,
#                          0.2697, 0.4855, 0.5736, 0.5793, 0.6578, 0.5337, 0.5839, 0.4447, 0.5168, 0.77)
#   } else if (station == 'TVCSoil2016') {
#     # porosity of soil
#     phi <- c(0.85, 0.455, 0.4346, 0.363)
#     # vwc values for 2016 (taken from first year that sensor was thawed in summer)
#     theta_tot_prior <- c(0.4, 0.25, 0.33, 0.42)
#   } else if (station == 'SaSoil2012') {
# ##############
# ## unklar was hier auskommentiert ist:
# ## check Stephan!
# ##############
#     #phi <- c(0.98, 0.99, 0.78, 0.99, 0.99, 0.93, 0.93, 0.93, 1.00)
#     #     ceter:         froz, froz,       rim:             froz, froz
#     # maximum in summer 2013
#     # porosity of soil
#     phi <- c(0.85, 0.62, 0.62, 0.99, 0.40, 0.52, 0.52, 0.52, 0.52)
#     # vwc values for 2012 (taken from first year that sensor was thawed in summer)
#     theta_tot_prior <- c(0.9337, 0.9358, 0.7070, 0.7665, 0.7751, 0.1975, 0.3694, 0.6835, 0.5776)
#   }
#
  # row_nr_freez_prior: row number when freezing period of the preceding year starts
  # ==> the begin of the freezing period is defined as last date with temperature >= 0.05 degree Celsius
  # 6300 is the default value, if the soil is frozen the whole year
#########
# WHY 6300?
# Ask Stephan
#########
  row_nr_freez_prior <- rep(6300, length(phi))

  # b) overwrite volumetric water content with water content from the previous year, if the processed year is not the first one
  if (year_i > years[1]) {
    # read vwc from previous year (the last year before the processed year)
    threshold_prior <- read.table(path.input, sep = ",", header = T)
    # overwrite vwc
    theta_tot_prior <- threshold_prior[, "theta_tot"]
  }

  # c) general parameters (constants)
  # dielectricity air
  eps_air <- 1
  # dielectricity ice
  eps_ice <- 1.96 * 1.96
  # dielectricity soil
  eps_soil <- 1.96 * 1.96
  # alpha: power for the calculation of theta ~> square root
  alpha <- 0.5

  # d) eps_liq_T
  # dielectricity liquid phase ==> water ==> dielectric constant of water at temperature temp calculated with function t.dep
  # ==> the dielectric constant is temperature dependent
  eps_liq_T <- as.data.frame(apply(temp[, -1], MARGIN = c(1, 2), FUN = t.dep))

  # main computation
  theta <- diel_to_vwc(eps_bulk, temp, theta_tot_prior, eps_air, eps_ice, eps_liq_T, eps_soil, phi, alpha, mw.width, row_nr_freez_prior)

  if (vwc_overview_plots == "TRUE") {

    pdf(file = paste(paste0(p.1$w[p.1$n == "plot.p"]), "OverviewPlots/vwc/", station, "/", station, "_", year_i, "_theta_ice+theta_liq.pdf", sep = ""), height = 10, width = 10)

    theta_UTC <- as.POSIXct(diel$UTC, origin = origin, tz = 'UTC', format = '%Y-%m-%d %H:%M')
    temp_UTC <- as.POSIXct(temp$UTC, origin = origin, tz = 'UTC', format = '%Y-%m-%d %H:%M')

    for (theta.i in 1:ncol(theta$theta_liq)) {
      # check.theta.calc: difference between theta_liq and the difference of theta_tot - theta_ice
      # ==> this should be equal ==> difference should be zero
      check.theta.calc <- theta$theta_liq[, theta.i] - (theta$theta_tot[, theta.i] - theta$theta_ice[, theta.i])

      if (sum(!is.na(check.theta.calc)) > 0) {
        par(mfrow = c(2, 1), oma = c(0, 0, 2, 0), mar = c(2, 4, 1, 8))

        ########
        # plot 1
        # difference between theta_liq and the difference of theta_tot - theta_ice
        # ==> this should be equal ==> difference should be zero
        # values deviating more than 0.0001 from zero are colored red
        #
        # plot(theta_UTC, check.theta.calc,
        #      #main = sub("E2_", "", names(diel))[theta.i + 1],
        #      ylab = "theta_liq - (theta_tot - theta_ice)", xlab = "", cex = 0.5)
        # grid()
        # points(theta_UTC[which(check.theta.calc > 0.0001)], check.theta.calc[which(check.theta.calc > 0.0001)],
        #        cex = 0.5, col = "red")
        # legend("topright", legend = c("Diff > 0.0001"), col = c("red"), pch = 1, inset = c(-0.185, 0), xpd = TRUE, xjust = 0)

        ########
        # plot 2
        # theta_ice and theta_liq
        # the theta_ice and theta_tot values corresponding with the red colored points of plot 1 are likewise colored in red
        plot(theta_UTC, theta$theta_ice[, theta.i],
             #main = sub("E2_", "", names(diel))[theta.i + 1],
             ylab = "theta", xlab = "", cex = 0.3,
             ylim = range(c(theta$theta_ice[, theta.i], theta$theta_liq[, theta.i], theta$theta_tot[, theta.i]), na.rm = TRUE))
        grid()
        points(theta_UTC, theta$theta_liq[, theta.i], cex = 0.3, col = "blue")
        points(theta_UTC, theta$theta_tot[, theta.i], cex = 0.3, col = "green")
        # red points
        points(theta_UTC[which(check.theta.calc > 0.0001)], theta$theta_ice[which(check.theta.calc > 0.0001), theta.i],
               cex = 0.5, col = "red")
        points(theta_UTC[which(check.theta.calc > 0.0001)], theta$theta_tot[which(check.theta.calc > 0.0001), theta.i],
               cex = 0.5, col = "red")

        legend("topright", legend = c("theta_ice", "theta_liq", "theta_tot", "Diff > 0.0001"), col = c("black", "blue", "green", "red"),
               pch = 1, inset = c(-0.185, 0), xpd = TRUE, xjust = 0)
        # legend("topright", legend = c("theta_ice", "theta_liq", "theta_tot"), col = c("black", "blue", "green"),
        #        pch = 1, inset = c(-0.15, 0), xpd = TRUE, xjust = 0)

        ########
        # plot 3
        # soil temperature ==> to check at which temperatures the issues occur
        # index for column ==> theta.i + 1, because first column is used as time column
        # the temperature values corresponding with the red colored points of plot 1 + 2 are likewise colored in red

        plot(temp_UTC, temp[, theta.i + 1],
             ylab = "Soil temperature", xlab = "", cex = 0.3)
        grid()

        # red points
        points(temp_UTC[which(check.theta.calc > 0.0001)], temp[which(check.theta.calc > 0.0001), theta.i + 1],
               cex = 0.5, col = "red")
      } else {
        plot(0, 0, t = "n", ylab = "", xlab = "", xaxt = "n", yaxt = "n")
        text(0, 0, labels = "NO DATA", cex = 3)
      }
    title(main = sub("E2_", "", names(diel))[theta.i + 1], outer = TRUE)
    }
    dev.off()
  }
######################
  # check:
  #  ==> how can it be, that there are theta_tot values smaller than the sum of theta_liq and theta_ice ??
  # !!
  # ==> this is because of the last additional condition in db_diel_to_vwc.R
  # where all theta_ice < 0 are set to 0 because ice contents cannot be negative
  # !!
  # bla <- apply(theta$theta_tot - theta$theta_liq - theta$theta_ice, MARGIN = 2, function(x) which(x < 0))
  # bla
  # bla1 <- apply(theta$theta_ice, MARGIN = 2, function(x) which(x == 0))
  # bla2 <- apply(theta$theta_ice_raw, MARGIN = 2, function(x) which(x < 0))
  # all.equal(bla, bla1)
  # all.equal(bla1, bla2)
  #
######################

  # sort output
  # first column: UTC (time), other columns: volumetric water content (vwc) of soil
  vwc <- data.frame(diel$UTC, round(theta$theta_liq, 3))
  # rename former dielectricity columns to "UTC" and "vwc" of the different depths
  colnames(vwc) <- c(names(diel)[1], sub("E2", "vwc", names(eps_bulk)))

  # check:
  # head(vwc)

#############
# remove next 2 lines which were commented out
# theta_ice <- data.frame(diel[,1], round(theta$theta_ice,3))
# colnames(theta_ice) <- c(names(diel)[1], sub("E2","ice",names(eps_bulk)))
#############

  # write threshold file of processed year for next computation
  threshold <- theta$threshold
  write.table(threshold, path.output, sep = ",", dec = ".", row.names = F)

  # replace previous vwc
  # select "vwc" columns from lv1.data
  I3 <- col.cat[col.cat[, 'vwc'] != 0, 'vwc']
  if (length(I3) > length(I2)) {
    I3 <- I3[1:length(I2)]
  }
  # replace the vwc columns of the lv1.data, but not the time column
  lv1.data[, I3] <- vwc[, 2:ncol(vwc)]



  # pass on the flags from Ts and E2 to the newly calculated vwc ==> the minimum flag is passed
  # ==> old vwc flags are overwritten
  ##########
  # ==> ATTENTION: This approach does work because the default flag is 100.
  # After all flagging routines are applied, the remaining flag 100 values are replaced with flag 0
  # (==> This takes place at the end of LV0_to_LV1_BaAll.R)
  ##########
  vwc.fl <- lv1.data[, I3 + 1]
  diel.fl <- lv1.data[, I2 + 1]
  temp.fl <- lv1.data[, I + 1]
  for (i in 1:(ncol(diel) - 1)) {
    vwc.fl[, i] <- apply(data.frame(diel.fl[, i], temp.fl[, i]), 1, FUN = min)
    #cat(vwc.fl[vwc.fl[,i]>1 & vwc.fl[,i]<100,i])
  }
  lv1.data[, I3 + 1] <- vwc.fl

# ????
# remove this part, which was commented out:
# ????

  # theta_ice <- data.frame(diel[,1], round(theta$theta_ice,3))
  # png(paste0(p.1$w[p.1$n=="LV2.p"], "/TVC/VWC/VWC_", year_i, ".png", sep = ""), width = 1280, height = 942, pointsize = 20)
  # par(mfrow = c(2,2), mar = c(1.5,1,2,2), oma = c(1.5,2.5,1,0), cex.axis = 0.9)
  #
  # for(i in 1:4){
  #   lim <- range(c(as.matrix(vwc[,-1]), theta_ice[-1], temp[,-1]/100), phi, finite = T)
  #   plot(y = vwc[,i+1], x = vwc[,1],
  #        ylim = lim,  type = "l", #xaxt = "n", #yaxt = "n",
  #        main = paste(names(temp)[i+1],year_i))
  #   lines(y = theta_ice[,i+1], x = vwc[,1], col = "blue")
  #   lines(y = temp[,i+1]/100, x = vwc[,1], col = "red")
  #   abline(h = phi[i], lty = "dotted")
  #   abline(h = 0, lty = "dashed", lwd = 0.8)
  #   abline(v = threshold$row_nr_freez[i])
  #   abline(h = threshold$theta_tot[i])
  # }
  # legend("topright", c("VWC", "Ice content", "Temperature [1/100 °C]", "porosity"),
  #        cex = 0.7, col = c("black", "blue", "red", "black"), lty = c(1,1,1,3), lwd = 1)
  #
  # dev.off()


  return(lv1.data)
}
