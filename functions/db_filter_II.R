###############################################################################
#
#   LEVEL1 filter/flag functions
#
#   written by: stephan.lange@awi.de
#
#   last check: 2020-02-06
#   checked by: christian.lehr@awi.de
#
#############################################################################
#   last modified:
#   2020-10-29 by CL: replaced t.year with year_i (this is the global variable used in LV0_to_LV1 Scripts)
#                     and define year_p as running variables used only in this script
#   2020-09-28 by CL: special case in the flag 5 routine that for "BaSoil1998", "BaSoil2009" and "BaHole2009" all variables shall be checked and flagged
#                     ==> this is necessary to achieve consistent flagging with the Pangaea "Basoil" data publications
#   2020-09-24 by CL: new zero curtain period table for TVC created and implemented
#   2020-08-19 by CL: variable time.res removed in function detect.peaks because it is not used
#   2020-08-19 by CL: change order of flag column in function frozen.water.table
#   2020-08-18 by CL: phys.limit_ba of "rho_K" (snow density calculated from "SWE_K" and "Dsn") updated
#   2020-08-18 by CL: exclude category "rho" from gradient flagging (flag 5), because it inherits its flags from "SWE_K" and "Dsn"
#   2020-08-18 by CL: "TL" and "KTL" changed to "Tl" and "KTl" (Tl: Thallium)
#   2020-07-28 by CL: flagging of "RH50" and "RH200" with flag 6 in dataset "SaMet2002" for all air temperature values
#                     of the respective height with flag 4 ("TAir 50", respectively "TAir200") moved here from LV0_to_LV1_SaAll.R
#   2020-06-12 by CL: flag 6: 2 rule based flaggings transferred to manual flaggings (filter files)
#                     Dataset "BaMet1998":
#                     a) All zero degree values of wind direction in the period from 2008-09-01 00:00 to 2008-10-31 23:00.
#                     Periods of zero degree wind direction are defined as all values with "wind_deg_200" = 0 plus the next value of such a zero value period.
#                     b) All relative humidity values < 42 in the year 2008. ==> this was already done by SL in 2017-08-02.
#
#   2020-03-19 by CL: option encoding = "ISO-8859-1" added in read.table for reading of the tables because of special sign for degree Celsius "°C"
#   2020-03-10 by CL: tables for physical limits, peak parameter and period of zero curtain outsourced
#   2020-02-10 by CL: selection for sensors with "possibility of snow or dirt on Radiation Sensor" simplified
#   2020-02-05 by SL, NB, CL: upper physical limit of Bayelva Dsn set to < 1.4 m
#   2019-03-28 by Anne: physical limits for Q and Tw in physical.limits.ku
#   2018-09-09 by Peter: there was no Filter for NetRad!!
#   2018-09-09 by Peter: flag 6 for all Radiation Parameters if SwIn < SwOut by min. -10 W/mÂ²
#   2018-07-10 by Peter.Schreiber@awi.de: other limits for Tair samoylov
#
#
#############################################################################
##
## open issues:
##
#############################################################################
##
## Comments:
#############################################################################
##
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


## ==============================================================================
##
## flag == 2    (System error)
##
add.systemerror <- function(lv1.data, station) {

  # Additional errors that can only be identified based on other variables:
  #
  # Special cases
  #
  # a)
  # Dsn SQ ==> Signal Quality
  # If signal Quality = 0 set flag 2
  if (station == "SaSoil2012") {
    lv1.data$Dsn_fl[which(lv1.data$SQsn == 0)] <- 2
  }

  # b)
  # System error of temperature sensors in years 2000 and 2001 is indicated by air temperature in 200 cm height (Tair_200) above -3°C.
  # Erratic low values confirm that most measurements of all sensors are really affected,
  # including the PT100 air temperatures, but only when the temperature (most likely the datalogger temperature) exceeds a threshold of roughly -3 degree C.
  # This concerns the PT100 sensors in soil profiles 252 and 203 and in air temperatures 20, 35, 48, 100.
  # All PT100 sensors exhibited loose screws between 2000-04-26 and 2001-11-12.
  # For security, one day is added as buffer to the "loose screws period" and
  # the values from all affected sensors are flagged between 2000-04-25 and 2001-11-13.
  # Affected sensors:
  # Ts_252_2, Ts_252_12, Ts_252_32, Ts_252_62, Ts_252_102, Ts_252_152, Ts_203_2, Ts_203_5, Ts_203_23, Ts_203_53, Ts_203_93, Ts_203_143
  # Tair_20, Tair_35, Tair_48, Tair_100
  if (station == 'BaMet1998') {
    cols <- colnames(lv1.data)
    year_p <- round(mean(as.numeric(format(as.Date(lv1.data$UTC, format = '%Y-%m-%d %H:%M'), '%Y'))))
    # several columns with "Ts_252" and "Ts203"
    I <- c(which(grepl('Ts_252', cols) & grepl('_fl', cols)), which(grepl('Ts_203', cols) & grepl('_fl', cols)),
           which(cols == 'Tair_20_fl'), which(cols == 'Tair_35_fl'), which(cols == 'Tair_48_fl'), which(cols == 'Tair_100_fl'))

    if (year_p == 2000) {
      # select days warmer than -3 degree C
      Ipot <- which(lv1.data$Tair_200 > (-3))
      # remove warmer days after 25 April 2000 (one day earlier than loose screws period for security)
      tmp1 <- as.POSIXct('2000-04-25 00:00', origin = options()$origin, format = '%Y-%m-%d %H:%M')
      tmp2 <- which(lv1.data$UTC >= tmp1)[1]
      Ipot <- Ipot[which(Ipot >= tmp2)]
      # flag erroneous values
      lv1.data[Ipot, I] <- 2
    } else if (year_p == 2001) {
      # select days warmer than -3 degree C (one day after loose screws period for security)
      Ipot <- which(lv1.data$Tair_200 > (-3))
      # remove those days before 13 Nov 2001
      tmp1 <- as.POSIXct('2001-11-13 00:00', origin = options()$origin, format = '%Y-%m-%d %H:%M')
      tmp2 <- which(lv1.data$UTC >= tmp1)[1]
      Ipot <- Ipot[which(Ipot <= tmp2)]
      # flag erroneous values
      lv1.data[Ipot, I] <- 2
    }
  }

#################
# ==> exclude c) for Pangaea Publication:
#################

  # c)
  # Set flag 2 for KuLucky22014: Tw, P, WT and Wtch for the period from 2015-09-13 till 2016-07-02
  # The sensor is supposed to measure in a lake. During that time the sensor was not in the water and measure air temperature.
  if (station == 'KuLucky22014') {
    lv1.data$Tw_unknown_fl[which(('2015-09-13' < lv1.data$UTC) & (lv1.data$UTC < '2016-07-22'))] <- 2
    lv1.data$P_fl[which(('2015-09-13' < lv1.data$UTC) & (lv1.data$UTC < '2016-07-22'))] <- 2
    lv1.data$WT_fl[which(('2015-09-13' < lv1.data$UTC) & (lv1.data$UTC < '2016-07-22'))] <- 2
    lv1.data$WTch_fl[which(('2015-09-13' < lv1.data$UTC) & (lv1.data$UTC < '2016-07-22'))] <- 2
  }

  return(lv1.data)
}

## ==============================================================================
##
## flag == 3     (Maintenance)
##

# Maintenance flags are set for maintenance periods.
# The maintenance periods are read in the level 0 to level 1 script separately for each station from a text file.


## ==============================================================================
##
## flag == 4     (Physical limits)
##

# The physical limits are defined for categories of variables separately for each station based on "hard" physical limits, information from technical manuals and experience.
# They are read separately for each station from a text file.
#
# In the pool of all the datasets the following categories of variables are available:
# 'Tair', 'prec', 'RH', 'Dsn', 'RadNet', 'SwIn', 'SwOut', 'LwIn', 'LwOut', 'windv', 'winddeg', 'windsddeg',
# 'Ts', 'vwc', 'cond', 'E2', 'E2sn', 'G', 'SwNet', 'LwNet', 'Albedo', 'distcor', 'CountsK', 'CountsTl', 'KTl', 'SWE', 'Tcryst', 'rho'
# (see level 0 to level 1 Script for the definition of the categories).

########
## physical limits Bayelva
physical.limits.ba <- function(lv1.data, col.cat) {

  cats <- colnames(col.cat)
  # Define physical limits for each category
  phys.limit <- t(read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Ba.csv"),
                             sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("min", "max")])
  colnames(phys.limit) <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Ba.csv"),
                                     sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("category")]
  # for each category
  for (i in 1:(length(cats))) {
    if (col.cat[1, cats[i]] > 0) {
      # for each dataseries within this category
      for (j in which(col.cat[, cats[i]] != 0)) {
        # flag data with '4' if it is above the maximum OR below the minimum physical limit
        ind.pl <- which( (lv1.data[, col.cat[j, cats[i]]] > phys.limit[2, cats[i]]) |
                         (lv1.data[, col.cat[j, cats[i]]] < phys.limit[1, cats[i]]) )
        lv1.data[ind.pl, (col.cat[j, cats[i]] + 1)] <- 4
      }
    }
  }

  return(lv1.data)
}

########
## physical limits Samoylov
physical.limits.sa <- function(lv1.data, col.cat) {

  cats <- colnames(col.cat)
  # Define physical limits for each category
  phys.limit <- t(read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Sa.csv"),
                             sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("min", "max")])
  colnames(phys.limit) <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Sa.csv"),
                                     sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("category")]
  # for each category
  for (i in 1:(length(cats))) {
    if (col.cat[1, cats[i]] > 0) {
      # for each dataseries within this category
      for (j in which(col.cat[, cats[i]] != 0)) {
        # flag data with '4' if it is above the maximum OR below the minimum physical limit
        ind.pl <- which( (lv1.data[, col.cat[j, cats[i]]] > phys.limit[2, cats[i]]) |
                         (lv1.data[, col.cat[j, cats[i]]] < phys.limit[1, cats[i]]) )
        lv1.data[ind.pl, (col.cat[j, cats[i]] + 1)] <- 4
      }
    }
  }

  return(lv1.data)
}


### VERSION 1 Pangaea:
# exclude Kurungnakh and Sardakh


########
## physical limits Kurungnakh
physical.limits.ku <- function(lv1.data, col.cat) {

  cats <- colnames(col.cat)
  # Define physical limits for each category
  phys.limit <- t(read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Ku.csv"),
                             sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("min", "max")])
  colnames(phys.limit) <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Ku.csv"),
                                     sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("category")]
  # for each category
  for (i in 1:(length(cats))) {
    if (col.cat[1, cats[i]] > 0) {
      # for each dataseries within this category
      for (j in which(col.cat[, cats[i]] != 0)) {
        # flag data with '4' if it is above the maximum OR below the minimum physical limit
        ind.pl <- which( (lv1.data[, col.cat[j, cats[i]]] > phys.limit[2, cats[i]]) |
                         (lv1.data[, col.cat[j, cats[i]]] < phys.limit[1, cats[i]]) )
        lv1.data[ind.pl, (col.cat[j, cats[i]] + 1)] <- 4
      }
    }
  }

  # Special cases for KuQ12013:
  # temporal resolution "KuQ12013": 10 minutes

  # Filter values for mean temp. above 0°C
  # a) if the mean temperature is below 0°C OR NA for more than 4 hours, than it is assumed that the lake is frozen
  # and that the measurements of WT and Q are corrupted

  if (station %in% c("KuQ12013")) {
    for (s in 1:length(lv1.data[, 1])) {
      temp_mean <- mean(as.numeric(lv1.data[seq(s, s + 25, by = 1), 2]), na.rm = T) # mean temp below 0?C for 4 hours
      if (temp_mean <= 0 || temp_mean == "NaN") {
        lv1.data[s, 'Q_fl'] <- 4
      }
    }

    # b) In August of the year 2014 the temperature sensor did not measure
    # ==> use air temperature in 80 cm height (Tair_80) from SaSnow2012 instead
    ###############
    # temporal resolution "KuQ12013": 10 minutes
    # temporal resolution "SaSnow2012": 30 minutes
    # because of this: the construction with s, l and m
    l <- 1
    m <- 0
    if (year_i == 2014) {
      SaMet14 <- read.table(paste0(p.1$w[p.1$n == "LV1.p"], "SaSnow2012/01_air_temp/SaSnow2012_", year_i, "_air_temp_lv1_noflag.dat"),
                            sep = ",", dec = ".", header = T)
      for (s in 1:52560) {
        m <- m + 1
        if (substr(SaMet14[l, 1], 1, 7) == "2014-08") {
          # mean temp above 0°C for 4.5 hours
          # ==> set flag 0 for Q
          temp_mean <- mean(as.numeric(SaMet14[seq(l, l + 8, by = 1), 'Tair_80']), na.rm = T)
          if (temp_mean > 0) {
            # flagging of three 10-minutes blocks of KuQ12013: 0, 10, 20 OR 30, 40, 50
            # to fit to the 30 minutes resolution of SaSnow2012
            lv1.data[s, 'Q_fl'] <- 0
            lv1.data[s + 1, 'Q_fl'] <- 0
            lv1.data[s + 2, 'Q_fl'] <- 0
          }
        }
        if (m == 3) {
          l <- l + 1
          m <- 0
        }
      }
    }
  }

  return(lv1.data)
}


########
## physical limits Sardakh
physical.limits.sd <- function(lv1.data, col.cat) {

  cats <- colnames(col.cat)
  # Define physical limits for each category
  phys.limit <- t(read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Sd.csv"),
                             sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("min", "max")])
  colnames(phys.limit) <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "phys.limits/phys.limit_Sd.csv"),
                                     sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")[, c("category")]
  # for each category
  for (i in 1:(length(cats))) {
    if (col.cat[1,cats[i]] > 0) {
      # for each dataseries within this category
      for (j in which(col.cat[, cats[i]] != 0)) {
        # flag data with '4' if it is above the maximum OR below the minimum physical limit
        ind.pl <- which( (lv1.data[, col.cat[j, cats[i]]] > phys.limit[2, cats[i]]) |
                         (lv1.data[, col.cat[j, cats[i]]] < phys.limit[1, cats[i]]) )
        lv1.data[ind.pl, (col.cat[j, cats[i]] + 1)] <- 4
      }
    }
  }

  return(lv1.data)
}

## ==============================================================================
##
## flag == 5    (Gradient)
##

# Each series is checked for prolonged constant periods and high / low spikes.
# This is done automatically by a sequence of six steps (a, ..., f).
# The checks are adjusted for the categories of the observed variables with six parameters (pp1, ..., pp6).
#
#######################################################################################
# peak parameters:
# pp1: maximum absolute difference between positive or negative peak and the two surrounding values in the measurement unit
#     -> used in step a)
#     -> the bigger the number, the taller peaks are allowed to be
# pp2: minimum difference between peak and adjacent two values to define a peak
#     -> used in step b)
#     -> the bigger the number, the less peaks are identified (most of these peaks are not flagged in the end, just if they are in an otherwise constant period)
# pp3: maximum difference between two values that indicates a constant period in which single peaks are unlikely
#     -> used in step b)
#     -> the bigger the number, the more variability is allowed in constant periods and the more peaks are identified
# pp4: number of closest non-NA entries for computing the quantiles in a moving window which is centered at the value to be checked
#     -> used in step c) - e)
#     -> the bigger the number, the more values are used for the quantiles
# pp5: maximum amount a value is allowed to exceed (fall below) the median in 0.95 (0.05) quantile minus median units
#     -> used in step c) - e)
#     -> the bigger the number, the less peaks are identified
# pp6: number of timesteps in a row with excactly constant values which indicate a suspicious period, values of 100 or more indicate no removal of constant periods
#     -> used in step f)
#     -> the bigger the number, the less constant periods are identified
#
#     The parameters of the checks for the different categories of variables are defined in:
#
#     N:\sparc\LTO\R_database\database_R\settings\peak.parameter.csv
#
#######################################################################################
# The data is flagged according to the following steps:
#
# a) Remove spikes (local maxima or minima of one or two values) which are more than pp1 different
#    from both neighbouring values
#
# b) Remove spikes bigger than pp2 in constant periods defined by pp3
#
# c) Flag and remove peaks which are
#   i) smaller than the median - pp5 * (median - 5th percentile) or
#   ii) larger than the median + pp5 * (95th percentile - median)
# where the median and quantiles are calculated for the pp4 closest neighbouring non-NA values of the value to be checked.
# ==> the moving window is centered at the value to be checked.
#
# In the operational window defined by pp4, values of the 0.05 (0.95) quantile relatively close to the median are replaced by standardized values median - pp1 * 0.02 (median + pp1 * 0.02).
#
# Step c) is performed THREE times in a row as the quantiles are affected by the spikes.
#
# d) Same conditions like c) with the additional condition that all flagged values are a
#   i) one, or
#   ii) two
# value "peak" in the sense of non-NA values directly surrounded by NA values.
#
# e) Repeat step c) ONCE with cleaned data.
#
# f) Flag periods with at least pp6 (e.g. 10) consecutive constant non-NA values.
#
#######################################################################################

detect.peaks <- function(lv1.data, col.cat, station) {
  # col.cat: matrix of categories and index of columns of lv1-Data which belongs to a specific category
  if (length(colnames(col.cat)) > 4) {
    #### the following variables are excluded
    #### Ts ==> soil temperature ==> very hard to catch, differences in depth
    #### vwc ==> volumetric water content ==> vwc is calculated from Ts and E2 ==> E2 is in this routine and give the flags to vwc
    ####
    #### !! but only for datasets other than "BaSoil1998" because the flag 5 routine was especially designed for
    #### the soil temperature data from this data set !!
###############################
# remove "KTl" for Pangaea
#####
###############################
    #
    if (!(station %in% c("BaSoil1998", "BaSoil2009", "BaHole2009"))) { # if (station != "BaSoil1998") {
      col.cat <- col.cat[, -which(names(col.cat) %in% c("Ts", "vwc", "KTl", "rho"))]
    }    
    if ((station %in% c("SaSnow2012"))) { 
      col.cat <- col.cat[, -which(names(col.cat) %in% c("Dsn"))]
    }
    if ((station %in% c("SaCalm2002"))) { 
      col.cat <- col.cat[, -which(names(col.cat) %in% c("Dal"))]
    }
  }

  cats <- colnames(col.cat)

  ####################################
  # read and set peak parameters pp
  pp <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "peak.parameter.csv"),
                   sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")
  # peak parameters which are defined for all datasets
  ind <- which(pp$dataset == "all")
  peak.parameter <- as.data.frame(t(pp[ind, c("pp1", "pp2", "pp3", "pp4", "pp5", "pp6")]))
  colnames(peak.parameter) <- pp[ind, c("category")]

  ####################################
  # stations (datasets) that exhibit specific pp values for some categories
  spec.stations <- as.character(unique(pp$dataset)[which(unique(pp$dataset) != "all")])

  if (station %in% spec.stations) {
    ind <- which(pp$dataset == station)
    # store column names
    colnames.peak.parameter <- colnames(peak.parameter)

    # replace pp values of the categories that have different values for the selected station
    ind <- ind[which(pp$category[ind] %in% colnames.peak.parameter)]
    for (ind2 in ind) {
      ind3 <- which(colnames(peak.parameter) == pp$category[ind2])
      peak.parameter[, ind3] <- as.numeric(pp[ind2, c("pp1", "pp2", "pp3", "pp4", "pp5", "pp6")])
    }

    # include categories which were not in the peak parameter table so far
    # which categories are not yet in the peak parameter table
    indx <- ind[which(!pp$category[ind] %in% colnames.peak.parameter)]
    # include those
    peak.parameter <- data.frame(peak.parameter, as.numeric(pp[indx, c("pp1", "pp2", "pp3", "pp4", "pp5", "pp6")]))
    # add new column names
    colnames(peak.parameter) <- c(colnames.peak.parameter, pp$category[indx])
  }
  ####################################

  # Index of 1 to number of rows of lv1.data. This is later used for distance in time steps to a specific value (i).
  tmpdate <- c(1:nrow(lv1.data))

  # initialise matrix for percentiles
  perc <- matrix(as.numeric(NA), nrow = nrow(lv1.data), ncol = 3)

  ####################################
  # loop with steps a) to f)
  # for each category
  for (i in 1:(length(cats))) {
    if (col.cat[1, cats[i]] > 0) {
      # for each dataseries within this category
      for (j in which(col.cat[, cats[i]] != 0)) {
        ###########################
        # create temporarily version of the data in which all values with a flag other than 100 are set to NA
        # because the default flag value is 100 due to the application of the function get.100flag.columns() in LV0_to_LV1_BaAll.R
        # ==> if the function is called in the order of the flag numbers, this means that the flags which were set in the routine before
        # (this is flag 1 to 4) are set to NA
        tmp <- lv1.data[, col.cat[j, cats[i]]]
        tmpflag <- lv1.data[, col.cat[j, cats[i]] + 1]
        tmp[tmpflag < 100] <- NA
        tmpnew <- tmp

        ###########################
        # continue only if there are more non-NAs than 2 times pp4 measurements + 1
        # pp4: number of closest non-NA entries for computing the quantiles
        if ((sum(!is.na(tmp)) > peak.parameter[4, cats[i]] * 2 + 1)) {
          # Start of steps a) to f)
          #############################
          # a) Flag and remove spikes (local maxima or minima of one or two values) which are more than pp1 different
          #    from both neighbouring values
          #############################
          # shrink the dataseries to non-NAs (many strange minima are next to NAs at one side and thus otherwise neglected)
          #
          # ATTENTION ==> This means that the value is compared towards an adjacent value and towards another value that can be quite some time apart
          # It can also happen that a single value, if surrounded by NAs is compared to non-adjacent values for peakiness
          #############################
          #
          # tmpNotNA: index of the data without NAs
          tmpNotNA <- !is.na(tmpnew)
          tmpnew2 <- tmpnew[tmpNotNA]
          tmpflag2 <- tmpflag[tmpNotNA]

          #############################
          # case 1: find all local one value maxima and minima (one value peaks)
          #############################
          l <- length(tmpnew2)

          # tmp1: difference between the values of neighbouring data points
          # "0" is added as first element of the diff-vector (tmp1) to keep the same index as the data vector
          # because of this it is not necessary to add a "+1" at the end of the index vector of the minima / maxima (findmax) below
          # !!! <==> this is different from the way it is done in section b) !!!
          tmp1 <- c(0, diff(tmpnew2))
          # tmp2: vector of difference between the signs of the differences between neighbouring data points
          tmp2 <- c(diff(sign(tmp1)), 0)
          #
          #   findmax: which values fulfill conditions i) AND ii) AND iii):
          #   i) which values are a peak compared to the two neighbouring values (change in sign) AND
          #   ii) which peaks are separated from the preceding value by a distance larger than the threshold defined as peak.parameter[1, xx] ==> pp1 AND
          #   iii) which peaks are separated from the following value by a distance larger than the threshold defined as peak.parameter[1, xx] ==> pp1
          findmax <- which( (abs(tmp2[1:(l - 1)]) == 2) &
                            (abs(tmp1[1:(l - 1)]) > peak.parameter[1, cats[i]]) &
                            (abs(tmp1[2:l]) > peak.parameter[1, cats[i]]) )
          # flag suspicious values
          tmpflag2[findmax] <- 5

          #############################
          # case 2: find all local two value minima and maxima (two value peaks)
          #############################
          #
          # !!! ATTENTION ==> here findmax of the step before is overwritten !!!
          #
          # findmax: which values fulfill conditions i) AND ii):
          #   i) for which values is the distance to the 2-lag neighbour larger than the threshold pp1
          #   ii) which values are potential two value peaks compared to the two adjacent values (change in sign) AND
          #       exhibit a distance to the surrounding values larger than the threshold pp1
          #
          ##############
          # Comments to
          # i) this identifies one value peaks with relaxed threshold conditions
          #    because the 2-lag neighbour distance is compared with the threshold, not like above the 1-lag neighbour distance !!!!
          #    ==> In analogy to the calculation of tmp1, this corresponds to c(0, diff(tmpnew2, lag = 2))
          # ii) this is solved here not with a comparison of the differences in sign vector (tmp2) like in the first routine, but with two conditions:
          #       c1: difference to the preceding value is larger than the threshold AND
          #           the negative of the difference to the 2-lag following value is larger than the threshold
          #       OR
          #       c2: the negative version of c1
          ##############
          findmax <- which( (abs(tmp1[1:(l - 2)] + tmp1[2:(l - 1)]) > peak.parameter[1, cats[i]]) & (
                            (tmp1[1:(l - 2)] > peak.parameter[1, cats[i]] & ((-1) * tmp1[3:l]) > peak.parameter[1, cats[i]]) |
                             (((-1) * tmp1[1:(l - 2)]) > peak.parameter[1, cats[i]] & tmp1[3:l] > peak.parameter[1, cats[i]])
                            )
                          )

          #####################
          # Because the routine above identifies only the index of the first value of a two values peak
          # the index of the second value (findmax + 1) is added here:
          findmax <- sort(unique(c(findmax, findmax + 1)))

          ######################
          # flag suspicious values
          tmpflag2[findmax] <- 5

          #####################
          # create new version of the data in which all values with a flag are set to NA
          # ==> excludes the values which were flagged in the above lines with flag = 5
          tmpflag[tmpNotNA] <- tmpflag2
          tmpnew[tmpflag < 100] <- NA

          # clear data space
          rm(tmpNotNA, tmpnew2, tmpflag2, l, tmp1, tmp2, findmax)

          #############################
          #############################
          #############################
          # b) Flag and remove spikes bigger than pp2 in constant periods defined by pp3

          # compute first derivative ==> vector of differences with lag 1
          tmpdif <- c(diff(tmpnew), 0)

          #####################
          # tmp1: identifies one-value peaks (change of sign) in relation to threshold peak.parameter[2, xx] ==> pp2
          #
          # all 1 value peaks:
          # c1: upward peak OR
          # c2: downward peak
          #
          # ==> to get index of peak: + 1 ==> the diff vector (tmpdif) is shifted by lag 1 with respect to the data vector (tmpnew)
          #
          # !!! this is different from section a), there a "0" was added as first element of the vector of differences (tmp1)
          # to keep the same index as the data vector
          #############
          tmp1 <- which(
                       ((tmpdif[1:(length(tmpdif) - 1)] > peak.parameter[2, cats[i]]) & (tmpdif[2:(length(tmpdif))] < (-peak.parameter[2, cats[i]]))) |
                       ((tmpdif[1:(length(tmpdif) - 1)] < (-peak.parameter[2, cats[i]])) & (tmpdif[2:(length(tmpdif))] > peak.parameter[2, cats[i]]))
                       ) + 1

          ###################
          # if the lag 4 - difference is relatively small (smaller than pp3) this is used as indication
          # that after the peak the level is rather similar (almost the same than before the peak).

          # tmp2a: difference vector of the data vector with lag 4 ==> diff(tmpnew, lag = 4)
          tmp2a <- tmpdif[1:(length(tmpdif) - 3)] + tmpdif[2:(length(tmpdif) - 2)] + tmpdif[3:(length(tmpdif) - 1)] + tmpdif[4:length(tmpdif)]

          # tmp2: which difference vector of the data vector with lag 4 is smaller than pp3 !! IN ABSOLUTE VALUES !!
          # +2 : to get the index in the middle of the assumed "constant period"
          tmp2 <- which(abs(tmp2a) < peak.parameter[3, cats[i]]) + 2

          # tmp3: index of values which fulfill 2 conditions (are part of both sets):
          #     c1: the index + 2 of the differences with lag 1 (!) that are smaller than pp3
          #         ==> index of 2 elements after the lag 1 difference is smaller than pp3
          #     c2: the index - 1 of the differences with lag 1 (!) that are smaller than pp3
          #         ==> index of 1 element earlier after the lag 1 difference is smaller than pp3
          #
          #     ==> the intersection of the 2 conditions c1 and c2 ensures that
          #         2 time steps before the peak as well as 1 time step after the peak,
          #         the lag-1 difference is smaller than pp3 (first derivative is rather small).
          tmp3 <- intersect( (which(abs(tmpdif) < peak.parameter[3, cats[i]]) + 2),
                             (which(abs(tmpdif) < peak.parameter[3, cats[i]]) - 1) )

          # join all criteria
          # ==> select only those values that fulfil all conditions (that are part of all sets)
          # tmp1: peaks > pp2
          # tmp2: difference to distance lag 4 < pp3
          # tmp3: only elements within sections of which 2 time steps before the peak as well as 1 time step after the peak, the lag-1 difference is smaller than pp3
          tmp4 <- intersect(intersect(tmp1, tmp2), tmp3)

          ######################
          # flag suspicious values
          tmpflag[tmp4] <- 5

          #####################
          # create new version of the data vector in which all values with a flag are set to NA
          # ==> excludes the values which were flagged in the above lines with flag = 5
          #
          # remove flagged values for next median calculation
          tmpnew[tmpflag < 100] <- NA

          # clear data space
          rm(tmpdif, tmp1, tmp2a, tmp2, tmp3, tmp4)

          #############################
          # c) Flag and remove spikes which deviate more than
          #    i) smaller than the median - pp5 * (median - 5th percentile) or
          #    ii) larger than the median + pp5 * (95th percentile - median)
          # where the median and quantiles are calculated for the pp4 closest neighbouring non-NA values of the value to be checked
          # ==> the moving window is centered at the value to be checked.
          #
          # In the operational window defined by pp4, values of the 0.05 (0.95) quantile relatively close to the median
          # are replaced by standardized values median - pp1 * 0.02 (median + pp1 * 0.02).
          # Step c) is performed THREE times in a row as the quantiles are affected by the spikes.

          ##########
          for (k in 1:3) {
            perc[, ] <- NA
            tmpNA <- is.na(tmpnew)
            # index of non-NAs
            tmpNA2 <- which(!is.na(tmpnew))
            # Calculate (new) median in a moving window and (again) remove spikes
            # moving window width: 2 * pp4 + 1
            for (l in tmpNA2) {
              # (absolute) distance (in time steps) of values to value l
              tmpDist <- abs(tmpdate - l)
              # remove NA entries from list
              tmpDist[tmpNA] <- NA
              # find the indices of the pp4 closest non-NA entries for computing the quantiles
              tmpClosest <- sort(order(tmpDist)[1:(peak.parameter[4, cats[i]] * 2 + 1)])
              # calculate 5, 50, 95 percentile for every value and its closest surrounding values (according to pp4)
              perc[l, 1:3] <- quantile(tmpnew[tmpClosest], c(.05, .50, .95), na.rm = TRUE)
              # values relatively close to the median are replaced by standardized values
              # if 5 percentile (0.05 quantile) larger than or equal to: the median (0.5 quantile) - pp1 * 0.02
              # than replace the 0.05 quantile with: the median (0.5 quantile) - pp1 * 0.02
              if (perc[l, 1] >= (perc[l, 2] - peak.parameter[1, cats[i]] * 0.02)) {
                  perc[l, 1] <- perc[l, 2] - peak.parameter[1, cats[i]] * 0.02
              }
              # if 95 percentile (0.95 quantile) smaller than or equal to: the median (0.5 quantile) - pp1 * 0.02
              # than replace the 0.95 quantile with: the median (0.5 quantile) - pp1 * 0.02
              if (perc[l, 3] <= (perc[l, 2] + peak.parameter[1, cats[i]] * 0.02)) {
                  perc[l, 3] <- perc[l, 2] + peak.parameter[1, cats[i]] * 0.02
              }
            }

            ######################
            # flag suspicious values
            # all values are flagged that are smaller than the median - pp5 * (median - 0.05 quantile)
            tmpflag[tmpnew < (perc[, 2] - peak.parameter[5, cats[i]] * (perc[, 2] - perc[, 1]))] <- 5
            # all values are flagged that are bigger than the median + pp5 * (0.95 quantile - median)
            tmpflag[tmpnew > (perc[, 2] + peak.parameter[5, cats[i]] * (perc[, 3] - perc[, 2]))] <- 5
            ######################
            # create new version of the data vector in which all values with a flag are set to NA
            # ==> excludes the values which were flagged in the above lines with flag = 5
            #
            # remove flagged values for next median calculation
            tmpnew[tmpflag < 100] <- NA
          }

          # clear data space
          rm(k, l, tmpNA, tmpNA2, tmpDist, tmpClosest)

          #############################
          # d) Same conditions like step c) with the additional condition that all flagged values are a
          #   i) one, or
          #   ii) two
          # value "peak" in the sense of non-NA values directly surrounded by NA values.
          #
          #############################

          # tmp1: TRUE / FALSE vector with index of NAs in data vector (tmpnew)
          tmp1 <- is.na(tmpnew)
          # matrix of zeros
          tmp2 <- matrix(0, nrow = length(tmp), ncol = 5)

          # value itself is NA: not interesting ==> stays 0
          # value (x) itself is NOT NA ==> 1

          # !!! ATTENTION ==> the logic of this first check is kind of different compared to the next lines
          # to enable the final summing up step of tmp1 at the end of flagging step c) !!!
          tmp2[tmp1 == FALSE, 1] <- 1

          # index of values that are followed by a NA ==> (x + 1) = NA
          tmp2[which(tmp2[2:nrow(tmp2), 1] == 0), 2] <- 1
          # index of values for which the 2nd following value is a NA ==> (x + 2) = NA
          ## second subsequent value (x + 2) is NA
          tmp2[which(tmp2[3:nrow(tmp2), 1] == 0), 3] <- 1
          # index of values for which the preceding value is a NA ==> (x - 1) = NA
          # preceding value (x - 1) is NA
          tmp2[which(tmp2[1:(nrow(tmp2) - 1), 1] == 0) + 1, 4] <- 1
          # index of values for which the second preceding value is a NA ==> (x - 2) = NA
          # second preceding value (x - 2) is NA
          tmp2[which(tmp2[1:(nrow(tmp2) - 2), 1] == 0) + 2, 5] <- 1

          ##############
          ### overwrite tmp1
          ### tmp1: matrix of zeros
          tmp1 <- matrix(0, nrow = length(tmp), ncol = 3)
          # case 1: single non-NA value surrounded by NA
          # single non-NA value (tmp2[, 1] == 1) surrounded by NA (tmp2[, 2] == 1) AND (tmp2[, 4] == 1)
          tmp1[, 1] <- tmp2[, 1] + tmp2[, 2] + tmp2[, 4]
          # case 2 and 3: two non-NA values surrounded by NA
          # case 2: non-NA value (tmp2[, 1] == 1) AND NAs: subsequent value (tmp2[, 2] == 1) and second preceding value (tmp2[, 5] == 1)
          tmp1[, 2] <- tmp2[, 1] + tmp2[, 2] + tmp2[, 5]
          # case 3: non-NA value (tmp2[, 1] == 1) AND NAs: second subsequent value (tmp2[, 3] == 1) and preceding value (tmp2[, 4] == 1)
          tmp1[, 3] <- tmp2[, 1] + tmp2[, 3] + tmp2[, 4]
          # overwrite tmp1 ==> create vector with maximum of all rows
          # ==> this gives the value 3 if for one of the three cases above all conditions are met
          tmp1 <- apply(tmp1, 1, max)

          ######################
          # flag suspicious values
          # same like in section c) AND condition tmp1 == 3
          tmpflag[(tmpnew < perc[, 2] - peak.parameter[5, cats[i]] * (perc[, 2] - perc[, 1])) & (tmp1 == 3)] <- 5
          tmpflag[(tmpnew > perc[, 2] + peak.parameter[5, cats[i]] * (perc[, 3] - perc[, 2])) & (tmp1 == 3)] <- 5
          ######################
          # create new version of the data vector in which all values with a flag are set to NA
          # ==> excludes the values which were flagged in the above lines with flag = 5
          #
          # remove flagged values
          tmpnew[tmpflag < 100] <- NA

          # clear data space
          rm(tmp1, tmp2)

          #############################
          # e) repeat step c) ONE time (not three times) with cleaned data
          #
          #############################

          perc[, ] <- NA
          tmpNA <- is.na(tmpnew)
          # index of non-NAs
          tmpNA2 <- which(!is.na(tmpnew))
          # Calculate (new) median in a moving window and (again) remove spikes
          # moving window width: 2 * pp4 + 1
          for (l in tmpNA2) {
            # (absolute) distance (in time steps) of values to value l
            tmpDist <- abs(tmpdate - l)
            # remove NA entries from list
            tmpDist[tmpNA] <- NA
            # find the indices of the pp4 closest non-NA entries for computing the quantiles
            tmpClosest <- sort(order(tmpDist)[1:(peak.parameter[4, cats[i]] * 2 + 1)])
            # calculate 5, 50, 95 percentile for every value and the closest surrounding values
            perc[l, 1:3] <- quantile(tmpnew[tmpClosest], c(.05, .50, .95), na.rm = TRUE)

            # if 5 percentile (0.05 quantile) larger than or equal to: the median (0.5 quantile) - pp1 * 0.02
            # than replace the 0.05 quantile with: the median (0.5 quantile) - pp1 * 0.02
            if (perc[l, 1] >= (perc[l, 2] - peak.parameter[1, cats[i]] * 0.02)) {
                perc[l, 1] <- perc[l, 2] - peak.parameter[1, cats[i]] * 0.02
            }

            # if 95 percentile (0.95 quantile) smaller than or equal to: the median (0.5 quantile) - pp1 * 0.02
            # than replace the 0.95 quantile with: the median (0.5 quantile) - pp1 * 0.02
            if (perc[l, 3] <= (perc[l, 2] + peak.parameter[1, cats[i]] * 0.02)) {
                perc[l, 3] <- perc[l, 2] + peak.parameter[1, cats[i]] * 0.02
            }
          }

          # flag suspicious values
          tmpflag[tmpnew < (perc[, 2] - peak.parameter[5, cats[i]] * (perc[, 2] - perc[, 1]))] <- 5
          tmpflag[tmpnew > (perc[, 2] + peak.parameter[5, cats[i]] * (perc[, 3] - perc[, 2]))] <- 5

          # create new version of the data vector in which all values with a flag are set to NA
          # ==> excludes the values which were flagged in the above lines with flag = 5
          #
          # remove flagged values for next median calculation
          tmpnew[tmpflag < 100] <- NA

          # clear data space
          rm(l, tmpNA, tmpDist, tmpClosest)

          #############################
          # f) flag periods with at least pp6 (e.g. 10) consecutive constant values
          #
          #############################
## ????????? NO UPDATE Of tempNA2 (index of non-NA´s) after step e) ?? ==> because it is used in the second next if-condition

          # exceptions
          if (!(station %in% c("BaHole2009", "BaHole2015", "SaSoil2002", "SaSoil2012","SaSnow2012"))) {
#,"SaSoil2002" problems in year 2014
            # if pp6 IS NOT 100 AND there are non-NA values, than...
            if ((peak.parameter[6, cats[i]] < 100) & (length(tmpNA2) > 0)) {
              # shrink the dataseries to non NAs (many strange periods are next to NAs at one side and thus otherwise neglected)
              tmpNotNA <- !is.na(tmpnew)
              tmpnew2 <- tmpnew[tmpNotNA]
              tmpflag2 <- tmpflag[tmpNotNA]

              # difference between two values
              tmp1 <- c(1, abs(diff(tmpnew2)))
              # sum of difference of consecutive pp6 values
              # initialise vector of 0s
              tmp2 <- vector(mode = 'numeric', length = length(tmp1))

              for (n in 1:(peak.parameter[6, cats[i]] - 1)) {
                # shift.ind: tmp1 shifted by steps n
                shift.ind <- n:(length(tmp1) - peak.parameter[6, cats[i]] + n)
                # concatenate 1s at the end of the vector tmp1 to achieve the same length as vector tmp2
                tmp2 <- tmp2 + c(tmp1[shift.ind],
                                (vector(mode = 'numeric', length = peak.parameter[6, cats[i]] - 1) + 1))
              }

              ###########
              # first of the two indices of two subsequent constant values
              tmp3 <- which(tmp2 == 0) - 1
              ########################
              # all other indices of the 2nd, 3rd, 4th, 5th, ... constant value
              findconst <- tmp3
              for (n in 1:peak.parameter[6, cats[i]]) {
                findconst <- c(findconst, tmp3 + n - 1)
              }

              # remove duplicates and sort
              findconst <- sort(unique(findconst))
              findconst[findconst > length(tmpflag2)] <- NA

              ########################
              # flag suspicious values
              tmpflag2[findconst] <- 5
              tmpflag[tmpNotNA] <- tmpflag2

              # create new version of the data vector in which all values with a flag are set to NA
              # ==> excludes the values which were flagged in the above lines with flag = 5
              tmpnew[tmpflag < 100] <- NA

              # clear data space
              rm(tmpNotNA, tmpnew2, tmp1, tmp2, tmp3, findconst, tmpflag2)
            }
          }
          # replace old flag with new flag
          lv1.data[, col.cat[j, cats[i]] + 1] <- tmpflag

          # clear data space
          rm(tmpflag, tmpnew, tmp, tmpNA2)
        }
      }
    }
  }

  return(lv1.data)
}

## ==============================================================================
##
## flag == 6    (Plausibility)
##
add.plausibility <- function(lv1.data, station) {
  ## Additional errors that can only be identified based on other variables:

  cols <- colnames(lv1.data)

  # year_p: year to be processed
  # because in the Lv 0 to Lv 1 Script the week before and after the actual year to be processed is added
  # for the application of some functions using moving windows,
  # year_p is calculated here as the mean of all years
  # (because there is no built in mode function in R)
  year_p <- round(mean(as.numeric(format(as.Date(lv1.data$UTC, format = '%Y-%m-%d %H:%M'), '%Y'))))


##########
#!!!! Sonderfälle a, b in filter files der einzelnen Jahre ausgelagert
##########

  if (station == 'BaMet1998') {
    # #############################
    # # a) extreme wind velocity coincides with wind direction and sdev of wind direction = 0
    # i <- c(which(cols == 'wind_v_200_fl'), which(cols == 'wind_deg_200_fl'), which(cols == 'wind_sddeg_200_fl'))
    # if ((year_p == 2008) & (length(i) > 0)) {
    #   # September and October 2008
    #
    #   # !!! ATTENTION:
    #   # ==> not quite clear why the selection is done here in this way:
    #   # see few lines below for tmp3
    #   # it would be more simple to define tmp1 and tmp2 directly as the respective date
    #   #tmp1 <- as.POSIXct('2008-09-01 00:00', origin = options()$origin, format = '%Y-%m-%d %H:%M')
    #   #tmp1 <- which(lv1.data$UTC >= tmp1)[1]
    #   tmp1 <- which(lv1.data$UTC == '2008-09-01 00:00')
    #   #tmp2 <- as.POSIXct('2008-10-31 23:00', origin = options()$origin, format = '%Y-%m-%d %H:%M')
    #   #tmp2 <- which(lv1.data$UTC >= tmp2)[1]
    #   tmp2 <- which(lv1.data$UTC == '2008-10-31 23:00')
    #   # OLD: remove also the following velocity values as wind velocities are still far too high
    #   # NEW: indices of values with wind direction = 0 (x) and of the respective next value (x + 1)
    #   tmp3 <- sort(unique(c(which(lv1.data$wind_deg_200 == 0), which(lv1.data$wind_deg_200 == 0) + 1)))
    #   # which dates fulfill the above condition and are within the time above defined period
    #   # !!! ATTENTION: is the idea here to define a period from tmp1 to tmp2?
    #   # ==> see comment above
    #
    #   tmp3 <- tmp3[(tmp3 >= tmp1) & (tmp3 <= tmp2)]
    #   # flag those values with 6
    #   lv1.data[tmp3, i] <- 6
    # }
    #
    # #############################
    # # b) extremely low and partly constant values for relative humidity
    # i <- which(cols == 'RH_200_fl')
    # if ((year_p == 2008) & (length(i) > 0)) {
    #   lv1.data[which(lv1.data$RH_200 < 42), i] <- 6
    #
    #   ##############
    #   # the RH values < 0 are already flagged with flag 4.
    #   # Flags with higher rank (lower numeric value != 0) are maintained ==> update.flags() function
    #   ##############
    # }


#############################
## VERSION 2:
## 1998: G_cen_18 Mitte November bis Anfang Dezember ==> kompletten Zeitraum manuell rausnehmen ??
#############################

    #############################
    # c) spikes in ground heatflux data 1998 - April 2000 at 13:00
    i <- col.cat[col.cat[,'G'] != 0, 'G']
    # index of flag column
    i <- i + 1
    if ( (year_p <= 2000) & (length(i) > 0) ) {
      t.hour <- as.numeric(format(strptime(lv1.data$UTC, '%Y-%m-%d %H:%M'), "%H"))
      for (j in length(i)) {
        tmp.data <- lv1.data[, i[j] - 1]
        # indices of all spikes above 0.07 and below 1 W/m^2 (range of error)
        spike.up <- which(
          # difference of the values and their preceding values:
          # x[2:(n-1)] - x[1:(n-2)]
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[1:(length(tmp.data) - 2)]) > 0.07) &
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[1:(length(tmp.data) - 2)]) < 1) &
          # difference of the values and their subsequent values:
          # x[2:(n-1)] - x[3:n]
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[3:length(tmp.data)]) > 0.07) &
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[3:length(tmp.data)]) < 1)) + 1

        spike.down <- which(
          # difference of the values and their preceding values:
          # x[2:(n-1)] - x[1:(n-2)]
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[1:(length(tmp.data) - 2)]) < -0.07) &
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[1:(length(tmp.data) - 2)]) > -1) &
          # difference of the values and their subsequent values:
          # x[2:(n-1)] - x[3:n]
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[3:length(tmp.data)]) < -0.07) &
          ((tmp.data[2:(length(tmp.data) - 1)] - tmp.data[3:length(tmp.data)]) > -1)) + 1
        # union of all spikes
        spike <- sort(c(spike.up, spike.down))

        if (year_p == 2000) {
          # problem only before 18 April
          tmp2 <- which(lv1.data$UTC == '2000-04-18 00:00')
          spike <- spike[spike < tmp2]
        }
        # only for values at hour 13
        lv1.data[intersect(which(t.hour == 13), spike), i[j]] <- 6
      }
    }
  }

  if ((station == 'BaMet1998') | (station == 'BaMet2009')) {
    #############################
    # d) albedo not useful for SwIn < 5W/m^2 OR missing values of SwIn
    i <- which(cols == 'Albedo_fl')
    if (length(i) > 0) {
      lv1.data[which( (lv1.data$SwIn < 5) | is.na(lv1.data$SwIn) ), i] <- 6
    }

    #############################
    # e)
    # i) liquid precipitation not likely below -2 degree C and
    # ii) precipitation > 1 mm likely wrong at wind speeds >= 12.5m/s hourly average

    i <- which(cols == 'prec_fl')
    if (length(i) > 0) {
      lv1.data[which( (lv1.data$Tair_200 < -2) & (lv1.data$prec > 0) ), i] <- 6
      lv1.data[which( (lv1.data$wind_v_200 > 12.5) & (lv1.data$prec > 1) ), i] <- 6
    }
  }

  if (station == 'SaMet2002') {
    # f) possibility of snow or dirt on sensor for incoming radiation
    # ==> threshold: more than 10 W/m^2 short wave radiation is going out than coming in
    i <- which(cols %in% c('SwIn_fl', 'SwOut_fl', 'SwNet_fl', 'LwIn_fl', 'LwOut_fl', 'LwNet_fl', 'Albedo_fl', 'NetRad_fl'))

    if (length(i) > 0) {
      lv1.data[which( (lv1.data$SwIn - lv1.data$SwOut < (-10)) ), i] <- 6
    }

    # g) All values of relative humidity in height 50 and 200 cm ("RH_50", "RH_200")
    # if the air temperature of that height ("Tair_a_50", "Tair_a_200") was flagged with flag 4.
    # Because both parameters are measured with the same sensor and air temperature is used in the calculation of relative humidity.
    # Consquently, if the physical limits of air temperature are exceeded, the relative humidity values are not reliable any more.
    # !!
    # Attention: The flagging is directly related to the physical limits of the air temperature sensor.
    # If a new sensor, with new temperature range, is installed, this general rule here has to be adjusted to the time periods of the different sensors.
    # ==> The sensor was changed in September 2017
    # !!
    lv1.data$RH_200_fl[which(lv1.data$Tair_a_200_fl == 4)] <- 6
    lv1.data$RH_50_fl[which(lv1.data$Tair_a_50_fl == 4)] <- 6

  }

  return(lv1.data)
}


## ==============================================================================
##
## flag == 7   (sensor accuracy decreased significantly)
##

detect.T.degradation <- function(lv1.data, col.cats, station) {

  #############################
  # a) soil temperature
  cats <- colnames(col.cat)
  # select indices of columns with soil temperature (excluding the 0 entries of the column index table)
  I <- col.cat[col.cat[, 'Ts'] != 0, 'Ts']
  if (length(na.omit(I)) > 0) {
    # tested based on the zero curtain in the freezeback period
    # (roughly 10 days between mid September and end of October in the different years)
    # "%j": day of year as decimal number
    # ==> daily time scale
    doy <- as.numeric(strftime(lv1.data$UTC, format = '%j'))

    # index of the last value of the preceding year
    time.extra <- which(doy == 1)[1] - 1
    ##########
    # year
    tz.year <- as.numeric(format(as.Date(lv1.data$UTC[time.extra + 2], format = '%Y-%m-%d %H:%M'), '%Y'))
    ##########
    # extra period at the beginning from last year is set to 0
    doy[1:time.extra] <- 0
    # extra period after the current year is set to (number of days of the current year + 1)
    doy[(length(doy) - time.extra + 1):length(doy)] <- doy[(length(doy) - time.extra)] + 1
    years <- 1998:recent.year

#########
# ==> stephan fragen wegen 2020 oder besser recent.year
#########

    zero.curtain.doi <- matrix(0, ncol = 3, nrow = length(years))
    zero.curtain.doi[,1] <- years

    ##############
    # read table of periods of zero curtain
    # and select the period of the processed year and station
    if (station %in% c("SaSoil1998", "SaSoil2002", "SaMet2002", "SaSoil2012")) {
    zero.curtain.doi <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "zero.curtain/zero.curtain.doy_Sa.dat"),
                                   sep = ",", dec = ".", header = FALSE)
    }
    if (station %in% c("BaSoil1998", "BaSoil2009", "BaMet1998", "BaMet2009")) {
    zero.curtain.doi <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "zero.curtain/zero.curtain.doy_Ba.dat"),
                                   sep = ",", dec = ".", header = FALSE)
    }
    if (station %in% c("TVCSoil2016","TVCHole12015","TVCHole22015")) {
      zero.curtain.doi <- read.table(file = paste0(p.1$w[p.1$n == "settings.p"], "zero.curtain/zero.curtain.doy_TVC.dat"),
                                     sep = ",", dec = ".", header = FALSE)
    }

    tmpdata <- lv1.data[, I]
    tmpdata[lv1.data[, I + 1] < 100] <- NA

    zc <- zero.curtain.doi[zero.curtain.doi[,1] == tz.year,2:3]

    # index of the first and the last data point within the zero curtain period
    zcy <- c(which(doy >= zc[1])[1], tail(which(doy <= zc[2]), n = 1))

    if (!(station %in% c("SaHole2006","SaHole2010", "SaHole2018", "SdHole2009", "BaHole2009", "BaHole2015","SaCalm2002"))) {
      if (!is.na(zcy[1])) {
        # calculate mean and sdev for each series
        tmpmean <- colMeans(tmpdata[zcy[1]:zcy[2], ], na.rm = TRUE)
        tmpstd <- apply(tmpdata[zcy[1]:zcy[2], ], 2, sd, na.rm = TRUE)

        # define BEGINNING of first and second last day of August
        tmp1 <- as.POSIXct(paste(toString(year_i), '-08-01 00:00', sep = ''), origin = options()$origin, format = '%Y-%m-%d %H:%M')
        tmp2 <- as.POSIXct(paste(toString(year_i), '-08-30 00:00', sep = ''), origin = options()$origin, format = '%Y-%m-%d %H:%M')
        tmp3 <- which( (lv1.data$UTC >= tmp1) & (lv1.data$UTC <= tmp2) )
        tmpSummer <- apply(tmpdata[tmp3, ], 2, quantile, probs = c(0.9), na.rm = TRUE)

        ################################
        # all three conditions have to be fulfilled:
        # i) standard deviation of temperature of zero curtain period smaller than 0.25
        # ==> because zero curtain period exhibits only little variation in temperature
        # ii) series identified with shifted zero curtain
        # ==> mean temperature of zero curtain period outside the +/- 0.5 degree C range
        # iii) 0.9 Percentile of temperature of period 1st to 30th of August > 0
        # ==> the soil was thawed
        tmpBAD <- which( (tmpstd < 0.25) & (abs(tmpmean) >= 0.5) & (tmpSummer > 0) )

        ################################
        # flag values
        lv1.data[, I[tmpBAD] + 1] <- 7

        # clear data space
        rm(tmpdata, tmpBAD, tmpmean, tmpstd, tmp1, tmp2, tmp3, tmpSummer, zc, zcy, I)
      }
    }
  }

  return(lv1.data)
}


## ==============================================================================
##
## flag == 8   (sensor snow covered)
##

# a) Frozen water table:
# This concerns only the sensor measuring water table ("WT") at dataset "SaMet2002").
# The selected year is divided in two parts. The first part contains the first 55% of the series, the second part the remaining 45%.
# Thus, the first part starts in winter ("winter influenced period"), the second part starts in summer ("summer influenced period").
# Because the variable of interest is the liquid water table, and not the frozen water:
# -	all data points in the first part with soil temperature in 6 cm depth (Ts_6) < 0.4, and
# -	all data points in the second part with soil temperature in 6 cm depth (Ts_6) < 0.1,
# which are not flagged with another flag than 0 are flagged with 8.
# It is assumed that coming from the colder "winter influenced period", it needs warmer temperatures to change the phase of the water from solid to liquid,
# and that coming from the warmer "summer influenced period", it needs colder temperatures to change the phase of the water from liquid to solid.
# This is accounted for with the different temperature thresholds of the winter and summer influenced period. Those thresholds were fit manually.

frozen.water.table <- function(lv1.data, col.cats, station, year_i) {

  I <- col.cat[col.cat[, 'WT'] != 0, 'WT']

  if (length(I) > 0) {

    if (station == "SaMet2002") {
      tmpdata <- lv1.data[, c("WT", "WT_fl", "Ts_6")]
    }

    ## division of the year in two parts:
    ## The first part contains the first 55% of the series, the second part the remaining 45%.
    gr <- round((length(tmpdata$WT) / 2) * 1.1, 0)
    pr <- (length(tmpdata$WT))
    #########################

#########################
# tmp1: selection of flagging column and data column
# BE AWARE OF THE ORDER:
# First the flagging column, than the data column
# ==> this is different to all other selections in the script
#
# ==> is there a specific reason for this?
#
#  if not
#
# ==> change order before publication
#########################
# OLD
# tmp1 <- lv1.data[, c(I + 1, I)]
    # changed to:
    tmp1 <- lv1.data[, c(I, I + 1)]

#tmp1[1:gr,which(tmpdata$Ts_6<0.4)]<-8
#########################
# tmp1[, 1]: flagging column (WT_fl)
#
#
# All data points in the first part with temperature (Ts_6) < 0.4 and NO Flag are flagged with 8
#  tmp1[1:gr, 1][which( (tmpdata[1:gr, 3] < 0.4) & (tmpdata$WT_fl[1:gr] == 100) )] <- 8
# All data points in the second part with temperature (Ts_6) < 0.1 and NO Flag are flagged with 8
# tmp1[gr:pr, 1][which( (tmpdata[gr:pr, 3] < 0.1) & (tmpdata$WT_fl[gr:pr] == 100) )] <- 8

    # changed to:
    #
    # tmp1[, 2]: flagging column (WT_fl)
    #
    # All data points in the first part with temperature (Ts_6) < 0.4 and NO Flag are flagged with 8
    tmp1[1:gr, 2][which( (tmpdata[1:gr, 3] < 0.4) & (tmpdata$WT_fl[1:gr] == 100) )] <- 8
    # All data points in the second part with temperature (Ts_6) < 0.1 and NO Flag are flagged with 8
    tmp1[gr:pr, 2][which( (tmpdata[gr:pr, 3] < 0.1) & (tmpdata$WT_fl[gr:pr] == 100) )] <- 8

    #########################

    # tmp2 <- lv1.data[,I+1]
    # tmp2[tmp1] <- 8

#########################
# OLD:
# lv1.data[, I + 1] <- tmp1[, 1]
### tmp1[, 1]: flagging column (WT_fl)
### different from order in the rest of the script
### ==> see above
### check:
### names(tmp1)
### names(lv1.data)[I + 1]
    ## ==> change to:
    lv1.data[, I + 1] <- tmp1[, 2]
  }
  return(lv1.data)
}

##########
# b) estimate to which height the air temperature sensors are likely to be snow covered based on characteristics of the measured temperatures
#
# why is a default value used here, if the time.res is always defined in LV0_to_LV1_BaAll.R?
# better remove default value
#
detect.snow.cover <- function(lv1.data, col.cats, time.res = 24) {

  cats <- colnames(col.cat)
  # distance (in time steps) of all values to specific value (i)
  tmpdate <- c(1:nrow(lv1.data))

  I <- col.cat[col.cat[, 'Tair'] != 0, 'Tair']
  if (length(na.omit(I)) > 1) {
    # set all flags < 100 to NA
    tmpdata <- lv1.data[, I]
    tmpdata[lv1.data[, I + 1] < 100] <- NA
    # instrument height
    tmpheight <- as.numeric(regmatches(colnames(tmpdata), regexpr('a*[0-9]+', colnames(tmpdata), perl = TRUE)))

    # moving window for standard deviation
    # time.res: measurements per day
    # mw.width: moving window width: low-high variation in winter roughly 4 days ===> 2 days to each side
    mw.width <- time.res * 4

    # moving window for standard deviation ===> fill at the beginning and end of the series with 0s
    ############
    # (why not with NA?s??) ==> see the next lines
    ############
    tmpSTD <- rollapply(tmpdata, width = mw.width, sd, na.rm = TRUE, fill = 0, align = 'center')
    # replace the inserted 0s with the first line after the first 2 days (half of the length of the moving window)
    # this is still inside the additional week of the last year and the subsequent year
    tmpSTD[1:((mw.width / 2) - 1), ] <- matrix(rep(tmpSTD[(mw.width / 2), ], (mw.width / 2 - 1)), ncol = length(I), byrow = T)
    # replace the inserted 0s with the first line before the last 2 days (half of the length of the moving window)
    tmpSTD[(nrow(tmpdata) - mw.width / 2 + 1):nrow(tmpdata), ] <- matrix(rep(tmpSTD[(nrow(tmpdata) - mw.width / 2), ], (mw.width / 2)), ncol = length(I), byrow = T)

    ##################
    # find height with FIRST maximum sdev and find the lowest height with at least 80% of the maximum sdev
    # Indices of columns with first maximum sdev ==> indx

    # should be not a problem, because indx is used to calculcate percSTD
    # ==> the value of the maximum sdev is used to divide the values in all columns to calculate the ratio with respect to the maximum sdev
    indx <- max.col(replace(tmpSTD, is.na(tmpSTD), -Inf), ties.method = 'first')
    # vector of maximum sdev´s
    maxSTD <- tmpSTD[cbind(1:nrow(tmpSTD), indx)]
    # sdev of the different heights as percentage of the maximum sdev
    percSTD <- tmpSTD / matrix(rep(maxSTD, length(I)), ncol = length(I))

    # find sensors showing T > 0.1 degree C as those are clearly not snow-covered
    tmpzero <- tmpdata > 0.1
    # set NAs FALSE
    tmpzero[is.na(tmpzero)] <- FALSE

    classSTD <- percSTD
    # above the snow (either variable signal OR warm)
    # ==> all readings with sdev >= 80% of the maximum sdev OR temperature > 0.1°C ==> set category 1
    classSTD[(classSTD >= 0.80) | tmpzero] <- 1
    # potentially below the snow (signal not variable AND cold)
    # ==> all readings with sdev < 80% of the maximum sdev AND (temperature <= 0.1 degree C OR NA) ==> set category 1000

    # tmpzero == FALSE ==> all values with temperature <= 0.1°C or NA
    classSTD[(classSTD < 0.80) & tmpzero == FALSE] <- 1000
    # product of height of temperatur sensors and the two categories 1 and 1000 of classSTD
    snowheight <- classSTD * matrix(rep(tmpheight, length(tmpdate)), ncol = length(I), byrow = T)
    # replace NAs with -Infinity and identify in matrix snowheight the
    # indices of the FIRST column that exhibits maximum negative snow height ((-1) * snowheight)
    # ==> maximum of negative snowheight ensures, that columns of category 1 are selected
    # in case both categories 1 and 1000 exist for a specific date / reading
    #######
    # ==> the negative of the snow height is used, because there is no function min.col in R-base
    # in case of ties, the first column of the columns with ties is used
    # ==> in that case, the order of the columns decides which column is selected
    #######
    indx <- max.col(replace((-1) * snowheight, is.na(snowheight), -Inf), ties.method = 'first')
    # sensor above
    sensor_above <- snowheight[cbind(1:nrow(snowheight), indx)]

    ##################
    ### remove snowfall periods with less than 4 days
    ######################
    # To give a more robust estimate of snow cover height,
    # all snow heights between snowfall events and snowmelt events of which
    # i) the snowmelt event takes place less than 4 days after the snowfall event, and
    # ii) of which the new snow height is equal or below to the snow height of the snowfall event,
    # are replaced with the snow height prior to that snowfall event.
    ######################

    tmpdiff <- c(0, diff(sensor_above))
    # due to changes in snow cover, the level of snow reaches different sensors
    # case 1: index of readings with decrease in snowheight
    tmpmelt <- which(tmpdiff < 0)
    # case 2: index of readings with increase in snowheight
    tmpfall <- which(tmpdiff > 0)

    for (i in 1:length(tmpfall)) {
      # reaches again previous level:
      # for which tmpmelt events is the snow height (with respect to sensor) below or equal to the snow height before the tmpfall event i
      # AND the index of the tmpmelt event after the tmpfall event i
      # ==> select only the tmpmelt events after the tmpfall event i
      j1 <- which((sensor_above[tmpmelt] <= sensor_above[tmpfall[i] - 1]) & (tmpmelt > tmpfall[i]))
      if (length(j1) > 0) {
        # select the first of those tmpmelt events
        # ==> select the first tmpmelt event after tmpfall event i
        j1 <- tmpmelt[min(j1)]
        # if the first tmpmelt event j1 after the tmpfall event i is less than 4 days apart from the tmpfall event i
        # than replace the snowheights between the two events with the snowheight of the reading before the tmpfall event i
        if (j1 - tmpfall[i] < (4 * time.res)) {
          sensor_above[tmpfall[i]:j1] <- sensor_above[tmpfall[i] - 1]
        }
      }
    }

    ##################
    # remove snowmelt periods with less than 4 days (method is not precise enough for such small periods)
    # ==> analogue to the removal of snowfall periods above
    #
########################
# This comment is unclear:
#
# warm conditions are no pre-requisite as snow can also be blown away
########################
    #
    #
    # Analogue to the above, all snow heights between snowmelt events and snowfall events of which
    # i) the snowfall event takes place less than 4 days after the snowmelt event, and
    # ii) of which the new snow height is equal or larger to the snow height of the snowmelt event,
    # are replaced with the snow height prior to the respective snowmelt event.
    ##################

    tmpdiff <- c(0, diff(sensor_above))
    tmpmelt <- which(tmpdiff < 0)
    tmpfall <- which(tmpdiff > 0)
    ###########################

    for (i in 1:length(tmpmelt)) {
      # reaches again previous level:
      j1 <- which((sensor_above[tmpfall] >= sensor_above[tmpmelt[i] - 1]) & (tmpfall > tmpmelt[i]))
      if (length(j1) > 0) {
        j1 <- tmpfall[min(j1)]
        if (j1 - tmpmelt[i] < (4 * time.res)) {
          sensor_above[tmpmelt[i]:j1] <- sensor_above[tmpmelt[i] - 1]
        }
      }
    }

    # compile information of what to flag:
    # Is the sensor height below the estimated snow height?
    tmp1 <- matrix(FALSE, nrow = nrow(lv1.data), ncol = length(tmpheight))
    for (i in 1:length(tmpheight)) {
      tmp1[, i] <- sensor_above > tmpheight[i]
    }

    ###############
    # set flag 8
    tmp2 <- lv1.data[, I + 1]
    tmp2[tmp1] <- 8
    lv1.data[, I + 1] <- tmp2
  }
  return(lv1.data)
}

