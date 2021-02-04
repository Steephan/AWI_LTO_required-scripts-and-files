##################################################################################
# Equation to calculate the temperature dependency of the dielectric constant of water
# author:  Christian Budach
# Alfred-Wegener-Institute Potsdam, February 2015

# based on equation (8) of Roth, K., Schulin, R., Flueler, H., Attinger, W. (1990):
# "Calibration of Time Domain Reflectometry for Water Content Measurement Using a
# Composite Dielectric Approach"
# which refers to the Handbook of Physics and Chemistry, 1986.
#################################################################################


# dielectric constant of water at temperature temp:
# temperature in degree Celsius

# kt = 78.54 * [1 - 4.579 * 10^(-3) * (T - 25) + 1.19 * 10^(-5) * (T - 25)^2 - 2.8 * 10^(-8) * (T - 25)^3

t.dep <- function(t) {
  (78.54 * (1 - 4.579 * 10^(-3) * (t - 25) + 1.19 * 10^(-5) * (t - 25)^2 - 2.8 * 10^(-8) * (t - 25)^3))
}

##################################################################################
# Calculation of the volumetric water and ice content of the soil
# using dielectric permittivities - based on measured TDR data
# following a four-phase dielectric mixing model approach
#
# Author: Christian Budach
# +++ Alfred-Wegener-Institute Potsdam, July 2015
#
# last modifications:
#   2020-09-07 CL first variant of how to calculate theta_tot_save removed (this was not used in the script and overwritten with the current variant of how to calculate theta_tot_save)
#   2020-09-07 CL Threshold of mean_temp corrected to > 0.05 and <= 0.05 (before it was >= 0.05 ==> this means values = 0.05 were overwritten)
#   2020-08-06 CL theta_ice_raw added here as documentation which formerly negative theta_ice values are set to 0
#   2020-07-20 CL comments updated
#   2015-07-23 by Stephan Lange
#   Feb 2016 by Christian Budach (15.02.2016)
#
# based on equation (3) Roth, K., Schulin, R., Flueler, H., Attinger, W. (1990):
# "Calibration of Time Domain Reflectometry for Water Content Measurement Using a
# Composite Dielectric Approach", extended by a term representing the ice phase
##################################################################################
# Calculation of soil water content
# for a soil-water-air and a soil-water-ice-air system
# using the volumetric proportions and dielectric permittivities of each phase.
# Equations were changed using an averaged temperature threshold of 0.5 degree
# estimated by using a moving window.
##################################################################################
#
# eps_bulk: bulk permittivity (dielectricity), this is the sum of the permittivities of the liquid and solid phases of water (water and ice), soil and air (TDR data) ==> those are the actually measured permittivity values !!
# temp: soil temperature
# theta_tot_prior: water content from the previous year
# eps_air: permittivity (dielectricity) air, constant parameter: 1
# eps_ice: permittivity (dielectricity) ice, constant parameter: 1.96 * 1.96
# eps_liq_T: permittivity (dielectricity) liquid phase ==> water ==> dielectric constant of water at temperature temp calculated with function t.dep
# eps_soil: permittivity (dielectricity) soil, constant parameter: 1.96 * 1.96
# phi: porosity of soil
# alpha: constant 0.5 (alpha from Roth et al., (1990): geometry of the medium with relation to the orientation of the applied electrical field)
#     ==> power for the calculation of theta ~> square root
# mw.width: width of moving window to calculate vwc
# row_nr_freez_prior: row number when freezing period of the preceding year starts
#     ==> the begin of the freezing period is defined as last date with temperature > 0.05 degree Celsius

diel_to_vwc <- function(eps_bulk, temp, theta_tot_prior, eps_air, eps_ice, eps_liq_T, eps_soil, phi, alpha, mw.width, row_nr_freez_prior) {

  # define empty objects for parameters of the following calculations
  # number of rows
  nrows <- nrow(eps_bulk)
  # number of columns
  # ==> this is the number of different depths for which vwc is calculated
  ncols <- ncol(eps_bulk)
  # define empty matrices
  theta_liq <- matrix(NA, ncol = ncols, nrow = nrows)
  theta_ice <- matrix(NA, ncol = ncols, nrow = nrows)
  theta_tot <- matrix(NA, ncol = ncols, nrow = nrows)
  # define empty vectors
  theta_tot_seq <- NA
  theta_tot_save <- NA
  date_freez <- c(rep(NA, ncols))
  # row_nr_freez_prior is defined in the call of the function
  row_nr_freez <- row_nr_freez_prior

  ########################################
  # estimation of the average temperature for every time step using a moving window
  # mw.width: width of the moving window: this is defined in Lv0_to_LV1_BaAll.R ==> here time.res: the number of measurements per day

  # temp: temperatures
  # these average temperatures are used to set tipping points at which the calculation methods should be changed
  mean_temp <- rollapply(temp[, -1], width = mw.width, mean, na.rm = TRUE, fill = 0, align = 'center')
  mean_temp <- as.data.frame(matrix(unlist(mean_temp), nrow = nrow(temp)))
  colnames(mean_temp) <- colnames(temp)[-1]
  # replace zeros in front with first proper row
  tmp <- mean_temp[(mw.width / 2), ]
  tmp <- tmp[rep(seq_len(nrow(tmp)), each = (mw.width / 2)), ]
  rownames(tmp) <- 1:(mw.width / 2)
  mean_temp[1:(mw.width / 2), ] <- tmp[1:(mw.width / 2), ]
  # replace zeros in back with last proper row
  tmp <- mean_temp[nrow(mean_temp) - (mw.width / 2), ]
  colnames(tmp) <- colnames(mean_temp)
  tmp <- tmp[rep(seq_len(nrow(tmp)), each = (mw.width / 2 + 1)), ]
  rownames(tmp) <- rownames(tmp)[1]:nrow(temp)
  mean_temp[(nrow(mean_temp) - (mw.width / 2)):nrow(mean_temp), ] <- tmp

  # clean workspace
  rm(tmp)

  ########################################
  # Calculation of the soil volumetric water content (vwc)
  # equations based on the dielectric mixing model approach of Roth et al. 1990
  # converted from an approach of Paul Overduin

  for (j in 1:ncols) {

    ###############
    ## I: 3-phase-calculation of volumetric water content (only liquid phase, without ice):

    # for all dates
    for (i in 1:nrows) {
      # only for dates with non-NA temperatures
      if (is.na(mean_temp[i, j]) == F) {
        # calculation for all dates with mean temperatures > 0.05
        # ==> this is the threshold to assure that there is no ice, but only the liquid phase of the water
        if (mean_temp[i, j] > 0.05) {

          ######################
          ## 3-phase-calculation of volumetric water content (only liquid phase, without ice)
          ## This is equation 6 from the SPARC internal documentation of the TDR volumetric water content calculation by Christian Budach
          ## http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:tdr2
          ## This equation is based on a dielectric mixing model approach following Roth et al. (1990), see there: equation 11:
          ## theta_liq[i, j] <- (eps_bulk[i, j]^alpha - eps_soil^alpha - (1 - phi[j]) * eps_soil^alpha - phi[j] * eps_air^alpha) / (eps_liq_T[i, j]^alpha - eps_air^alpha)
          ##
          ## checked:
          ## 2020-06-29 CL
          ##
          ######################
          theta_liq[i, j] <- (eps_bulk[i, j]^alpha - eps_soil^alpha + phi[j] * (eps_soil^alpha - eps_air^alpha)) / (eps_liq_T[i, j]^alpha - eps_air^alpha)
        }
      }
    }
  #   cat(j,sum(!is.na(theta_liq[,j])) != 0)
  # }

###########
# theta_liq exhibits NA´s
# - if the mean_temp is NOT > 0.05 (second of the above if conditions)
# and for depths with
# - NA´s for temperature   ==> the above calculation is not performed (first of the above if condition)
# - NA´s for dielectricity ==> the above calculation is performed but gives NA because the first term is NA
#
# ==> ATTENTION:
# in db_vwc.R all soil temperature and dielectricity values with flags between 1 and 6 are set to NA
# This has the effect that the corresponding vwc values are set to NA
# and finally flagged with the smallest non-zero flag at the end of LV0_to_LV1_BaAll.R
# and not with flag 1 for "no data" !
###########

    ###############
    ## II: selecting indices for the following calculations

    # only if values were calculated using the 3-phase-equation
    if (sum(!is.na(theta_liq[, j])) != 0) {
      # y: indices where theta_liq values where calculated (non-NA values)
      y <- which(!is.na(theta_liq[, j]))
      # w: selection of indices of theta_liq values that are more than one time step apart from each other (==> gaps between theta_liq values)
      w <- y[which(diff(y, 1) > 1)]
      # x: indices of theta_liq values with gaps and of last date with temperature >= 0.05 (no ice)
      x <- c(w, y[length(y)])
      # theta_tot_seq: theta_liq values of last date (index) before the gaps and of last date with temperature >= 0.05 (no ice)
      theta_tot_seq <- theta_liq[x, j]
      # row_nr_freez[j]: row number (index) of the last date of the processed year j with temperature above the threshold of 0.05 degree
      # ===> after this date the freezing period starts
      row_nr_freez[j] <- x[length(x)]

      # theta_liq_from: index of the date 10 times the moving window width before the freezing period starts, but at least index 1
      # ==> this is solved here with the max function
      theta_liq_from <- max(c(row_nr_freez[j] - (mw.width * 10), 1))
      # theta_liq_to: index of the date 10 times the moving window width after the freezing period starts, but not further than the number of dates of theta_liq
      # ==> this is solved here with the min function and nrow (maximum row index)
      theta_liq_to <- min(c(row_nr_freez[j] + (mw.width * 10), nrow(theta_liq)))
      # theta_tot_save[j]: MAXIMUM theta_liq value of year j within the range of indices from theta_liq_from to theta_liq_to
      # ==> this will be the theta_tot_prior of the next year
      theta_tot_save[j] <- max(na.omit(theta_liq[theta_liq_from:theta_liq_to, j]))

      # data quality filtering: replace negative water contents with 0.01
      if (theta_tot_save[j] <= 0) {
        theta_tot_save[j] <- 0.01
      }
    } else {
      # else: NO values calculated with the 3-phase-equation
      # ==> thus NO periods with liquid water
      # ==> index x: is defined as the number of rows
      # ==> T < 0.05 for the whole year
      # ==> the theta_tot value of the preceding year is copied as value for the processed year
      # and x and theta_tot_seq are set to cover the whole year with the 4-phase calculation in the following section III
      x <- nrows
      theta_tot_seq <- 1
      theta_tot_save[j] <- theta_tot_prior[j]
    }

    ###############
    ## III: 4-phase-calculation of volumetric water content (with ice)

    # theta_tot: total water content of the processed year

    ####################
    # There are two cases, based on the calculation of the previous section (that is, if T >= 0.05 degree)
    #
    # CASE 1: If there were values calculated with the 3-phase-equation:
    #         x: indices of theta_liq values with gaps and of last date with temperature >= 0.05 (no ice)
    #         theta_tot_seq: theta_liq values with gaps and of last date with temperature >= 0.05 (no ice)
    #           ==> the time series is divided in k sections based on the number of gaps defined by x
    #           each section (sequence of values) gets its own theta_tot value
    #           as it is defined for the k sections in theta_tot_seq
    #
    # CASE 2: if NO values were calculated with the 3-phase-equation:
    #         the last value (index nrows) of theta_tot is set to 1
    #           ==> see the last else statement in the preceding section
    ####################

    # assignment of theta_tot values:
    # first section (k = 1):
    # for all values from 1 to the first entry of index vector x, theta_tot from the previous year is used
    theta_tot[1:x[1], j] <- theta_tot_prior[j]
    # for all other sections (k > 1), the respective theta_tot_seq value is assigned
    for (k in 1:length(x)) {
      theta_tot[x[k]:nrows, j] <- theta_tot_seq[k]
    }

    #######################
    ### What does this mean?
    ### The time series of theta_tot consists out of k (possibly just a few) different values for the k sections.
    ### Those are the theta_liq values collected in theta_tot_seq.
    ### Thus, each section k exhibits a constant theta_tot value.
    ###
    ### See also documentation http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:tdr2
    ### "theta_tot is kept constant..."
    ###
    ### The theta_tot values are used in the next step to calculate theta_liq (liquid water content) and theta_ice (ice content) with the four phase model.
    ### Basically, theta_tot is divided in theta_liq and theta_ice.
    #######################

    # loop runs again for all dates
    for (i in 1:nrows) {
      # if temperature is not NA
      if (is.na(mean_temp[i, j]) == F) {
        # calculation for all mean temperatures <= 0.05
        if (mean_temp[i, j] <= 0.05) {
          ######################
          ## 4-phase-calculation of volumetric water content (with ice)
          ## This is equation 7 from the SPARC internal documentation of the TDR volumetric water content calculation by Christian Budach
          ## http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:analysis:tdr2
          ## This equation is based on a dielectric mixing model approach following Roth et al. (1990, equation 11),
          ## extended by a term representing the ice phase.
          ##
          ## checked:
          ## 2020-06-29 CL
          ######################

          theta_liq[i, j] <- (eps_bulk[i, j]^alpha - eps_soil^alpha + phi[j]  *  (eps_soil^alpha - eps_air^alpha) + theta_tot[i, j]  *
                               (eps_air^alpha - eps_ice^alpha)) / (eps_liq_T[i, j]^alpha - eps_ice^alpha)

          ######################
          ## IV: Evaluating the volumetric ice content
          # using the simple assumption:
          # ice content = theta_tot - theta_liq
          theta_ice[i, j] <- theta_tot[i, j] - theta_liq[i, j]
        }
      }
    }
  }

  ####### End of the calculation

  ########################################
  # save the parameters for the criteria of changing the equations

  # names of the dielectricity sensors contain the depths of the sensors
  positions <- names(eps_bulk)

  # date and time of the last positive T of the year
  date_freez <- as.POSIXct(temp[row_nr_freez, 1], tz = "UTC", format = "%Y-%m-%d %H:%M")

  #########
  # additional condition: ice content cannot be negative
#########
# Attention! ==> this has the effect that for all values that were before theta_ice < 0
# theta_tot gets negative (the former negative theta_ice values)
# ==> theta_tot - theta_liq - theta_ice is not 0 any more
# theta_tot is not the sum of theta_ice and theta_liq any more
#
# because theta_ice is calculated as difference of theta_tot and theta_liq,
# this means that theta_tot was smaller than theta_liq already before
#
# ==> question: when does this happen?
#
# seems to be related to the moving window mean_temp
#
# ==> maybe in cases in which some temperature value within the moving window deviate stronger from the mean than others?
#  ? maybe especially at the beginning of the moving window periods??
#
#########
  # theta_ice_raw is added here as documentation which values are changed
  # this can be checked in db_vwc.R ==> see comments there
  theta_ice_raw <- theta_ice
  theta_ice[which(theta_ice[] < 0)] <- 0

  return(list(theta_liq = theta_liq, theta_ice = theta_ice, theta_tot = theta_tot, theta_ice_raw = theta_ice_raw,
              threshold = list(position = positions, date_freez = date_freez, row_nr_freez = row_nr_freez,
                             theta_tot = theta_tot_save, theta_tot_prior = theta_tot_prior)))

  # end of function
  ########################################
}
