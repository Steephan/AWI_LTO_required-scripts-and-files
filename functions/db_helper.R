###############################################################################
#
# helper functions everything except filter & plotting
#
# by: stephan.lange@awi.de
# last modified: 2016-04-25
#
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




###############################################################################
options(origin = "1970-01-01")
library("caTools")
library("zoo")
###############################################################################
#
# weekly statistics
#

####################################################################################################
####        table flag distribution          ####
####################################################################################################
# Flag meanings (aus mail von Julia):
#        0 value ok, not flagged
# Table Flags 	raw == > level0 	 1 no measurement
#        2 interval to short, line before flagged line deleted
# Filter Flags 	level == > level1 4 constant values
#        5 	value coerced, because above or below physical limits
#        6 	delete noise filter
#        7 	gradient too strong
# Olaf Ippisch Mask
#        12 	leicht verdächtig, das wird zum Beispiel verwendet wenn der
#         Wassergehalt berechnet wird und die Temperatur unter Null Grad lag.
#        13 	?
#        14 	au?erhalb von sinnvollen Grenzen
#        15 	Systemstörung

flagmeanings <- c(0, 1, 2, 4, 5, 6, 7, 12, 13, 14, 15) #### very old!!!!!!!!!!!!!!!!!!!!!!11
names(flagmeanings) <- c("value ok", "no measurement", "interval too short, line before flagged line deleted",
       "constant values", "value coerced, bc. above or below physical limits",
       "delete noise filter", "gradient too strong", "slightly suspicious", "?",
       "outside meaningful limits", "system error")
##########
createFlaghist <- function(d) {
 tmp <- reshape(d,
     direction = "long",
     idvar = "UTC",
     varying = 2:ncol(d),
     v.names = "flag",
     timevar = "sensor",
     times = names(d)[2:ncol(d)])

 flaghist <- within(as.data.frame(table(tmp$flag)), {
 Meaning <- names(flagmeanings[flagmeanings %in% Var1])
 Perc <- round(Freq / sum(Freq) * 100, 2)
 })
 names(flaghist) <- c('Flag', 'Frequency', 'Percentage', 'Meaning')
 rownames(flaghist) <- NULL
 return(flaghist)
}

addDateDetails <- function(d) {
 # Derives date details from POSIXct times
 d$hour <- as.POSIXlt(d$UTC)$hour
 d$day <- as.POSIXlt(d$UTC)$yday + 1
 d$monthday <- as.POSIXlt(d$UTC)$mday
 d$month <- factor(as.POSIXlt(d$UTC)$mon + 1)
 d$year <- as.POSIXlt(d$UTC)$year + 1900
 d$week <- round(d$day / 7)
 d$datum <- as.Date(d$UTC)
 return(d)
}

naStats <- function(d) {
 # Calculate NA statistics
 NAcount  <- apply(d, 2, function(x) {sum(is.na(x))})
 percentages <- round(NAcount / nrow(d) * 100, 2)
 out <- data.frame(Sensor = names(NAcount), NAcount, Percentages = percentages)
 rownames(out) <- NULL
 return(out)
}

longestGaps <- function(d, n = 5, dateformat = '%Y-%m-%d %H:%M') {
  # Calculate $n longest gaps in a data frame. Expects first col to be an ID.
  # To show all gaps, set n = 0.
  if (!is.data.frame(d)) {
    # warning("Coercing argument to data.frame")
    d <- data.frame(d)
  }
  d$missing <- FALSE
  # add missing if one or more values in a row are NA, arr.ind needed to get rows
  if (anyNA(d)) {
    d[which(is.na(d), arr.ind = T)[, 1], ]$missing <- TRUE
  } #else {
  # warning('No NAs found in argument.')
  # return(NULL)
  #}
  # calculate streak length with a nice command from http://stackoverflow.com/a/5016069/4700618
  #
  # lapply: for every streak, fill all except highest count with zero; c(rep(1, x-1), x)
  # do this for every streak and flatten the resulting list with unlist
  # multiply with d$missing to remove streaks of FALSE
  d$streak <- (d$missing * unlist(lapply(rle(d$missing)$lengths, function(x) {c(rep(0, x - 1), x)})))
  if (n) {
    orderedStreaks <- head(d[order(d$streak, decreasing = T), ], n) ## n longest streaks
  } else {
    orderedStreaks <- d[order(d$streak, decreasing = T), ]
  } ## all streaks

  orderedStreaks <- orderedStreaks[orderedStreaks$streak > 0, ] ## Remove streaks of zero
  if (nrow(orderedStreaks) > 1) {
    if (ncol(d) > 1) {
    out <- data.frame(from = NA, to = orderedStreaks[, 1], gaps = orderedStreaks$streak)
    out$from <- out$to - (out$gaps * 60 * 60 - 1)
    } else {
    # return starting ID for the gap-streak and according gap-length
    out <- data.frame(ID = as.numeric(rownames(orderedStreaks)) - orderedStreaks$streak,
                      gaps = orderedStreaks$streak)
    }
  } else {
    out <- data.frame(from = d[1, 1], to = d[2, 1], gaps = 0)
  }
  return(out)
}


##########
# Find rows where all values are the same, compares the first value with
# the rest of the values and checks if all are true (after removing NAs)
sameRows <- function(d) {
 x <- apply(d[, -1], 1, function(x) {
 x <- x[!(is.na(x))]
 if (length(x) <= 3) return(FALSE) # Prevents all(logical(0)) = TRUE and few measurements (false positives)
 return(all(x == x[1]))
 })
 names(x) <- NULL
}
##########
#
#

convertToDokuwikiTable <- function(d) {
  if (!is.data.frame(d)) {
    #warning("Coercing argument to data.frame")
    d <- data.frame(d)
  }
  header <- ""
  if (!is.null(names(d))) {
    header <- paste0("^ ", paste(names(d), collapse = " ^ "), " ^")
  }
  body <- paste0(apply(d, 1, function(x) {paste0("| ", paste(x, collapse = " | "), " |")}),
                 collapse = "\n")
  output <- paste(header, body, sep = "\n")
  return(output)
}

##############################################################################

wind.mean <- function(data) {
 # function to calculate the mean of angels in deg
 # mean(c(350, 10))should be 0 (not 180)
  if (length(na.omit(data)) < 1) {
    return(NA)
  } else {
    u = mean(sin(na.omit(data) * pi / 180))
    v = mean(cos(na.omit(data) * pi / 180))
    if (u >= 0 & v >= 0) bub <- ((180 / pi) * atan(abs(u)/abs(v)))
    if (u >= 0 & v < 0) bub <- 180 - ((180 / pi) * atan(abs(u)/abs(v)))
    if (u < 0 & v >= 0) bub <- 360 - ((180 / pi) * atan(abs(u)/abs(v)))
    if (u < 0 & v < 0) bub <- 180 + ((180 / pi) * atan(abs(u)/abs(v)))
    return(bub)
  }
}

###############################################################################

get.flag.columns <- function(table) {
  #### not used anymore, see get100flag.column #####
 # putting after each column a new flag-column
 db.lvl1 <- table
 colz <- dim(table)[2] - 1
 # create new flag_columns with default flag NA and named "flag_sensor" for each sensor
 for (i in 2:length(colnames(table))) {
 db.lvl1$new <- "0"
 colnames(db.lvl1)[ length(db.lvl1) ] <- paste0(colnames(db.lvl1)[i], "_fl")
 }
 # replace dots '.' (generated by colnames) with '()'
 colnames(db.lvl1) <- gsub(x = colnames(db.lvl1), pattern = "\\.", replacement = ")")
 for (i in 1:colz) {
 colnames(db.lvl1) <- gsub(x = colnames(db.lvl1), pattern = paste0(")", i), replacement = paste0("(", i) )
 }
 # reorder dataframe with flag column just after each sensor
 db.lvl1 <- db.lvl1[, c(1, c(t(matrix(nrow = colz, ncol = 2, c(2:colz, (colz + 1):(2 * colz + 1)))[])))]
 return(db.lvl1)
}




snow.gradient <- function(vector) {
  #### not used anymore?! ####

  ### LEVEL1 flag = 7
  ### gradient to strong?
  vec.new <- c(1:length(vector))
  vec.new[1] <- 0
  for (zack in 2:length(vector)) {
    if (is.na(vector[zack - 1]) == TRUE) {
      vec.new[zack] <- NA
    } else if (is.na(vector[zack - 1]) == TRUE) {
      vec.new[zack] <- NA
    } else {
      vec.new[zack] <- abs(vector[zack] - vector[zack - 1])
    }
  }
  return(vec.new)
}

###############################################################################

na.median <- function(x) {
 # function to calculate median without NA-values
  if (length(na.omit(x)) > 0) {
    median(na.omit(x))
  } else {
    NA
  }
}
###############################################################################

na.mean <- function(x) {
 # function to calculate mean without NA-values
  if (length(na.omit(x)) > 0) {
    mean(na.omit(x))
  } else {
    NA
  }
}
###############################################################################

na.min <- function(x) {
 # function to calculate mean without NA-values
  if (length(na.omit(x)) > 0) {
    min(na.omit(x))
  } else {
    NA
  }
}
###############################################################################

na.max <- function(x) {
 # function to calculate mean without NA-values
  if (length(na.omit(x)) > 0) {
    max(na.omit(x))
  } else {
    NA
  }
}
###############################################################################
na.sum <- function(x) {
 # function to calculate sum without NA-values
  if (length(na.omit(x)) > 0) {
    sum(na.omit(x))
  } else {
    NA
  }
}
###############################################################################
na.minus <- function(x1, x2) {
 # function to calculate sum without NA-values
  if (length(na.omit(x2)) > 0) {
    x3 <- x2
    x3 <- NA
    x3[!is.na(as.numeric(x2))] <- x1 - na.omit(as.numeric(x2))
    x3
  } else {
    NA
  }
}
###############################################################################

as.num <- function(x) {
 # sometimes necessary!
 as.numeric(as.character(x))
}

###############################################################################



lag_apply <- function(x, n, func = na.median) {
  #result  =  rep(0, length(x))
  result <- rep(func(x[(length(x) - n  + 1)]), length(x))
  for (i in 1:(length(x) - n + 1)) {
    result[i] <- func(x[i:(i + n - 1)])
  }
  return(result)
}

lag_apply2 <- function(x, n, func = na.median) {
  # better as lag_apply -> search in center
  #result  =  rep(0, length(x))
  result <- rep(func(x[(length(x) - n  + 1)]), length(x))
  for (i in (1 + n):(length(x) - n)) {
    result[i] <- func(na.omit(x[(i - n):(i + n)]))
  }
  return(result)
}


wind.freq <- function(speed, direction) {
 # create a frequency table for windrose()
 # see: db_plotter.R
 w <- data.frame(speed, direction)
 w <- w[complete.cases(w), ]
 wind.frec <- matrix(ncol = 16, nrow = 4)
 for (i in 1:16) {
   if (i == 1) {
    wind.frec[1, i] <- length(w$speed[ (w$direction >= 348.5) & (w$speed <= 3) ]) +
                       length(w$speed[ (w$direction <= 11.5) & (w$speed <= 3) ])
    wind.frec[2, i] <- length(w$speed[ (w$direction >= 348.5) & (w$speed > 3) & (w$speed <= 6) ]) +
                       length(w$speed[ (w$direction <= 11.5) & (w$speed > 3) & (w$speed <= 6) ])
    wind.frec[3, i] <- length(w$speed[ (w$direction >= 348.5) & (w$speed > 6) & (w$speed <= 9) ]) +
                       length(w$speed[ (w$direction <= 11.5) & (w$speed > 6) & (w$speed <= 9) ])
    wind.frec[4, i] <- length(w$speed[ (w$direction >= 348.5) & (w$speed > 9) ]) +
                       length(w$speed[ (w$direction <= 11.5) & (w$speed > 9) ])
   } else {
    wind.frec[1, i] <- length(w$speed[ (w$direction >= (i - 1) * 22.5 - 11) &
                                       (w$direction <= (i - 1) * 22.5 + 11) &
                                       (w$speed <= 3) ])
    wind.frec[2, i] <- length(w$speed[ (w$direction >= (i - 1) * 22.5 - 11) &
                                       (w$direction <= (i - 1) * 22.5 + 11) &
                                       (w$speed > 3) & (w$speed <= 6) ])
    wind.frec[3, i] <- length(w$speed[ (w$direction >= (i - 1) * 22.5 - 11) &
                                       (w$direction <= (i - 1) * 22.5 + 11) &
                                       (w$speed > 6) & (w$speed <= 9) ])
    wind.frec[4, i] <- length(w$speed[ (w$direction >= (i - 1) * 22.5 - 11) &
                                       (w$direction <= (i - 1) * 22.5 + 11) &
                                       (w$speed > 9) ])
   }
 }
 rownames(wind.frec) <- c("0-3", "3-6", "6-9", ">9")
 colnames(wind.frec) <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
 return(wind.frec)
}


## slider functions (moving window)
slider <- function(vec,from = 1, to = length(vec), windowsize = 5, multiplicator = 1) {
  vac <- which(!is.na(vec[from:to]))
  outlier <- (from - 1) +
    which( abs((vec[vac] - lag_apply2(vec[from:to], windowsize, na.median)[vac])) >=
             (multiplicator * (lag_apply2(vec[from:to], windowsize * 2))) )
  #return(outlier[1:(length(outlier)-(windowsize-1))])
  return(outlier[])
}

slider2 <- function(vec, from = 1, to = length(vec), windowsize = 5, multiplicator = 3) {
  outlier <- (from - 1) +
    which( abs((vec[from:to] - lag_apply(vec[from:to], windowsize))) >=
             (multiplicator * sd(lag_apply(vec[from:to], windowsize * 3))) )
  return(outlier[1:(length(outlier) - (windowsize - 1))])
}

slider3 <- function(vec, from = 1, to = length(vec), windowsize = 5, multiplicator = 1) {
  vac <- which(!is.na(vec[from:to]))
  outlier <- (from - 1) +
    which( abs((vec[vac] - lag_apply2(vec[from:to], windowsize)[vac])) >
             (multiplicator * sd(na.omit((vec[vac] - lag_apply2(vec[from:to], windowsize)[vac])))) )
  return(outlier[1:(length(outlier) - (windowsize - 1))])
  #return(outlier[])
}

#
smoother <- function(vec, windowsize = 5, multiplicator = 1) {
  outlier <- which( abs((vec - lag_apply2(vec, windowsize))) >
                      (multiplicator * sd(na.omit((vec - lag_apply2(vec, windowsize))))) )
  #return(outlier[windowsize:(length(outlier)-(windowsize-1))])
  return(outlier[])
}

# whole year removes the values of the year sd removes windowsize at begin and end
smoother2 <- function(vec,windowsize = 5, multiplicator = 1) {
  outlier <- which( abs((vec - lag_apply2(vec, windowsize))) >
                      (multiplicator * sd(na.omit((vec - lag_apply2(vec, windowsize))))) )
  #return(outlier[windowsize:(length(outlier)-(windowsize-1))])
  return(outlier[-c(1:windowsize, (length(vec) - windowsize):length(vec))])
}

# deviation from median
smoother3 <- function(vec, windowsize = 5, deviation = 1) {
  outlier <- which(abs((vec - lag_apply2(vec, windowsize))) > deviation)
  #return(outlier[windowsize:(length(outlier)-(windowsize-1))])
  return(outlier[])
}


###############################################################################


t.dep <- function(te.dep) {
 # Equation to calculate the temperature dependency of the dielectric constant of water
 # author: Christian Budach
 # Alfred-Wegener-Institute Potsdam, February 2015

 # +++ Reference: Handbook of Physics and Chemistry, 1986.
 # based on equation (8) of Roth, K., Schulin, R., Flueler, H., Attinger, W. (1990):
 # "Calibration of Time Domian Reflectometry for Water Content Measurement Using a
 # Compsite Dielectric Approach"
 return(78.54 * (1 - 4.579 * 10^(-3) * (te.dep - 25) + 1.19 * 10^(-5) * (te.dep - 25)^2 - 2.8 * 10^(-8) * (te.dep - 25)^3))}

###############################################################################

run.mean <- function(x, k, alg = c("C", "R", "fast", "exact"),
                     endrule = c("mean", "NA", "trim", "keep", "constant", "func"),
                     align = c("center", "left", "right")) {
  alg <- match.arg(alg)
  endrule <- match.arg(endrule)
  align <- match.arg(align)
  dimx <- dim(x)
  x <- as.vector(x)
  n <- length(x)
  if (k <= 1)
  return(x)
  if (k > n)
  k <- n
  k2 <- k %/% 2
  if (alg == "exact") {
    y <- .C("runmean_exact", x, y = double(n), as.integer(n),
    as.integer(k), NAOK = TRUE, PACKAGE = "caTools")$y
  } else if (alg == "C") {
    y <- .C("runmean", as.double(x), y = double(n), as.integer(n),
    as.integer(k), NAOK = TRUE, PACKAGE = "caTools")$y
  } else if (alg == "fast") {
    y <- .C("runmean_lite", as.double(x), y = double(n),
    as.integer(n), as.integer(k), NAOK = TRUE, PACKAGE = "caTools")$y
  } else {
    y <- double(n)
    k1 <- k - k2 - 1
    y <- c(sum(x[1:k]), diff(x, k))
    y <- cumsum(y) / k
    y <- c(rep(0, k1), y, rep(0, k2))
    if (endrule == "mean")
    endrule <- "func"
  }
   y <- EndRule(x, y, k, dimx, endrule, align, mean, na.rm = TRUE)
   return(y)
}

