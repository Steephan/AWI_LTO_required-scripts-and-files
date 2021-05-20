###.........................................................................
#
#   Create "check files" for all combinations of stations, years, data sets and variables in the database
#
#   Run this script if new stations, years, data sets or variables are included in the database
#
#   For the first set-up of the files uncomment the section A which is to include already existing checks. See comment in the script.
#
#
#   christian.lehr@awi.de
#   last modified: ----
#
#   2021-05-14 SL add script to run on checker app
#   2020-07-29 CL add an option in the "l-loop" for the case that there is no existing entry in the file ==> keep the entry of the new check table
#
#
#
#
# pathes -----
###.........................................................................
# to run this script separately, you have to uncomment the next 10 lines!
# running.system = 1
# if (running.system == 1) {
#   # read paths and allowed variables for windows
#   yearlyDataPath <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv",
#                                  stringsAsFactors = FALSE, strip.white = TRUE)
#   allowedVariables   <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
#                                  stringsAsFactors = FALSE, strip.white = TRUE)
#   filterbasepath     <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
#   checkbasepath      <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
#   # read file for modification of style of shiny-app
#   source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
# } else if (running.system == 2) {
#   # read paths and allowed variables for linux
#   yearlyDatasetPaths <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_AWI.csv", stringsAsFactors = FALSE,
#                                  strip.white = TRUE)
#   allowedVariables <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv", stringsAsFactors = FALSE,
#                                strip.white = TRUE)
#   filterbasepath     <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
#   checkbasepath      <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
#   # read file for modification of style of shiny-app
#   source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
# }
###.........................................................................



stations <- c("Bayelva" = "Ba","Kurungnakh" = "Ku", "Samoylov" = "Sa", "Sardakh" = "Sd", "TVC" = "TVC")

# Define which years to check:
# The check period starts with the first year and ends with the last complete year before the recent year.
recent.year <- as.numeric(format(Sys.Date(), "%Y"))
check.end <- recent.year - 1
years <- list(1998:check.end, 2013:2018, 1998:check.end, 2009:check.end, 2016:check.end)

# loop to create files for each station - year combination with file names stationShortcut_check_year: example: Ba_check_2009 -----
# ==> thus the file comprises all variables measured at that station
for (i in 1:length(stations)) {
  station <- names(stations)[i]
  for (j in 1:length(years[[i]])) {
    # structure of empty table for checking of data sets
    df <- data.frame(station = character(0), dataset = character(0), variable = character(0), check1 = character(0),
                     controller1 = character(0), check2 = character(0), controller2 = character(0))
    ind.datasets <- which(station == as.character(yearlyDataPath$station) & (years[[i]][j] == yearlyDataPath$year))
    select.datasets <- trimws(as.character(yearlyDataPath$dataset[ind.datasets]), "l")
    for (k in 1:length(select.datasets)) {
      dataset <- select.datasets[k]
      # index of variables in allowedVariables for the given combination of dataset and year
      ind.var <- which(allowedVariables$dataset == dataset)
      # perform only in case that there are variables
      if (length(ind.var) > 0) {
        for (l in 1:length(ind.var)) {
          variable <- trimws(as.character(allowedVariables$variable[ind.var[l]]), "l")

          # case 1: before 2018: set check1 "2018-01-01" and controller1 "SL"
          if (years[[i]][j] < 2018) {
            newline <- list(station = station, dataset = dataset, variable = variable, check1 = "2018-01-01", controller1 = "SL", check2 = NA, controller2 = NA) }
          # case 2: from 2018 on: create empty check fields for check1, controller1, check2, controller2
          if (years[[i]][j] >= 2018) {
            newline <- list(station = station, dataset = dataset, variable = variable, check1 = NA, controller1 = NA, check2 = NA, controller2 = NA)
          }
          # convert data format of columns of data.frame to character format before adding new rows
          # https://stackoverflow.com/questions/29035508/r-add-character-vector-to-data-frame-row
          indx <- sapply(df, is.factor)
          df[indx] <- lapply(df[indx], as.character)
          df <- rbind(df, newline)# tab[nrow(tab) + 1, ] <- newline
          df[] <- lapply(df, type.convert)
        }
      }
    }
    # convert all columns to character format. !! use df[] to keep data.frame structure.
    df[] <- lapply(df[], as.character)
    # or only for columns in factor format df[] <- lapply(df, type.convert, as.is = TRUE)

    ###.........................................................................
    ### Section A: Inclusion of already performed checks! Prevent that existing checks are overwritten!------
    ### For the first set-up of the files uncomment this section.
    ### Update of the check table. Combine the existing check table with the newly created check table.
    if (file.exists(paste(checkbasepath, stations[i], "_check_", years[[i]][j], ".dat", sep = ""))) {
      df.exist <- read.table(file = paste(checkbasepath, stations[i], "_check_", years[[i]][j], ".dat", sep = ""), dec = ".", sep = ",", header = TRUE)
      # convert all columns to character format. !! use df[] to keep data.frame structure.
      df.exist[] <- lapply(df.exist[], as.character)
      for (l in 1:nrow(df)) {
        # index of row l of df in df.exist
        ind <- which((df[l, "dataset"] == df.exist[, "dataset"]) & (df[l, "variable"] == df.exist[, "variable"]))
        # if there is an existing entry:
        # Keep the entries of the existing check table and overwrite the new check table.
        if (length(ind) == 1) {
          df[l, ] <- df.exist[ind, ]
        }
        # if there is no existing entry:
        # keep the entry of the new check table
        if (length(ind) == 0) {
          df[l, ] <- df[l, ]
        }
      }
    }
    ###.........................................................................
    write.table(df, file = paste(checkbasepath, stations[i], "_check_", years[[i]][j], ".dat", sep = ""), row.names = FALSE, dec = ".", sep = ",")
  }
}


# loop to create files for each station with file names stationShortcut_check_complete: example: Ba_check_complete -----
# ==> thus the file comprises all variables measured at that station
for (i in 1:length(stations)) {
  station <- names(stations)[i]
  # structure of empty table for checking of COMPLETE data sets with shiny app trendsetter
  df <- data.frame(station = character(0), dataset = character(0), variable = character(0), begin = character(0), end = character(0), check3 = character(0), controller3 = character(0))
  #which(substr(allowedVariables$dataset, 1, 2) == station)
  ind.datasets <- which(station == as.character(yearlyDataPath$station))
  # select each dataset only once
  select.datasets <- unique(trimws(as.character(yearlyDataPath$dataset[ind.datasets]), "l"))
  for (k in 1:length(select.datasets)) {
    dataset <- select.datasets[k]
    # index of variables in allowedVariables for the given combination of dataset and year
    ind.var <- which(allowedVariables$dataset == dataset)
    # perform only in case that there are variables
    if (length(ind.var) > 0) {
      for (l in 1:length(ind.var)) {
        variable <- trimws(as.character(allowedVariables$variable[ind.var[l]]), "l")
        newline <- list(station = station, dataset = dataset, variable = variable, begin = NA, end = NA, check3 = NA, controller3 = NA)

        # convert columns of data.frame before adding new rows
        # https://stackoverflow.com/questions/29035508/r-add-character-vector-to-data-frame-row
        indx <- sapply(df, is.factor)
        df[indx] <- lapply(df[indx], as.character)
        df <- rbind(df, newline)# tab[nrow(tab) + 1, ] <- newline
        df[] <- lapply(df, type.convert)
      }
    }
  }
  # convert all columns to character format. !! use df[] to keep data.frame structure.
  df[] <- lapply(df[], as.character)
  # or only for columns in factor format df[] <- lapply(df, type.convert, as.is = TRUE)

  ###.........................................................................
  ### Section A: Inclusion of already performed checks! Prevent that existing checks are overwritten! ----
  ### For the first set-up of the files uncomment this section.
  ### Update of the check table. Combine the existing check table with the newly created check table.
  if (file.exists(paste(checkbasepath, stations[i], "_check_complete.dat", sep = ""))) {
    df.exist <- read.table(file = paste(checkbasepath, stations[i], "_check_complete.dat", sep = ""), dec = ".", sep = ",", header = TRUE)
    # convert all columns to character format. !! use df[] to keep data.frame structure.
    df.exist[] <- lapply(df.exist[], as.character)
    for (l in 1:nrow(df)) {
      # index of row l of df in df.exist
      ind <- which((df[l, "dataset"] == df.exist[, "dataset"]) & (df[l, "variable"] == df.exist[, "variable"]))
      # if there is an existing entry:
      # Keep the entries of the existing check table and overwrite the new check table.
      if (length(ind) == 1) {
        df[l, ] <- df.exist[ind, ]
      }
      # if there is no existing entry:
      # keep the entry of the new check table
      if (length(ind) == 0) {
        df[l, ] <- df[l, ]
      }
    }
  }
  ###.........................................................................
  write.table(df, file = paste(checkbasepath, stations[i], "_check_complete.dat", sep = ""), row.names = FALSE, dec = ".", sep = ",")
}
