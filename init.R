CSV_PATH <- "./data/source"
DB_PATH <- "./data/db"


#' tideHeight
#'
#' Calculates the tide height at a given time based on the tide heights at
#' the start and end of the tide period.
#'
#' @param t The query time to calculate the tide height for.
#' @param t1 The start time of the tide period.
#' @param t2 The end time of the tide period.
#' @param h1 The tide height at the start of the tide period.
#' @param h2 The tide height at the end of the tide period.
#'
#' @return The tide height at the given time in metres.
#'
#' NOTE: The time must be in decimal format, for example 12:30pm would be 12.5.
#'
tideHeight <-
  function(t, t1, t2, h1, h2) {
    A = pi * ((t - t1) / (t2 - t1) + 1)
    return(h1 + (h2 - h1) * (cos(A) + 1) / 2)
  }

#' heightTime
#'
#' Calculates the time at which the tide will be at a given height based on the
#' tide heights at the start and end of the tide period.
#'
#' @param h The tide height to calculate the time for.
#' @param t1 The start time of the tide period.
#' @param t2 The end time of the tide period.
#' @param h1 The tide height at the start of the tide period.
#' @param h2 The tide height at the end of the tide period.
#'
#' @return The time at which the tide will be at the given height.
#'
#' NOTE: The time must be in decimal format, for example 12:30pm would be 12.5.
#'
heightTime <-
  function(h, t1, t2, h1, h2) {
    A <- 2 * pi - acos(2 * (h - h1) / (h2 - h1) - 1)
    return(t1 + (t2 - t1) * (A / pi - 1))
  }


#' getPortCode
#'
#' Returns the port's code.
#'
#' @param df  A data.frame with the port details.
#' @param port  The required port (case sensitive)
#'
#' @return
#'
getPortCode <-
  function(df, port) {
    if (!all(c("port", "code") %in% colnames(df))) {
      stop("The data.frame must have columns named 'port' and 'code'.")
    }

    i <- which(df$port == port)

    if (length(i) == 0) {
      return(NULL)
    }

    return(df$code[i])
  }


#' decimalTime
#'
#' Converts a time in the format HH:MM to decimal format.
#'
#' @param t The time to convert.
#'
decimalTime <-
  function(t) {
    t_ <- strsplit(t, ":")
    return(unlist(lapply(t_, function(x) as.integer(x[1]) + as.integer(x[2]) / 60)))
  }


#' properTime
#'
#' Converts a time in decimal format to the format HH:MM.  If the
#' time is greater than 24 hours, the time will be wrapped around.
#'
#' @param t The decimal time to convert.
#'
properTime <-
  function(t) {
    if (any(t < 0)) {
      stop("Invalid time.")
    }

    h_ <- trunc(t)
    m_ <- round((t - h_) * 60)

    if (any(m_ == 60)) {
      h_[m_ == 60] <- h_[m_ == 60] + 1
      m_[m_ == 60] <- 0
    }

    if (any(h_ >= 24)) {
      h_[h_ >= 24] <- h_[h_ >= 24] - (24 * trunc(h_[h_ >= 24] / 24))
    }

    return(sprintf("%02d:%02d", h_, m_))
  }


#' tideAtMouseX
#'
#' @param tideHeightsDF  A data.frame with tide height information
#' @param clickX  The x-xoordinate where the mouse was clicked on the chart
#' @param timeAdj The decimal time adjustment applied to the chart
#'
#' @return A list with elements decimalTime (adjusted), properTime, date, and
#' height_m.
#'
tideAtMouseX <-
  function(tideHeightsDF, clickX, timeAdj) {
    if (!all(c("decimalTime", "height_m") %in% colnames(tideHeightsDF))) {
      stop("The data.frame must have columns named 'decimalTime' and 'height_m'.")
    }

    if (length(clickX) == 0) {
      return(NULL)
    }

    if (clickX < 0 | clickX > tideHeightsDF$decimalTime[nrow(tideHeightsDF)]) {
      return(NULL)
    }

    i1 <- tail(which(tideHeightsDF$decimalTime <= clickX), 1)
    i2 <- head(which(tideHeightsDF$decimalTime >= clickX), 1)
    y <- tideHeightsDF$height_m[i1] + ((clickX - tideHeightsDF$decimalTime[i1]) / (tideHeightsDF$decimalTime[i2] - tideHeightsDF$decimalTime[i1])) * (tideHeightsDF$height_m[i2] - tideHeightsDF$height_m[i1])

    if (tideHeightsDF$date[i1] == tideHeightsDF$date[i2]) {
      date <- tideHeightsDF$date[i1]
    } else {  # can only be bigger
      if (decimalTime(properTime(clickX + timeAdj)) <= 1) {
        date <- tideHeightsDF$date[i2]
      } else {
        date <- tideHeightsDF$date[i1]
      }
    }

   return(list(
      decimalTime = clickX,
      properTime = properTime(clickX + timeAdj),
      date = date,
      height_m = y))
  }


#' calcTides
#'
#' Calculates the data required to plot the tide chart for a particular port.
#'
#' @param port The port's name
#' @param qt The query time in 'YYYYMMDDHH:MM' format
#' @param offset By default the first 'segment' of the tide chart is the current
#' tide, this can be shifted by using the offset; +ve offset moves the current
#' tide to the left, -ve offset moves it to the right. Default 0.
#' @param periods The number of tide segments to show on the chart. Default 5.
#' @param portsTbl The data.frame with the port data.
#' @param tidesTbl The data.frame with the tides data.
#' @param minuteInterval The time frequency at which to plot tide height points.
#'
#' @return A list
#'
calcTides <-
  function(port, qt, offset = 0, periods = 5, portsTbl = mainPorts, tidesTbl = tides, minuteInterval = 6) {
    if (length(qt) == 0 | length(port) == 0) {
      return(NULL)
    }

    if (!grepl("^\\d{10}:\\d\\d$", qt)) {
      stop("Invalid query time format, expected 'YYYYMMDDHH:MM'.")
    }

    portCode <- getPortCode(portsTbl, port)

    if (is.null(portCode)) {
      stop("Invalid port name.")
    }

    if (!all(c("code", "height", "datetime", "isHighTide") %in% colnames(tidesTbl))) {
      stop("'tidesTbl' has missing columns.")
    }

    portRecords <- which(tidesTbl$code == portCode)
    firstPeriod <- tail(which(tidesTbl$code == portCode & tidesTbl$datetime <= qt), 1) + offset
    lastPeriod  <- head(which(tidesTbl$code == portCode & tidesTbl$datetime > qt), 1) + offset + periods - 1

    firstPeriod <- min(max(min(portRecords), firstPeriod), max(portRecords))
    lastPeriod  <- min(max(min(portRecords), lastPeriod), max(portRecords))

    if (firstPeriod >= lastPeriod) {
      stop("Invalid query time.")
    }


    lastPeriod <- lastPeriod - 1
    tideHeights <- vector("list", length = lastPeriod - firstPeriod + 1)

    qTime <- NA
    qHeight <- NA
    isRising <- NA

    for (i in firstPeriod:lastPeriod) {
      t1 <- decimalTime(tidesTbl$time[i])
      t2 <- decimalTime(tidesTbl$time[i + 1])
      if (t2 < t1) t2 <- t2 + 24

      t <-
        seq(from = t1,
            to = t2,
            by = minuteInterval / 60)

      if (tail(t, 1) == t2)
        t <- head(t, length(t) - 1)

      h1 <- tidesTbl$height[i]
      h2 <- tidesTbl$height[i + 1]

      h <- tideHeight(t, t1, t2, h1, h2)
      ht <- heightTime(h, t1, t2, h1, h2)

      iht <- rep(NA, length(h))
      dt <- rep(NA, length(h))
      iht[1] <- tidesTbl$isHighTide[i]
      tod <- properTime(ht)

      if (i == firstPeriod) {
        dt[1] <- substr(tidesTbl$datetime[i], 1, 8)
      }

      if (tidesTbl$date[i] != tidesTbl$date[i + 1]) {
        s_ <- which(tod[2:length(tod)] < tod[1:(length(tod) - 1)]) + 1
        if (length(s_) == 0 & tod[1] <= "01:00") {#s_ <- 1
          dt[1] <- tidesTbl$date[i + 1]
        }

        if (length(s_) > 0) {
          dt[s_] <- tidesTbl$date[i + 1]
        }
      } else {
        if (tod[1] <= "01:00")
          dt[1] <- tidesTbl$date[i + 1]
      }

      htOffset <- 0
      if (i > firstPeriod) {
        htOffset <- tideHeights[[(i - firstPeriod + 1) - 1]]$decimalTime[nrow(tideHeights[[(i - firstPeriod + 1) - 1]])]
        htOffset <- trunc(htOffset / 24) * 24

        while (ht[1] + htOffset < tideHeights[[(i - firstPeriod + 1) - 1]]$decimalTime[nrow(tideHeights[[(i - firstPeriod + 1) - 1]])]) {
          htOffset <- htOffset + 24
        }
      }

      if (firstPeriod - i == offset) {
        qdate <- as.integer(substr(qt, 1, 8))

        if (min(dt, na.rm = T) <= qdate & qdate <= max(dt, na.rm = T)) {
          qTime <- decimalTime(substr(qt, 9, 14))
          if (t1 <= decimalTime("23:59") & t2 > decimalTime("23:59") & qTime < t1)
            qTime <- qTime + 24
          qHeight <- tideHeight(qTime, t1, t2, h1, h2)
          qTime <- heightTime(qHeight, t1, t2, h1, h2) + htOffset
          isRising <- head(h, 1) < tail(h, 1)
        }
      }

      tideHeights[[(i - firstPeriod + 1)]] <-
        data.frame(
          date = dt,
          decimalTime = ht + htOffset,
          timeOfDay = tod,
          height_m = h,
          isHighTide = iht
        )
    }


    df <- do.call(rbind, tideHeights)
    dtAdjustment <- df$decimalTime[1]
    df$decimalTime <- df$decimalTime - dtAdjustment
    d_ <- which(!is.na(df$date))
    d_v <- df$date[d_]
    for (i in seq_along(d_)) {
      df$date[d_[i]:nrow(df)] <- d_v[i]
    }

    return(list(
      port = port,
      queryDateTime = qt,
      queryDecimalTime = qTime - dtAdjustment,
      queryHeight = qHeight,
      isRising = isRising,
      decimalTimeAdj = dtAdjustment,
      tideHeights = df
    ))
  }


## SUNRISE & SUNSET CALCULATIONS
## The following functions are used for sunrise and sunset calculations and
## are based off the document found here:
## https://gml.noaa.gov/grad/solcalc/solareqns.PDF

#' dayOfYear
#'
#' Calculates the day of the year, 1 Jan is 1, 2 Jan is 2, 1 Feb is 32, etc.
#'
#' @param date The date for which to calculate the day of year.
#'
#' @return
#'
dayOfYear <-
  function(date) {
    d1 <- paste0(format(as.Date(date, "%Y%m%d"), "%Y"), "0101")
    d1 <- as.Date(d1, "%Y%m%d")
    as.integer(difftime(as.Date(date, "%Y%m%d"), d1, units = "days")) + 1
  }


#' isLeapYear
#'
#' Calculates whether a year is a leap year or not.
#'
#' @param year The year to check, in YYYY format.
#'
#' @return
#'
isLeapYear <- function(year) {
  if (year %% 4 != 0) {
    return(FALSE)
  } else if (year %% 100 != 0) {
    return(TRUE)
  } else if (year %% 400 != 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


fractionalYear_rad <-  # radians
  function(dayOfYear, hour, year) {
    return((2 * pi) / (365 + isLeapYear(year)) * (dayOfYear - 1 + ((hour - 12) / 24)))
  }


equationOfTime <-  # minutes
  function(gamma) {
    return(229.18 * (0.000075 + 0.001868 * cos(gamma) - 0.032077 * sin(gamma) - 0.014615 * cos(2 * gamma) - 0.040849 * sin(2 * gamma)))
  }


solarDeclination_rad <-  # radians
  function(gamma) {
    return((0.006918 - 0.399912 * cos(gamma) + 0.070257 * sin(gamma) - 0.006758 * cos(2 * gamma) + 0.000907 * sin(2 * gamma) - 0.002697 * cos(3 * gamma) + 0.00148 * sin(3 * gamma)))
  }


timeOffset <- # minutes
  function(eqnOfTime_min, long_dec, timezone) {
    return(eqnOfTime_min + 4 * long_dec - 60 * timezone)
  }


solarHourAngle_rad <- # radians
  function(hr, mn, sec, timeOffset) {
    return((((hr * 60 + mn + sec / 60 + timeOffset) / 4) - 180) / (180 / pi))
  }


solarZenithAngle_rad <-
  function(lat_dec, decl_rad, hrAngle_rad) {
    lat_rad <- lat_dec / (180 / pi)

    return(acos(sin(lat_rad) * sin(decl_rad) + cos(lat_rad) * cos(decl_rad) * cos(hrAngle_rad)))
  }


solarAzimuth_rad <-
  function(lat_dec, decl_rad, hrAngle_rad, zenAngle_rad) {
    lat_rad <- lat_dec / (180 / pi)

    return(acos(-(sin(lat_rad) * cos(zenAngle_rad) - sin(decl_rad)) / (cos(lat_rad) * sin(zenAngle_rad))))
  }


timeAtAngle <-
  function(angle_deg, lon_dec, lat_dec, decl_rad, eqnOfTime_min) {
    lat_rad <- lat_dec / (180 / pi)

    x <- (cos(angle_deg / (180 / pi)) / (cos(lat_rad) * cos(decl_rad))) - (tan(lat_rad) * tan(decl_rad))
    ha <- acos(x) * (180 / pi)
    return(720 - 4 * (lon_dec + ha) - eqnOfTime_min)
  }


sunriseTime <-
  function(lon_dec, lat_dec, decl_rad, eqnOfTime_min) {
    lat_rad <- lat_dec / (180 / pi)

    x <- (cos(90.833 / (180 / pi)) / (cos(lat_rad) * cos(decl_rad))) - (tan(lat_rad) * tan(decl_rad))
    ha <- acos(x) * (180 / pi)
    return(720 - 4 * (lon_dec + ha) - eqnOfTime_min)
  }


sunsetTime <-
  function(lon, lat, decl_rad, eqnOfTime_min) {
    lat_rad <- lat / (180 / pi)

    x <- (cos(90.833 / (180 / pi)) / (cos(lat_rad) * cos(decl_rad))) - (tan(lat_rad) * tan(decl_rad))
    ha <- -acos(x) * (180 / pi)
    return(720 - 4 * (lon + ha) - eqnOfTime_min)
  }


solarNoon <-
  function(long, eqnOfTime_min) {
    return(720 - 4 * long - eqnOfTime_min)
  }


#' toDecLat
#'
#' Converts the Latitude from degrees and minutes to decimal degrees.
#'
#' @param lat The latitude in degrees, minutes (decimal) format, for example 23°45.3'N
#' @param deg The character used to denote the degrees. Default "°".
#' @param min The character used to denote the minutes. Default "'".
#' @param isNorthPositive Logical indicating whether Latitudes North of the
#' equator are denoted as positive. Default TRUE
#'
#' @return A numeric
#'
toDecLat <-
  function(lat, deg = "°", min = "'", isNorthPositive = T) {
  isNorth <- substr(lat, nchar(lat), nchar(lat)) == "N"

  lat <- gsub("(^\\s)|(\\s$)", "", lat)
  lat <- gsub(paste0(min, "[SN$]"), "", lat, perl = T, ignore.case = T)
  lat <- strsplit(lat, deg)

  lat <- unlist(lapply(lat, function(x) as.numeric(x[1]) + as.numeric(x[2]) / 60 ))
  lat <- ifelse(isNorth, lat, -lat)

  lat
  }


#' Title
#'
#' Converts the Longitude from degrees and minutes to decimal degrees.
#'
#' @param long The longitude in degrees, minutes (decimal) format, for example 123°45.3'W
#' @param deg The character used to denote the degrees. Default "°".
#' @param min The character used to denote the minutes. Default "'".
#' @param isEastPositive Logical indicating whether Longitudes East of the
#' Greenwich Meridian are denoted as positive. Default TRUE.
#'
#' @export
#'
toDecLong <-
  function(long, deg = "°", min = "'", isEastPositive = T) {
  isEast <- substr(long, nchar(long), nchar(long)) == "E"

  long <- gsub("(^\\s)|(\\s$)", "", long)
  long <- gsub(paste0(min, "[WE$]"), "", long, perl = T, ignore.case = T)
  long <- strsplit(long, deg)

  long <- unlist(lapply(long, function(x) as.numeric(x[1]) + as.numeric(x[2]) / 60 ))
  long <- ifelse(isEast, long, -long)

  long
  }


#' sunriseXY
#'
#' Calculates the sunrise time at a particular Longitude and Latitude on a
#' specific day of the year.  It also compensates for timezone.
#'
#' @param long_dec The longitude
#' @param lat_dec The latitude
#' @param doy The day of the year
#' @param yr The year, in YYYY format
#' @param timezone The hour adjustment for the timezone. Default 12.
#' @param daylightSavings A logical indicating whether daylight savings is to
#' be applied or not. Default FALSE.
#'
#' @return
#'
sunriseXY <-
  function(long_dec, lat_dec, doy, yr, timezone = 12, daylightSavings = F) {
    gamma <- fractionalYear_rad(doy, hour = 12, year = yr)
    et_min <- equationOfTime(gamma)
    decl_rad <- solarDeclination_rad(gamma)

    properTime(sunriseTime(long_dec, lat_dec, decl_rad, et_min) / 60 + timezone + daylightSavings)
  }


#' sunsetXY
#'
#' Calculates the sunset time at a particular Longitude and Latitude on a
#' specific day of the year.  It also compensates for timezone.
#'
#' @param long_dec The longitude
#' @param lat_dec The latitude
#' @param doy The day of the year
#' @param yr The year, in YYYY format
#' @param timezone The hour adjustment for the timezone. Default 12.
#' @param daylightSavings A logical indicating whether daylight savings is to
#' be applied or not. Default FALSE.
#'
#' @return
#'
sunsetXY <-
  function(long_dec, lat_dec, doy, yr, timezone = 12, daylightSavings = F) {
    gamma <- fractionalYear_rad(doy, hour = 0, year = yr)
    et_min <- equationOfTime(gamma)
    decl_rad <- solarDeclination_rad(gamma)

    properTime(sunsetTime(long_dec, lat_dec, decl_rad, et_min) / 60 + timezone + daylightSavings)
  }


#' tideChart
#'
#' Plots the tide chart for a specific port in base R graphics.
#'
#' @param port The port's name (case sensitive)
#' @param long_dec The port's longitude in decimal degrees
#' @param lat_dec The port's latitude in decimal degrees
#' @param queryDateTime The query time in YYYYMMDDHH:MM format
#' @param periods The number of tide segments to plot. Default 8.
#' @param offset The location of the 0th tide segment (the one applicable for
#' the current time)
#' @param dylt A data.frame with daylight saving dates and times.
#'
#' @return
#'
tideChart <-
  function(port, long_dec, lat_dec, queryDateTime, periods = 8, offset = 0, dylt = NULL) {

    xx <- calcTides(port, queryDateTime, periods = periods, offset = offset)

    if (is.null(xx) || is.na(xx$queryDecimalTime)) {
      plot(0,
        type = "n",
        xlim = range(0, 1),
        ylim = c(min(tides$height), 5.05),
        axes = F,
        ylab = "",
        xlab = "",
        main = paste0("[ERROR: NO DATA FOUND!]"),
        col.main = "red")

    } else {
      displayDates <- as.integer(unique(xx$tideHeights$date))

      xx$tideHeights$ghRise <- 0
      xx$tideHeights$ghSet <- 0

      { # sunrise / sunset block
        riseTime <-
          sunriseXY(
            long_dec,
            lat_dec,
            dayOfYear(as.character(displayDates)),
            as.integer(substr(queryDateTime, 1, 4)),
            timezone = ifelse(long_dec < 0, 12.75, 12),
            daylightSavings = unlist(lapply(displayDates, function(x) any(dylt$startDate <= x & x <= dylt$endDate)))
          )
        setTime <-
          sunsetXY(
            long_dec,
            lat_dec,
            dayOfYear(as.character(displayDates)),
            as.integer(substr(queryDateTime, 1, 4)),
            timezone = ifelse(long_dec < 0, 12.75, 12),
            daylightSavings = unlist(lapply(displayDates, function(x) any(dylt$startDate <= x & x <= dylt$endDate)))
          )

      if (!is.null(riseTime) & !is.null(setTime)) {
          riseDF <-
            data.frame(
              date = displayDates,
              time1 = properTime(decimalTime(riseTime) - 35/60),
              time2 = properTime(decimalTime(riseTime) + 35/60))
          setDF <-
            data.frame(
              date = displayDates,
              time1 = properTime(decimalTime(setTime) - 35/60),
              time2 = properTime(decimalTime(setTime) + 35/60))

          x_ <-
            apply(riseDF,
                  1,
                  function(x) {
                    which(xx$tideHeights$date == x[1] & xx$tideHeights$timeOfDay >= x[2] & xx$tideHeights$timeOfDay <= x[3])
                  }
            )

          if (any(c("matrix", "data.frame", "array") %in% class(x_))) {
            for (i in seq(ncol(x_))) {
              if (length(i) > 0) xx$tideHeights$ghRise[x_[, i]] <- max(xx$tideHeights$ghRise) + 1
            }
          } else {
            for (i in x_) {
              if (length(i) > 0) xx$tideHeights$ghRise[i] <- max(xx$tideHeights$ghRise) + 1
            }
          }

          x_ <-
            apply(setDF,
                  1,
                  function(x) {
                    which(xx$tideHeights$date == x[1] & xx$tideHeights$timeOfDay >= x[2] & xx$tideHeights$timeOfDay <= x[3])
                  }
            )

          if (any(c("matrix", "data.frame", "array") %in% class(x_))) {
            for (i in seq(ncol(x_))) {
              if (length(i) > 0) xx$tideHeights$ghSet[x_[, i]] <- max(xx$tideHeights$ghSet) + 1
            }
          } else {
            for (i in x_) {
              if (length(i) > 0) xx$tideHeights$ghSet[i] <- max(xx$tideHeights$ghSet) + 1
            }
          }
        }
      }

      old_par_mar <- par("mar")
      par(mar = c(6, 4, 6, 2) + 0.1)

      descr <- ""
        if (!is.na(xx$isRising) && xx$isRising) {
          descr <- c("Flood", "↑")
        } else if (!is.na(xx$isRising) && !xx$isRising) {
          descr <- c("Ebb", "↓")
        }


      hDiff <- abs(c(0, diff(xx$tideHeights$height_m)))
      s_ <- tail(which(xx$tideHeights$decimalTime <= xx$queryDecimalTime), 1)
      if (s_ == 1) s_ <- 2  # This is a hack
      hDiff <- hDiff[s_]
      t_d <- xx$tideHeights$decimalTime[s_] - xx$tideHeights$decimalTime[s_ - 1]
      hDiff <- round(hDiff / t_d, 2)
      if (length(hDiff) == 0) hDiff <- 0.0

      ## setting the main plot area
      plot(0,
          type = "n",
          xlim = range(xx$tideHeights$decimalTime),
          ylim = c(min(tides$height), 5.05), #max(tides$height)+.1),
          axes = F,
          ylab = "Height (m)",
          xlab = "Time (24hr)",
          main = paste0("Tide forecast for: ",
                        port,
                        "\n",
                        paste0("(", sprintf("%06.3f", round(lat_dec, digits = 3)), ", ", sprintf("%06.3f",round(long_dec, digits = 3)), ")"),
                        "\n",
                        format(as.Date(substr(queryDateTime, 1, 8), "%Y%m%d"), "%a %d %b %Y "),
                        substr(queryDateTime, 9, 13),
                        "\n\n[",
                        descr[1],
                        " tide @ ",
                        round(xx$queryHeight, 1),
                        "m ",
                        descr[2],
                        hDiff,
                        "m/hr",
                        "]"))

      mtext("• Raw tide data sourced from:\n   https://www.linz.govt.nz\n• Sunrise and sunset time calculations obtained from:\n   https://gml.noaa.gov/grad/solcalc/sollinks.html", side = 3, line = 2, adj = 0, col = "grey50")
      if (any(unlist(lapply(displayDates, function(x) any(dylt$startDate <= x & x <= dylt$endDate)))))
        mtext("Times are adjusted for NZ daylight savings where applicable.", side = 1, line = 4, adj = 1, col = "grey50")

      axis(2,
          las = 1,
          yaxp = c(0, round(max(tides$height, 0)),
          round(max(tides$height, 0))))
      axis(1,
          at = xx$tideHeights$decimalTime[!is.na(xx$tideHeights$isHighTide)],
          labels = xx$tideHeights$timeOfDay[!is.na(xx$tideHeights$isHighTide)])

      # area under the tide height curve
      polygon(x = c(xx$tideHeights$decimalTime, rev(xx$tideHeights$decimalTime)),
            y = c(xx$tideHeights$height_m, rep(-1, length(xx$tideHeights$height_m))),
            col = rgb(red   = 0x0e / 0xFF,
                      green = 0x17 / 0xFF,
                      blue  = 0xc7 / 0xFF,
                      alpha = 0.05),
            border = NA)

      ## high and low tide
      abline(v = xx$tideHeights$decimalTime[!is.na(xx$tideHeights$isHighTide)],
            col = rgb(red   = 0xD9 / 0xFF,
                      green = 0xD9 / 0xFF,
                      blue  = 0xD9 / 0xFF,
                      alpha = 0.35),
            lwd = 2)
      abline(h = 0:5,
            col = rgb(red   = 0xD9 / 0xFF,
                      green = 0xD9 / 0xFF,
                      blue  = 0xD9 / 0xFF,
                      alpha = 0.45),
            lwd = 2,
            lty = 3)

      ## tide height
      points(x = xx$tideHeights$decimalTime,
            y = xx$tideHeights$height_m,
            type = "l",
            ylim = c(min(tides$height), max(tides$height)),
            lwd = 3)

      ## sunrise, sunset and golden hour
      sunrise <- NULL
      sunset <- NULL
      if (max(xx$tideHeights$ghRise) > 0)
        for (i in 1:max(xx$tideHeights$ghRise)) {
          x_ <- xx$tideHeights$decimalTime[xx$tideHeights$ghRise == i]
          sunrise <- c(sunrise, head(x_, 1))
          for (j in seq_along(x_)) {
            polygon(x = c(x_[1], x_[j] + 0.0, x_[j] + 0.0, x_[1]),
                  y = c(-1, -1, 5.02, 5.02),
                  col = rgb(red   = 0xDD / 0xFF,
                            green = 0x85 / 0xFF,
                            blue  = 0x00 / 0xFF,
                            alpha = 0.10),
                  border = NA)
          }
          text(x = head(x_, 1) + 0,
              y = 4.55,
              label = paste("Morning\ngolden\nhour\n[",
                            riseTime[i], "]"),
              col = "black",
              cex = 0.95,
              pos = 2,
              offset = 0.5)

          y_ <- xx$tideHeights$height_m[xx$tideHeights$ghRise == i]
          lines(x = x_, y = y_, col = "#a100009f", lwd = 3.5)
        }

      if (max(xx$tideHeights$ghSet) > 0)
        for (i in 1:max(xx$tideHeights$ghSet)) {
          x_ <- xx$tideHeights$decimalTime[xx$tideHeights$ghSet == i]
          sunset <- c(sunset, tail(x_, 1))
          for (j in seq_along(x_)) {
            polygon(x = c(tail(x_, 1), x_[j] + 0.0, x_[j] + 0.0, tail(x_, 1)),
                  y = c(-1, -1, 5.02, 5.02),
                  col = rgb(red   = 0xFF / 0xFF,
                            green = 0xA5 / 0xFF,
                            blue  = 0x00 / 0xFF,
                            alpha = 0.10),
                  border = NA)
          }
          text(x = tail(x_, 1) - 0,
              y = 4.55,
              label = paste0("Evening\ngolden\nhour\n[",
                             setTime[i], "]"),
              col = "#000000",
              cex = 0.95,
              pos = 4,
              offset = 0.5)

          y_ <- xx$tideHeights$height_m[xx$tideHeights$ghSet == i]
          lines(x = x_, y = y_, col = "#a100009f", lwd = 3.5)
        }

      ## night polygon
      if ((length(sunrise) >= 1 && length(sunset) >= 1 && sunrise[1] < sunset[1]) ||
          (length(sunrise) >= 1 && length(sunset) == 0)) {
        polygon(x = c(0, sunrise[1], sunrise[1], 0),
              y = c(-1, -1, 5.02, 5.02),
              col = rgb(red   = 0x0 / 0xFF,
                        green = 0x0 / 0xFF,
                        blue  = 0x0 / 0xFF,
                        alpha = 0.125),
              border = NA)

        sunrise <- sunrise[-1]
      }

      if ((length(sunrise) >= 1 && length(sunset) >= 1 && tail(sunset, 1) > tail(sunrise, 1)) ||
          (length(sunrise) == 0 && length(sunset) >= 1)) {
        polygon(x = c(tail(sunset, 1), max(xx$tideHeights$decimalTime), max(xx$tideHeights$decimalTime), tail(sunset, 1)),
              y = c(-1, -1, 5.02, 5.02),
              col = rgb(red   = 0x0 / 0xFF,
                        green = 0x0 / 0xFF,
                        blue  = 0x0 / 0xFF,
                        alpha = 0.125),
              border = NA)

        sunset <- sunset[-length(sunset)]
      }

      if (length(sunrise) > 0 && length(sunset) > 0)
        for (i in 1:min(length(sunrise), length(sunset))) {
          polygon(x = c(sunrise[i], sunset[i], sunset[i], sunrise[i]),
                y = c(-1, -1, 5.02, 5.02),
                col = rgb(red   = 0x0 / 0xFF,
                          green = 0x0 / 0xFF,
                          blue  = 0x0 / 0xFF,
                          alpha = 0.125),
                border = NA)
        }

      ## query time
      if (!is.na(xx$isRising)) {
        hDiff <- abs(c(0, diff(xx$tideHeights$height_m)))[tail(which(xx$tideHeights$decimalTime <= xx$queryDecimalTime), 1)]
        hDiff <- max(0.30, hDiff / max(abs(diff(xx$tideHeights$height))))
      }

      if (!is.na(xx$isRising) && !xx$isRising) {
        points(x = xx$queryDecimalTime,
              y = xx$queryHeight,
              col = "#0ba80b",
              bg = "#ffffffc3",
              pch = 25,
              lwd = 3,
              cex = 2.25 * hDiff)

      } else if (!is.na(xx$isRising) && xx$isRising) {
        points(x = xx$queryDecimalTime,
              y = xx$queryHeight,
              col = "#ff0000",
              bg = "#ffffffc3",
              pch = 24,
              lwd = 3,
              cex = 2.25 * hDiff)
      }

      ## midnight line and date
      midnight <- xx$tideHeights$date
      midnight <- which(c(F, midnight[1:(length(midnight) - 1)] != midnight[2:length(midnight)]))
      midnight <- c(midnight - 1, midnight)  ## midnight - 1 will never result in a 0 since the first element is always FALSE
      midnight <- midnight[order(midnight)]
      dTime <- decimalTime(xx$tideHeights$timeOfDay[midnight]) + rep(c(0, 24), length(midnight) / 2)
      midnight <- xx$tideHeights$decimalTime[midnight]
      grps <- length(midnight) / 2
      grp <- rep(1:grps, 2)
      grp <- grp[order(grp)]
      midnight <- split(midnight, grp)
      dTime <- split(dTime, grp)

      midnight <-
        unlist(
          lapply(1:grps,
                 function(i) midnight[[i]][1] + ((24 - dTime[[i]][1]) / (dTime[[i]][2] - dTime[[i]][1])) * (midnight[[i]][2] - midnight[[i]][1]))
        )
      # drawing the midnight line
      abline(v = midnight,
             col = rgb(red   = 0x00 / 0xFF,
                       green = 0x00 / 0xFF,
                       blue  = 0x00 / 0xFF,
                       alpha = 0.35),
             lwd = 3,
             lty = 5)

      # adding the date past the midnight line
      midnight <- xx$tideHeights$date
      midnight <- which(c(F, midnight[1:(length(midnight) - 1)] != midnight[2:length(midnight)]))

      if (xx$tideHeights$timeOfDay[1] <= "18:00")
        midnight <- c(1, midnight)

      size <- 1.1
      for (i in unique(midnight)) {
        dt <- xx$tideHeights$date[i]
        dt <- format(as.Date(dt, "%Y%m%d"), "%a\n%d %b '%y")
        text(x = xx$tideHeights$decimalTime[i],
            y = -0.08,
            label = dt,
            col = "blue",
            cex = size,
            pos = 4,
            offset = 0.25)
        # size <- 0.95
      }

      axis(1,
          at = xx$tideHeights$decimalTime[!is.na(xx$tideHeights$isHighTide)],
          labels = xx$tideHeights$timeOfDay[!is.na(xx$tideHeights$isHighTide)])
      par(mar = old_par_mar)
    }

    xx$longDec <- long_dec
    xx$latDec <- lat_dec

    invisible(xx)
  }
