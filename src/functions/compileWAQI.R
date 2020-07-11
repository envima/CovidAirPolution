#' Compile PM information from WAQI.
#'
#' Data is provided by WAQI.
#' Geographic information is included based on station locations.

compileWAQI <- function(flist, pm = "PM10", start_date = as.POSIXct("2020-01-01")) {

  # Combine information with geographical infromation and move each station to
  # individual list and fill NAs.
  aq <- lapply(flist, function(f) {
    latlon <- getWAQIpos(readLines(f, n = 5))
    act <- read.table(f, skip = 0, header = TRUE, sep = ",")
    latlon <- as.numeric(c(latlon[2], latlon[1]))
    act$lat <- latlon[2]
    act$lon <- latlon[1]
    act$date <- as.POSIXct(act$date, origin = "CET")
    for (i in seq(2, ncol(act))) {
      act[, i] <- as.numeric(act[, i])
    }
    act$statname <- strsplit(basename(f), split = "\\.")[[1]][2]

    act$stationname <- act$statname
    act$stationcode <- NA
    act$TYPE_OF_AREA <- NA
    act$TYPE_OF_STATION <- NA

    if (pm == "PM2.5") {
      act <- act[, c(
        "stationcode", "date", "pm25", "TYPE_OF_AREA",
        "TYPE_OF_STATION", "stationname", "lat", "lon"
      )]
    } else if (pm == "PM10") {
      if ("pm10" %in% names(act)) {
        act <- act[, c(
          "stationcode", "date", "pm10", "TYPE_OF_AREA",
          "TYPE_OF_STATION", "stationname", "lat", "lon"
        )]
      } else {
        act <- NULL
      }
    }

    if (!is.null(act)) {
      lna <- which(is.na(act$pm))
      filled <- na.approx(act$pm, na.rm = FALSE)

      if (!is_empty(lna)) {
        if (lna[1] == 1) {
          first_valid <- which(!is.na(filled))[1]
          filled[1:(first_valid - 1)] <- filled[first_valid]
        }

        if (lna[length(lna)] == length(filled)) {
          last_valid <- tail(which(!is.na(filled)), 1)
          filled[(last_valid + 1):length(filled)] <- filled[last_valid]
        }
      }
      act$pm <- filled

      if (any(is.na(act$pm))) {
        print(act$stationcode)
      }

      act <- act[act$date >= start_date, ]
    }

    return(act)
  })

  names(aq) <- sapply(aq, function(x) x[1, "stationname"])

  aq <- compact(aq)

  return(aq)
}


#' Parse header of WAQI data.
#'
#' Parse header for required information.
#'

getWAQI_1 <- function(header) {
  return(header[1])
}

getWAQIpos <- function(header) {
  info <- str_extract_all(
    string = str_subset(header, "latitude"),
    pattern = "\\d+\\.*\\d*"
  )[[1]]
  latlon <- data.frame(
    Latitude = as.numeric(info[1]),
    Longitude = as.numeric(info[2])
  )
  return(latlon)
}
