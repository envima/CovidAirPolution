#' Compile PM dataset from UBA.
#'
#' Data is provided by the German Environmental Protection Agency (UBA).
#' Geographic information is included based on station locations.

compileUBA <- function(flist, start_date = as.POSIXct("2020-01-01")) {
  geo <- read.table(file.path(envrmt$path_DE, "station_All_convert.csv"),
    skip = 1, header = TRUE, sep = ";", dec = ","
  )
  geo <- geo[, c(1, 2, 13, 14)]
  names(geo) <- c("stationcode", "stationname", "lon", "lat")
  geo$stationcode <- as.character(geo$stationcode)

  uba <- read.table(flist, skip = 0, header = TRUE, sep = ";")
  uba$date <- as.POSIXct(as.character(uba$Datum),
    format = "%Y%m%d",
    origin = "CET"
  )

  # Aggregate hourly values and set -999 to NA.
  ch01 <- which(colnames(uba) == "Wert01")
  ch24 <- which(colnames(uba) == "Wert24")
  uba[, ch01:ch24][uba[, ch01:ch24] < 0] <- NA
  uba$pm <- rowMeans(uba[, ch01:ch24], na.rm = TRUE)
  uba$Station <- as.character(uba$Station)
  uba <- uba[, c(1, 32, 33, 4:5)]
  names(uba)[1:3] <- c("stationcode", "date", "pm")

  uba <- merge(uba, geo, by = "stationcode")
  uba$lat <- uba$lat
  uba$lon <- uba$lon

  # Move each station to individual list and fill NAs.
  aq <- lapply(unique(uba$stationcode), function(s) {
    act <- uba[uba$stationcode == s, ]

    act <- act[which(!duplicated(act$date)), ]

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
    act <- act[act$date >= start_date, ]
    if (any(is.na(act$pm))) {
      print(act$stationcode)
    }

    return(act)
  })

  names(aq) <- sapply(aq, function(s) s[1, "stationname"])

  return(aq)
}
