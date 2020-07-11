#' Compile map data.
#'
#' Compile a single timestep subset of the analysis data for 2020-04-01.
#' PM time series is averaged from the
#' beginning of the time series to 2020-04-01.

# Compile map datasets for Germany ---------------------------------------------
compileMapDE <- function(nuts) {
  map <- lapply(nuts, function(n) {
    tmp <- cbind(n[n$date == as.POSIXct("2020-04-01"), ],
      nuts3Area = st_area(n[1, c("nuts3Name", "nuts3Code")])
    )
    tmp$pm_mean <- mean(n$pm_mean[n$date >= as.POSIXct("2020-02-15") &
      n$date <= as.POSIXct("2020-04-01")])
    tmp$pm_median <- mean(n$pm_median[n$date >= as.POSIXct("2020-02-15") &
      n$date <= as.POSIXct("2020-04-01")])
    return(tmp)
  })
  map <- do.call("rbind", map)
  return(map)
}
