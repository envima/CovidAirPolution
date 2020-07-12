#' Compile map data.
#'
#' Compile a single timestep subset of the analysis data for 2020-04-01.
#' PM time series is averaged from the
#' beginning of the time series to 2020-04-01.

# Compile map datasets for Germany ---------------------------------------------
compileMapDE <- function(data) {
  map <- lapply(data, function(n) {
    tmp <- cbind(n[n$date == as.POSIXct("2020-04-01"), ],
      nuts3Area = st_area(n[1, c("nuts3Name", "nuts3Code")])
    )
    tmp$pm_mean <- mean(n$pm_mean[n$date >= as.POSIXct("2020-02-15") &
      n$date <= as.POSIXct("2020-04-01")])
    tmp$pm_median <- mean(n$pm_median[n$date >= as.POSIXct("2020-02-15") &
      n$date <= as.POSIXct("2020-04-01")])
    tmp$pm_mean_estm <- mean(n$pm_mean_estm[n$date >= as.POSIXct("2020-02-15") &
                                    n$date <= as.POSIXct("2020-04-01")])
    tmp$pm_median_estm <- mean(n$pm_median_estm[n$date >= as.POSIXct("2020-02-15") &
                                        n$date <= as.POSIXct("2020-04-01")])
    tmp$pm_mean_rplced <- sum((n$pm_mean_rplced))
    tmp$pm_median_rplced <- sum((n$pm_median_rplced))
    return(tmp)
  })
  map <- do.call("rbind", map)
  return(map)
}
