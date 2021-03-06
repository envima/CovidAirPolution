#' Compile lagged GAM models.
#'
#' Compile GAM models with time lags for PM data.

compileLaggedGAM <- function(data, frml, lag_var = "pm_median", nlags = 14,
                             subset_var = "cases", subset_thv = 1,
                             individual = "start", ndays = c(-14, 32),
                             obsprd_start = NULL, obsprd_end = NULL) {
  data <- st_drop_geometry(data)
  model_lag <- lapply(unique(data$nuts3Code[data[, subset_var] >= subset_thv]), function(n) {
    tmp <- data[data$nuts3Code == n, ]

    if (individual == "start") {
      obsprd_start <- tmp$date[which(tmp$cases >= 1)[1] + ndays[1]]
      obsprd_end <- tmp$date[which(tmp$cases >= 1)[1] + ndays[2]]
    } else if (individual == "max") {
      obsprd_start <- tmp$date[tail(which(tmp$new_cases == max(tmp$new_cases)), n = 1) + ndays[1]]
      obsprd_end <- tmp$date[tail(which(tmp$new_cases == max(tmp$new_cases)), n = 1) + ndays[2]]
    } else {
      obsprd_start <- as.POSIXct("2020-02-15")
      obsprd_end <- as.POSIXct("2020-04-01")
    }
    if (is.na(obsprd_end)) obsprd_end <- 0

    if (obsprd_end > obsprd_start) {
      model_lag <- lapply(seq(0, nlags), function(l) {
        tmp <- data[data$nuts3Code == n, ]

        tmp <- tmp %>%
          mutate(pm_lag = dplyr::lag(get(lag_var), n = (l), default = NA))

        frml <- as.formula(frml)
        set.seed(01042020)
        tmp_gam <- gam(frml,
          family = quasipoisson(link = "log"),
          data = tmp[tmp$date >= obsprd_start & tmp$date <= obsprd_end, ]
        )

        test <- summary(tmp_gam)
        data.frame(
          nuts3Code = n,
          lag_var = lag_var,
          cluster = tmp$cluster_covid[1],
          cluster_pm = tmp$cluster_pm[1],
          pm_median_mean = mean(tmp$pm_median),
          lag = -l,
          t = test$p.t["pm_lag"],
          p = test$p.pv["pm_lag"],
          obsprd_start = obsprd_start,
          obsprd_end = obsprd_end
        )
      })
      model_lag <- do.call("rbind", model_lag)
    } else {
      model_lag <- NULL
    }
  })
  model_lag <- do.call("rbind", model_lag)

  return(model_lag)
}
