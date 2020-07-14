#' Check analysis data.
#'
#' Exclude datasets that do not cover the analysis time range and compute an
#' outlier detection and replacement value estimate.

checkAnalysisData <- function(data, start_date, end_date, pm) {
  date_length <- round(difftime(end_date, start_date, units = c("days")))

  valid_data <- data

  # Complete coverage.
  for (n in seq(length(data))) {
    if (any(data[[n]]$date == end_date) &
      any(data[[n]]$date == start_date)) {
      if (which(data[[n]]$date == end_date) -
        which(data[[n]]$date == start_date) == date_length &
        all(!is.na(data[[n]][which(data[[n]]$date == start_date):
        which(data[[n]]$date == end_date), "pm_mean"])) &
        all(!is.na(data[[n]][which(data[[n]]$date == start_date):
        which(data[[n]]$date == end_date), "new_cases"]))) {
        valid_data[[n]] <- data[[n]][data[[n]]$date >= start_date &
          data[[n]]$date <= end_date, ]
        ndf <- data.frame(
          pm_mean = data[[n]]$pm_mean,
          pm_median = data[[n]]$pm_median,
          nuts3Code = data[[n]]$nuts3Code,
          n = rep(n, nrow(data[[n]]))
        )
        # pm = rbind(pm, ndf)
      } else {
        names(valid_data)[n] <- "incomplete"
      }
    } else {
      names(valid_data)[n] <- "incomplete"
    }
  }

  valid_data <- valid_data[which((!names(valid_data) == "incomplete"))]

  print(names(data)[!(names(data) %in% names(valid_data))])

  saveRDS(names(data)[!(names(data) %in% names(valid_data))], file.path(envrmt$path_analysis, paste0("non_valid_", pm, ".rds")))

  # Outlier detection and estimate.
  valid_data_oc <- lapply(valid_data, function(d) {
    inds <- seq(as.Date(d$date[1]), as.Date(d$date[nrow(d)]), by = "day")

    d_ts_pm_mean <- ts(d$pm_mean, start = c(2020, as.numeric(format(inds[1], "%j"))), frequency = 365)
    d_ts_pm_mean_outliers <- tsoutliers(d_ts_pm_mean)

    d_ts_pm_median <- ts(d$pm_median, start = c(2020, as.numeric(format(inds[1], "%j"))), frequency = 365)
    d_ts_pm_median_outliers <- tsoutliers(d_ts_pm_median)

    d$pm_mean_estm <- d$pm_mean
    d$pm_mean_rplced <- FALSE

    d$pm_median_estm <- d$pm_median
    d$pm_median_rplced <- FALSE

    if (length(d_ts_pm_mean_outliers$index) > 0) {
      d$pm_mean_estm[d_ts_pm_mean_outliers$index] <- d_ts_pm_mean_outliers$replacements
      d$pm_mean_rplced[d_ts_pm_mean_outliers$index] <- TRUE
    }

    if (length(d_ts_pm_median_outliers$index) > 0) {
      d$pm_median_estm[d_ts_pm_median_outliers$index] <- d_ts_pm_median_outliers$replacements
      d$pm_median_rplced[d_ts_pm_median_outliers$index] <- TRUE
    }
    return(d)
  })
  return(valid_data_oc)
}
