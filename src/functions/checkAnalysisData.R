#' Check analysis data.
#'
#' Exclude datasets that do not cover the analysis time range.

checkAnalysisData <- function(data, start_date, end_date) {
  date_length <- round(difftime(end_date, start_date, units = c("days")))

  valid_data <- data

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

  return(valid_data)
}