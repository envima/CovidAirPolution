#' Compile DTW clusters.

compileDTWClusters <- function(data) {
  new_cases <- lapply(data, function(n) {
    as.data.frame(n[, "new_cases"])[, -2]
  })

  pm_median <- lapply(data, function(n) {
    as.data.frame(n[, "pm_median"])[, -2]
  })

  # Compute DTW clusters.
  new_cases_dtw_cluster <- tsclust(new_cases,
    type = "partitional", k = 6,
    distance = "dtw_basic", centroid = "pam",
    seed = 01042020, trace = TRUE,
    args = tsclust_args(dist = list(window.size = 10))
  )

  pm_dtw_cluster <- tsclust(pm_median,
    type = "partitional", k = 6,
    distance = "dtw_basic", centroid = "pam",
    seed = 01042020, trace = TRUE,
    args = tsclust_args(dist = list(window.size = 10))
  )

  # Create DTW cluster maps.
  clstr_map <- lapply(seq(length(data)), function(c) {
    tmp <- data[[c]][1, ]
    tmp$cluster_covid <-
      as.factor(paste0(
        "c ", new_cases_dtw_cluster@cluster[c], " n=(",
        new_cases_dtw_cluster@clusinfo[new_cases_dtw_cluster@cluster[c], 1],
        ")"
      ))
    tmp$cluster_pm <-
      as.factor(paste0(
        "c ", pm_dtw_cluster@cluster[c], " n=(",
        pm_dtw_cluster@clusinfo[pm_dtw_cluster@cluster[c], 1],
        ")"
      ))
    return(tmp)
  })
  clstr_map <- do.call("rbind", clstr_map)

  # Add cluster information to dataset.
  clstr <- lapply(seq(length(data)), function(c) {
    tmp <- data[[c]]
    tmp$cluster_covid <-
      as.factor(paste0(
        "c ", new_cases_dtw_cluster@cluster[c], " n=(",
        new_cases_dtw_cluster@clusinfo[new_cases_dtw_cluster@cluster[c], 1],
        ")"
      ))
    tmp$cluster_pm <-
      as.factor(paste0(
        "c ", pm_dtw_cluster@cluster[c], " n=(",
        pm_dtw_cluster@clusinfo[pm_dtw_cluster@cluster[c], 1],
        ")"
      ))
    return(tmp)
  })
  clstr <- do.call("rbind", clstr)

  clstr$cluster_covid <- factor(clstr$cluster_covid, levels = sort(unique(as.character(clstr$cluster_covid))))
  clstr$cluster_pm <- factor(clstr$cluster_pm, levels = sort(unique(as.character(clstr$cluster_pm))))

  # Compile dataset averaged over each cluster
  clstr_avg <- st_drop_geometry(clstr[, c(
    "date", "pm_mean", "pm_median", "cases",
    "deaths", "new_cases", "new_deaths",
    "pop_total", "pop_male", "pop_female", "pop_dens",
    "cluster_covid", "cluster_pm"
  )])

  clstr_avg_pm <- aggregate(. ~ date + cluster_pm,
    data = clstr_avg[, -which((names(clstr_avg) == "cluster_covid"))], FUN = mean
  )
  clstr_avg_pm$date_day <- paste(clstr_avg_pm$date, substr(weekdays(clstr_avg_pm$date), 1, 1))
  clstr_avg_pm$weekday <- weekdays(clstr_avg_pm$date)
  # clstr_avg_pm$weekday_c <- compileDetrendedTimeSeries(data = clstr_avg_pm$weekday, comp = "weekday_c")

  clstr_avg_covid <- aggregate(. ~ date + cluster_covid,
    data = clstr_avg[, -which((names(clstr_avg) == "cluster_pm"))], FUN = mean
  )
  clstr_avg_covid$date_day <- paste(clstr_avg_covid$date, substr(weekdays(clstr_avg_covid$date), 1, 1))
  clstr_avg_covid$weekday <- weekdays(clstr_avg_covid$date)
  # clstr_avg_covid$weekday_c <- compileDetrendedTimeSeries(data = clstr_avg_covid$weekday, comp = "weekday_c")

  # Compile detrended dataset with cluster information.
  # clstr_avg_pm_glm_time = lapply(seq(6), function(i){
  #
  #   tmp_pm = clstr_avg_pm[clstr_avg_pm$cluster_pm ==
  #                           unique(clstr_avg_pm$cluster_pm)[i] ,]
  #
  #   tmp = compileDetrendedTimeSeries(data = tmp_pm,
  #                                    frml = "new_cases ~ date + weekday",
  #                                    comp = "detr")
  #   tmp_pm$new_cases_glm_time = tmp$pred_val
  #   tmp_pm$new_cases_glm_time_residuals = tmp$res_val
  #
  #   return(tmp_pm)
  # })
  # clstr_avg_pm_glm_time = do.call("rbind", clstr_avg_pm_glm_time)
  #
  #
  # clstr_avg_covid_glm_time = lapply(seq(6), function(i){
  #
  #   tmp_covid = clstr_avg_covid[clstr_avg_covid$cluster_covid ==
  #                                 unique(clstr_avg_covid$cluster_covid)[i] ,]
  #   tmp = compileDetrendedTimeSeries(data = tmp_covid,
  #                                    frml = "new_cases ~ date + weekday",
  #                                    comp = "detr")
  #   tmp_covid$new_cases_glm_time = tmp$pred_val
  #   tmp_covid$new_cases_glm_time_residuals = tmp$res_val
  #
  #   return(tmp_covid)
  # })
  # clstr_avg_covid_glm_time = do.call("rbind", clstr_avg_covid_glm_time)

  return(list(
    clstr = clstr,
    clstr_avg_pm = clstr_avg_pm,
    clstr_avg_covid = clstr_avg_covid,
    # clstr_avg_pm_glm_time = clstr_avg_pm_glm_time,
    # clstr_avg_covid_glm_time = clstr_avg_covid_glm_time,
    clstr_map = clstr_map
  ))
}
