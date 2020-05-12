#' Compile DTW clustering analysis.

compileDTW = function(data){
  
  new_cases = lapply(data, function(n){
    as.data.frame(n[, "new_cases_detr"])[, -2]
  })
  
  pm_mean = lapply(data, function(n){
    as.data.frame(n[, "pm_mean"])[, -2]
  })
  
  # Compute DTW clusters.
  new_cases_dtw_cluster = tsclust(new_cases, type = "partitional", k = 4,
                                  distance = "dtw_basic", centroid = "pam", seed=01042020, 
                                  trace = TRUE,
                                  args = tsclust_args(dist = list(window.size = 10)))
  
  pm_dtw_cluster = tsclust(pm_mean, type = "partitional", k = 4,
                             distance = "dtw_basic", centroid = "pam", seed=01042020, 
                             trace = TRUE,
                             args = tsclust_args(dist = list(window.size = 10))) 
  
  # Create DTW cluster maps.
  clstr_map = lapply(seq(length(data)), function(c){
    tmp = data[[c]][1, ]
    tmp$cluster_covid = as.factor(paste0("c ", new_cases_dtw_cluster@cluster[c], " n=(", 
                                         new_cases_dtw_cluster@clusinfo[new_cases_dtw_cluster@cluster[c], 1],
                                         ")"))
    tmp$cluster_pm = as.factor(paste0("c ", pm_dtw_cluster@cluster[c], " n=(", 
                                        pm_dtw_cluster@clusinfo[pm_dtw_cluster@cluster[c], 1],
                                        ")"))
    return(tmp)
  })
  clstr_map = do.call("rbind", clstr_map)
  
  # Add cluster information to dataset.
  clstr = lapply(seq(length(data)), function(c){
    
    tmp = data[[c]][, c("date", "pm_mean", "nuts3Code", "new_cases", 
                            "new_cases_smooth", "cases", "cases_smooth", 
                            "deaths_smooth", "new_deaths", "new_deaths_smooth")]
    tmp$cluster_covid = as.factor(paste0("c ", new_cases_dtw_cluster@cluster[c], " n=(", 
                                         new_cases_dtw_cluster@clusinfo[new_cases_dtw_cluster@cluster[c], 1],
                                         ")"))
    tmp$cluster_pm = as.factor(paste0("c ", pm_dtw_cluster@cluster[c], " n=(", 
                                        pm_dtw_cluster@clusinfo[pm_dtw_cluster@cluster[c], 1],
                                        ")"))
    return(tmp)
  })
  clstr = do.call("rbind", clstr)
  
  clstr = as.data.frame(clstr)[, -ncol(clstr)]
  # clstr = clstr[clstr$date >= as.POSIXct("2020-02-15", tz = "CET"), ]
  clstr$cluster_covid = factor(clstr$cluster_covid, levels = sort(unique(as.character(clstr$cluster_covid))))
  clstr$cluster_pm = factor(clstr$cluster_pm, levels = sort(unique(as.character(clstr$cluster_pm))))
  clstr$weekday = weekdays(clstr$date)
  clstr$weekday_c = compileDetrendedTimeSeries(data = clstr$weekday, comp = "weekday_c")
  
  # Compile dataset averaged over each cluster
  clstr_avg = clstr[, -which(names(clstr) %in% c("nuts3Code", "weekday", "weekday_c"))]
  clstr_avg = aggregate(. ~ date + cluster_pm, data = clstr_avg, FUN = mean)
  clstr_avg$date_day = paste(clstr_avg$date, substr(weekdays(clstr_avg$date), 1, 1))
  
  # Compile detrended dataset with cluster information.
  clstr_avg_detr = lapply(unique(clstr_avg$cluster_pm), function(c){
    tmp = clstr_avg[clstr_avg$cluster_pm == c,]
    tmp$weekday = weekdays(tmp$date)
    tmp$weekday_c = compileDetrendedTimeSeries(data = tmp$weekday, comp = "weekday_c")
    detr = compileDetrendedTimeSeries(data = tmp,
                                      vars = c("new_cases", "date", "weekday_c"),
                                      comp = "detr")
    clstr_avg[clstr_avg$cluster_pm == c, "new_cases_detr"] = detr
    detr = compileDetrendedTimeSeries(data = tmp,
                                      vars = c("new_cases_smooth", "date", "weekday_c"),
                                      comp = "detr")
    clstr_avg[clstr_avg$cluster_pm == c, "new_cases_smooth_detr"] = detr
    return(clstr_avg[clstr_avg$cluster_pm == c,])
  })
  clstr_avg_detr = do.call("rbind", clstr_avg_detr)
  
  # clstr_avg_detr = clstr_avg_detr[
  #   clstr_avg_detr$date >= as.POSIXct("2020-02-15 CET") & 
  #     clstr_avg_detr$date <= as.POSIXct("2020-04-01 CEST"), ]
  
  return(list(clstr = clstr, clstr_avg = clstr_avg, 
              clstr_avg_detr = clstr_avg_detr, clstr_map = clstr_map))
}