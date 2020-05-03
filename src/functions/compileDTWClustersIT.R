#' Compile DTW clustering analysis.

compileDTWIT = function(data){
  
  new_cases = lapply(data, function(n){
    as.data.frame(n[, "new_cases"])[, -2]
  })
  
  pm25_mean = lapply(data, function(n){
    as.data.frame(n[, "pm25_mean"])[, -2]
  })
  
  # Compute DTW clusters.
  new_cases_dtw_cluster = tsclust(new_cases, type = "partitional", k = 2,
                                  distance = "dtw_basic", centroid = "pam", seed=01042020, 
                                  trace = TRUE,
                                  args = tsclust_args(dist = list(window.size = 10)))
  
  pm25_dtw_cluster = tsclust(pm25_mean, type = "partitional", k = 2,
                             distance = "dtw_basic", centroid = "pam", seed=01042020, 
                             trace = TRUE,
                             args = tsclust_args(dist = list(window.size = 10))) 
  
  # Create DTW cluster maps.
  clstr_map = lapply(seq(length(data)), function(c){
    tmp = data[[c]][1, ]
    tmp$cluster_covid = as.factor(paste0("c ", new_cases_dtw_cluster@cluster[c], " n=(", 
                                         new_cases_dtw_cluster@clusinfo[new_cases_dtw_cluster@cluster[c], 1],
                                         ")"))
    tmp$cluster_pm25 = as.factor(paste0("c ", pm25_dtw_cluster@cluster[c], " n=(", 
                                        pm25_dtw_cluster@clusinfo[pm25_dtw_cluster@cluster[c], 1],
                                        ")"))
    return(tmp)
  })
  clstr_map = do.call("rbind", clstr_map)
  
  # Add cluster information to dataset.
  clstr = lapply(seq(length(data)), function(c){
    
    tmp = data[[c]][, c("date", "pm25_mean", "nuts3Code", "new_cases", 
                            "cases")]
    tmp$cluster_covid = as.factor(paste0("c ", new_cases_dtw_cluster@cluster[c], " n=(", 
                                         new_cases_dtw_cluster@clusinfo[new_cases_dtw_cluster@cluster[c], 1],
                                         ")"))
    tmp$cluster_pm25 = as.factor(paste0("c ", pm25_dtw_cluster@cluster[c], " n=(", 
                                        pm25_dtw_cluster@clusinfo[pm25_dtw_cluster@cluster[c], 1],
                                        ")"))
    return(tmp)
  })
  clstr = do.call("rbind", clstr)
  
  clstr = as.data.frame(clstr)[, -ncol(clstr)]
  # clstr = clstr[clstr$date >= as.POSIXct("2020-02-15", tz = "CET"), ]
  clstr$cluster_covid = factor(clstr$cluster_covid, levels = sort(unique(as.character(clstr$cluster_covid))))
  clstr$cluster_pm25 = factor(clstr$cluster_pm25, levels = sort(unique(as.character(clstr$cluster_pm25))))
  clstr$weekday = weekdays(clstr$date)
  clstr$weekday_c = NA
  for (i in (1:length(clstr$weekday))){
    clstr$weekday_c[i] = ifelse((clstr$weekday[i] == "Montag"), "M", 
                                ifelse((clstr$weekday[i] == "Sonntag") | 
                                         (clstr$weekday[i] == "Samstag"),
                                       "SS","W"))
  }
  clstr$weekday_c = as.factor(clstr$weekday_c)
  clstr$new_cases[clstr$new_cases < 0] = 0
  
  # Compile dataset averaged over each cluster
  clstr_avg = clstr[, -which(names(clstr) %in% c("nuts3Code", "weekday", "weekday_c"))]
  clstr_avg = aggregate(. ~ date + cluster_pm25, data = clstr_avg, FUN = mean)
  clstr_avg$date_day = paste(clstr_avg$date, substr(weekdays(clstr_avg$date), 1, 1))
  
  # Compile detrended dataset with cluster information.
  clstr_avg_detr = lapply(unique(clstr_avg$cluster_pm25), function(c){
    tmp = clstr_avg[clstr_avg$cluster_pm25 == c,]
    detr = glm(new_cases ~ date,  family = poisson, data =tmp)
    clstr_avg[clstr_avg$cluster_pm25 == c, "new_cases_detr"] = residuals(detr)
    return(clstr_avg[clstr_avg$cluster_pm25 == c,])
  })
  clstr_avg_detr = do.call("rbind", clstr_avg_detr)
  
  # clstr_avg_detr = clstr_avg_detr[
  #   clstr_avg_detr$date >= as.POSIXct("2020-02-15 CET") & 
  #     clstr_avg_detr$date <= as.POSIXct("2020-04-01 CEST"), ]
  
  return(list(clstr = clstr, clstr_avg = clstr_avg, 
              clstr_avg_detr = clstr_avg_detr, clstr_map = clstr_map))
}