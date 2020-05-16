#' Compile lagged glm models.

compileLaggedGLM = function(data, target = "new_cases", nlags = 14,
                            min_cases = 1, individual = FALSE, ndays = c(-14, 32),
                            obsprd_start = NULL, obsprd_end = NULL){
  
  
  model_lag = lapply(unique(data$nuts3Code), function(n){
    
    if(individual){
      obsprd_start = data$date[which(data[data$nuts3Code == n, "cases"] >= min_cases)[1] + ndays[1]]
      obsprd_end = data$date[which(data[data$nuts3Code == n, "cases"] >= min_cases)[1] + ndays[2]]
    } else {
      obsprd_start = as.POSIXct("2020-02-15")
      obsprd_end =  as.POSIXct("2020-04-01")
      
    }
    
    model_lag = lapply(seq(0, nlags), function(l){
      
      tmp = data[data$nuts3Code == n, ]
      
      tmp = tmp %>%
        mutate(pm_mean_lag = dplyr::lag(pm_mean_dt, n = (l), default = NA))
      
      frml = as.formula(paste(target, "~ date + weekday_c + pm_mean_lag"))      
      tmp_glm = glm(frml , family = quasipoisson, data = tmp[tmp$date >= obsprd_start & tmp$date <= obsprd_end,])
      
      test = summary(tmp_glm)
      data.frame(nuts3_code = n,
                 cluster = tmp$cluster_covid[1],
                 cluster_pm = tmp$cluster_pm[1],
                 pm_mean_mean = mean(tmp$pm_mean),
                 lag = -l, 
                 t = test$coefficients["pm_mean_lag", "t value"], 
                 p = test$coefficients["pm_mean_lag", "Pr(>|t|)"],
                 obsprd_start = obsprd_start,
                 obsprd_end = obsprd_end)
    })
    model_lag = do.call("rbind", model_lag)
    
  })
  model_lag = do.call("rbind", model_lag)
  
  return(model_lag)
}



compileLaggedGLMboth = function(data){
  
  model_lag = lapply(unique(data$nuts3Code), function(n){
    
    model_lag = lapply(seq(0, 14), function(l){
      
      tmp = data[data$nuts3Code == n, ]
      
      tmp25 = tmp %>%
        mutate(pm25_mean_lag = dplyr::lag(pm25_mean, n = l, default = NA))
      
      tmp10 = tmp %>%
        mutate(pm10_mean_lag = dplyr::lag(pm10_mean, n = l, default = NA))
      
      tmp = merge(tmp25, tmp10)
      
      tmp_glm = glm(cases ~ date + weekday_c + pm10_mean_lag + pm25_mean_lag, family = quasipoisson, data = tmp)
      
      test = summary(tmp_glm)
      data.frame(nuts3_code = n,
                 cluster = tmp$cluster_covid[1],
                 cluster_pm = tmp$cluster_pm[1],
                 pm_mean_mean = mean(tmp$pm_mean),
                 lag = -l, 
                 t25 = test$coefficients["pm25_mean_lag", "t value"], 
                 p25 = test$coefficients["pm25_mean_lag", "Pr(>|t|)"],
                 t10 = test$coefficients["pm10_mean_lag", "t value"], 
                 p10 = test$coefficients["pm10_mean_lag", "Pr(>|t|)"])
    })
    model_lag = do.call("rbind", model_lag)
    
  })
  model_lag = do.call("rbind", model_lag)
  return(model_lag)
}
