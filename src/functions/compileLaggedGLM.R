#' Compile lagged glm models.

compileLaggedGLM = function(data){
  model_lag = lapply(unique(data$nuts3Code), function(n){
    
    model_lag = lapply(seq(0, 14), function(l){
      
      tmp = data[data$nuts3Code == n, ]
      
      tmp = tmp %>%
        # group_by(nuts3Code) %>%
        mutate(pm25_mean_lag = dplyr::lag(pm25_mean, n = l, default = NA))
      
      # tmp = tmp[!is.na(tmp$cases_lag), ]
      # print(tmp$pm25_mean_lag)
      
      tmp_glm = glm(cases ~ date + weekday_c + pm25_mean_lag, family = quasipoisson, data = tmp)
      
      test = summary(tmp_glm)
      data.frame(nuts3_code = n,
                 cluster = tmp$cluster_covid[1],
                 cluster_pm25 = tmp$cluster_pm25[1],
                 pm25_mean_mean = mean(tmp$pm25_mean),
                 lag = -l, 
                 t = test$coefficients["pm25_mean_lag", "t value"], 
                 p = test$coefficients["pm25_mean_lag", "Pr(>|t|)"])
    })
    model_lag = do.call("rbind", model_lag)
    
  })
  model_lag = do.call("rbind", model_lag)
  return(model_lag)
}
