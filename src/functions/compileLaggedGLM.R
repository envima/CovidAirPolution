#' Compile lagged glm models.

compileLaggedGLM = function(data){
  model_lag = lapply(unique(data$nuts3Code), function(n){
    
    model_lag = lapply(seq(0, 14), function(l){
      
      tmp = data[data$nuts3Code == n, ]
      
      tmp = tmp %>%
        # group_by(nuts3Code) %>%
        mutate(pm_mean_lag = dplyr::lag(pm_mean, n = l, default = NA))
      
      # tmp = tmp[!is.na(tmp$cases_lag), ]
      # print(tmp$pm_mean_lag)
      
      tmp_glm = glm(cases ~ date + weekday_c + pm_mean_lag, family = quasipoisson, data = tmp)
      
      test = summary(tmp_glm)
      data.frame(nuts3_code = n,
                 cluster = tmp$cluster_covid[1],
                 cluster_pm = tmp$cluster_pm[1],
                 pm_mean_mean = mean(tmp$pm_mean),
                 lag = -l, 
                 t = test$coefficients["pm_mean_lag", "t value"], 
                 p = test$coefficients["pm_mean_lag", "Pr(>|t|)"])
    })
    model_lag = do.call("rbind", model_lag)
    
  })
  model_lag = do.call("rbind", model_lag)
  return(model_lag)
}
