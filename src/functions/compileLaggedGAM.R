#' Compile lagged gam models.

compileLaggedGAM = function(data, target = "new_cases", nlags = 14,
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
    
    if(obsprd_end > obsprd_start){
      
      model_lag = lapply(seq(0, nlags), function(l){
        
        tmp = data[data$nuts3Code == n, ]
        
        tmp = tmp %>%
          mutate(pm_mean_lag = dplyr::lag(pm_mean, n = (l), default = NA))
        
        frml = as.formula(paste(target, "~ s(seq(length(date)), bs='tp', sp=0.6) + weekday_cn + pop_dens + pm_mean_lag"))
        set.seed(01042020)
        tmp_gam = gam(frml, family = quasipoisson(link = "log"), data = tmp[tmp$date >= obsprd_start & tmp$date <= obsprd_end,])
        
        test = summary(tmp_gam)
        data.frame(nuts3_code = n,
                   cluster = tmp$cluster_covid[1],
                   cluster_pm = tmp$cluster_pm[1],
                   pm_mean_mean = mean(tmp$pm_mean),
                   lag = -l, 
                   t = test$p.t["pm_mean_lag"],
                   p = test$p.pv["pm_mean_lag"],
                   obsprd_start = obsprd_start,
                   obsprd_end = obsprd_end)
      })
      model_lag = do.call("rbind", model_lag)
    } else {
      model_lag = NULL
    }
    
  })
  model_lag = do.call("rbind", model_lag)
  
  
  return(model_lag)
}