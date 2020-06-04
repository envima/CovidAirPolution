#' Compile averaged data over regions.

# Compile average over Germany -------------------------------------------------
compileAvg = function(data){
  avg = lapply(data, 
               "[", c("date", "pm_mean", "pm_median", "cases", "deaths", 
                      "new_cases", "new_deaths", 
                      "cases_glm_time", "cases_glm_time_residuals",
                      "new_cases_glm_time", "new_cases_glm_time_residuals",
                      "deaths_glm_time", "deaths_glm_time_residuals",
                      "new_deaths_glm_time", "new_deaths_glm_time_residuals",
                      "cases_loess", "new_cases_loess", 
                      "deaths_loess", "new_deaths_loess"))
  
  avg = do.call("rbind", avg)
  
  avg = st_set_geometry(avg, NULL)
  
  avg = aggregate(. ~ date, data = avg, FUN = mean)
  avg$weekday = weekdays(avg$date)
  avg$date_day = paste(avg$date, substr(avg$weekday, 1, 1))
  avg$weekday_c = compileDetrendedTimeSeries(data = avg$weekday, 
                                             comp = "weekday_c")
  
  set.seed(01042020)
  tmp = compileDetrendedTimeSeries(data = avg,
                                   frml = "cases ~ date + weekday",
                                   comp = "detr")
  avg$avg_cases_glm_time = tmp$pred_val
  avg$avg_cases_glm_time_residuals = tmp$res_val
  
  set.seed(01042020)
  tmp = compileDetrendedTimeSeries(data = avg,
                                   frml = "new_cases ~ date + weekday",
                                   comp = "detr")
  avg$avg_new_cases_glm_time = tmp$pred_val
  avg$avg_new_cases_glm_time_residuals = tmp$res_val
  
  set.seed(01042020)
  tmp = compileDetrendedTimeSeries(data = avg,
                                   frml = "deaths ~ date + weekday",
                                   comp = "detr")
  avg$avg_deaths_glm_time = tmp$pred_val
  avg$avg_deaths_glm_time_residuals = tmp$res_val
  
  set.seed(01042020)
  tmp = compileDetrendedTimeSeries(data = avg,
                                   frml = "new_deaths ~ date + weekday",
                                   comp = "detr")
  avg$avg_new_deaths_glm_time = tmp$pred_val
  avg$avg_new_deaths_glm_time_residuals = tmp$res_val
  
  return(avg)
} 

compileAvgIT = function(data){
  avg = lapply(data, 
               "[", c("date", "date_day", "pm_mean", "new_cases", 
                      "cases"))
  avg = do.call("rbind", avg)
  
  avg = st_set_geometry(avg, NULL)
  
  avg = aggregate(. ~ date, data = avg, FUN = mean)
  avg$weekday = weekdays(avg$date)
  avg$date_day = paste(avg$date, substr(avg$weekday, 1, 1))
  avg$weekday_c = compileDetrendedTimeSeries(data = avg$weekday, comp = "weekday_c")
  
  
  set.seed(01042020)
  avg$cases_detr = compileDetrendedTimeSeries(data = avg,
                                              frml = "cases ~ date + weekday_c",
                                              comp = "detr")
  
  set.seed(01042020)
  avg$new_cases_detr = compileDetrendedTimeSeries(data = avg,
                                                  frml = "new_cases ~ date + weekday_c",
                                                  comp = "detr")
  
  return(avg)
} 
