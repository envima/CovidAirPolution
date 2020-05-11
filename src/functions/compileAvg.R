#' Compile averaged data over regions.

# Compile average over Germany -------------------------------------------------
compileAvg = function(data){
  avg = lapply(data, 
                  "[", c("date", "date_day", "pm_mean", "new_cases", 
                         "new_cases_smooth", "cases", "cases_smooth", 
                         "deaths_smooth", "new_deaths", "new_deaths_smooth"))
  avg = do.call("rbind", avg)

  avg = st_set_geometry(avg, NULL)
  
  avg = aggregate(. ~ date, data = avg, FUN = mean)
  avg$weekday = weekdays(avg$date)
  avg$date_day = paste(avg$date, substr(avg$weekday, 1, 1))
  avg$weekday_c = compileDetrendedTimeSeries(data = avg$weekday, comp = "weekday_c")
  
  set.seed(01042020)
  avg$cases_detr = compileDetrendedTimeSeries(data = avg,
                                              vars = c("cases", "date", "weekday_c"),
                                              comp = "detr")
  
  set.seed(01042020)
  avg$new_cases_detr = compileDetrendedTimeSeries(data = avg,
                                                  vars = c("new_cases", "date", "weekday_c"),
                                                  comp = "detr")
  
  set.seed(01042020)
  avg$new_cases_smooth_detr = compileDetrendedTimeSeries(data = avg,
                                                  vars = c("new_cases_smooth", "date", "weekday_c"),
                                                  comp = "detr")
  
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
                                              vars = c("cases", "date", "weekday_c"),
                                              comp = "detr")
  
  set.seed(01042020)
  avg$new_cases_detr = compileDetrendedTimeSeries(data = avg,
                                                  vars = c("new_cases", "date", "weekday_c"),
                                                  comp = "detr")
  
  return(avg)
} 
