#' Compile averaged data over regions.

# Compile average over Germany -------------------------------------------------
compileAvg = function(data){
  avg = lapply(data, 
                  "[", c("date", "date_day", "pm25_mean", "new_cases", 
                         "new_cases_smooth", "cases", "cases_smooth", 
                         "deaths_smooth", "new_deaths", "new_deaths_smooth"))
  avg = do.call("rbind", avg)

  avg = st_set_geometry(avg, NULL)
  
  avg = aggregate(. ~ date, data = avg, FUN = mean)
  avg$date_day = paste(avg$date, substr(weekdays(avg$date), 1, 1))
  
  set.seed(01042020)
  new_cases_smooth_detr = glm(new_cases_smooth ~ date,  family = quasipoisson, 
                              data =avg)
  new_cases_smooth_detr = residuals(new_cases_smooth_detr)
  avg$new_cases_smooth_detr = new_cases_smooth_detr
  
  return(avg)
} 



compileAvgIT = function(data){
  avg = lapply(data, 
               "[", c("date", "date_day", "pm25_mean", "new_cases", 
                      "cases"))
  avg = do.call("rbind", avg)
  
  avg = st_set_geometry(avg, NULL)
  
  avg = aggregate(. ~ date, data = avg, FUN = mean)
  avg$date_day = paste(avg$date, substr(weekdays(avg$date), 1, 1))
  
  set.seed(01042020)
  new_cases_detr = glm(new_cases ~ date,  family = quasipoisson, 
                              data =avg)
  new_cases_detr = residuals(new_cases_detr)
  avg$new_cases_detr = new_cases_detr
  
  return(avg)
} 
