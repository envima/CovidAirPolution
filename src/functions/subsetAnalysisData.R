#' Subset analysis data to start and end date.
#' 

subsetAnalysisData = function(data, start_date, end_date){
  
  date_length = round(difftime(end_date, start_date, units = c("days")))
  
  valid_data = data
  
  for(n in seq(length(data))){
    
    if(any(data[[n]]$date == end_date) &
       any(data[[n]]$date == start_date)){
      
      if(which(data[[n]]$date == end_date) - 
         which(data[[n]]$date == start_date) == date_length & 
         all(!is.na(data[[n]][which(data[[n]]$date == start_date) : 
                              which(data[[n]]$date == end_date), "pm25_mean"])) &
         all(!is.na(data[[n]][which(data[[n]]$date == start_date) : 
                              which(data[[n]]$date == end_date), "new_cases"]))){
        valid_data[[n]] = data[[n]][data[[n]]$date >= start_date &
                                      data[[n]]$date <= end_date  , ]
      } else {
        names(valid_data)[n] = "incomplete"
      }
      
    } else {
      names(valid_data)[n] = "incomplete"
    }
  }
  
  valid_data = valid_data[(!names(valid_data) == "incomplete")]
  
  print(names(data)[!(names(data) %in% names(valid_data))])
  
  return(valid_data)
}
