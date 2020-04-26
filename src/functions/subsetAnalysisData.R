# Subset analysis data to start and end date.

subsetAnalysisData = function(data, start_date, end_date){
  
  date_length = round(difftime(end_date, start_date, units = c("days")))
  
  valid = lapply(seq(length(data)), function(n){
    valid = NULL
    if(any(data[[n]]$date == end_date) &
       any(data[[n]]$date == start_date)){
      if(which(data[[n]]$date == end_date) - 
         which(data[[n]]$date == start_date) == date_length & 
         all(!is.na(data[[n]][which(data[[n]]$date == start_date) : 
                        which(data[[n]]$date == end_date), "pm25_mean"])) &
         all(!is.na(data[[n]][which(data[[n]]$date == start_date) : 
                              which(data[[n]]$date == end_date), "new_cases"])))
        valid = names(data[n])
      }
    return(valid)
  })
  valid = unlist(valid)
  print(names(data)[!(names(data) %in% valid)])
  
  return(data[valid])
  }
