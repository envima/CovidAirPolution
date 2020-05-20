#' Compile detrended time series using glm models or mark weekdays.

compileDetrendedTimeSeries = function(data, frml = NA, comp = c("detr", "weekday_c")){
  
  comp = comp[1]

  if(comp == "detr"){
    
    set.seed(01042020)
    glmmod = glm(as.formula(frml), family = quasipoisson, data = data)

    ret_val = list(pred_val = predict(glmmod, newdata = data), 
                   res_val = residuals(glmmod))
  }
  
  if(comp == "weekday_c"){
    
    ret_val = rep(NA, length(data))
    
    for (i in (seq(length(ret_val)))){
      ret_val[i] = ifelse((data[i] == "Montag"), "M", 
                            ifelse((data[i] == "Sonntag") | 
                                     (data[i] == "Samstag"),
                                   "SS","W"))
    }
    ret_val = as.factor(ret_val)
  }
  
  return(ret_val)
}  
  
