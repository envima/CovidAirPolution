#' Compile detrended time series using glm models or mark weekdays.

compileDetrendedTimeSeries = function(data, vars = NA, comp = c("detr", "weekday_c")){
  
  comp = comp[1]

  if(comp == "detr"){
    
    frml = as.formula(paste(paste(vars[1], "~"), paste(vars[-1], collapse="+")))
    set.seed(01042020)
    glmmod = glm(frml, family = quasipoisson, data = data)
    ret_val = list(fit_val = glmmod$fitted.values, res_val = residuals(glmmod))
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
  
