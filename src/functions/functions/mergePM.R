#' Merge PM2.5 and PM10 data
#' 

mergePM = function(pm25, pm10){
  
  tmp25 = pm25$de_clstr$clstr
  tmp10 = pm10$de_clstr$clstr
  
  colnames(tmp25)[which(colnames(tmp25) == "pm_mean")] = "pm25_mean"
  colnames(tmp10)[which(colnames(tmp10) == "pm_mean")] = "pm10_mean"
  
  tmp = merge(tmp25, tmp10[, c("date", "nuts3Code", "pm10_mean")], by = c("date", "nuts3Code"))
  
  return(tmp)
  
  
  # common_names = names(pm25$de_nuts3)[which(names(pm25$de_nuts3) %in% names(pm10$de_nuts3))]
  # 
  # nuts_3 = lapply(common_names, function(n){
  # 
  #   tmp25 = pm25$de_nuts3[[n]]
  #   tmp10 = pm10$de_nuts3[[n]]
  #   
  #   colnames(tmp25)[which(colnames(tmp25) == "pm_mean")] = "pm25_mean"
  #   colnames(tmp10)[which(colnames(tmp10) == "pm_mean")] = "pm10_mean"
  #   
  #   tmp = merge(as.data.frame(tmp25), st_set_geometry(tmp10[, c("date", "nuts3Code", "pm10_mean")], NULL), by = c("date", "nuts3Code"))
  #   
  #   return(tmp)
  # })
  # 
  # names(nuts_3) = common_names
  # return(nuts_3)
}