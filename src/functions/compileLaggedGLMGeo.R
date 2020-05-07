#' Compile lagged glm models geographic data.
#' 

compileLaggedGLMGeo = function(model_lag, map){
  test_geo = merge(model_lag, map[, c("nuts3Code", "cases", "deaths", "nuts3Name", "state", "centroid_lat", "centroid_lon", "nuts3Area")], by.x = "nuts3_code", by.y = "nuts3Code")
  
  test_geo_lag = lapply(unique(test_geo$lag), function(l){
    tmp = test_geo[test_geo$lag == l,]
    tmp_lm = lm(tmp$t ~ tmp$centroid_lat + tmp$centroid_lon + log10(tmp$nuts3Area))
    s = summary(tmp_lm)
    data.frame(tmp[1,],
               rsq = s$r.squared,
               centroid_lat_t = s$coefficients["tmp$centroid_lat", "t value"],
               centroid_lat_p = s$coefficients["tmp$centroid_lat",  "Pr(>|t|)"],
               centroid_lon_t = s$coefficients["tmp$centroid_lon", "t value"],
               centroid_lon_p = s$coefficients["tmp$centroid_lon",  "Pr(>|t|)"],
               area_t = s$coefficients["log10(tmp$nuts3Area)", "t value"],
               area_p = s$coefficients["log10(tmp$nuts3Area)",  "Pr(>|t|)"])
  })
  test_geo_lag = do.call("rbind", test_geo_lag)
  return(list(test_geo = test_geo, test_geo_lag = test_geo_lag))
}



compileLaggedGLMGeoIT = function(model_lag, map){
  test_geo = merge(model_lag, map[, c("nuts3Code", "cases", "nuts3Name", "denominazione_regione", "centroid_lat", "centroid_lon", "nuts3Area")], by.x = "nuts3_code", by.y = "nuts3Code")
  
  test_geo_lag = lapply(unique(test_geo$lag), function(l){
    tmp = test_geo[test_geo$lag == l,]
    tmp_lm = lm(tmp$t ~ tmp$centroid_lat + tmp$centroid_lon + log10(tmp$nuts3Area))
    s = summary(tmp_lm)
    data.frame(tmp[1,],
               rsq = s$r.squared,
               centroid_lat_t = s$coefficients["tmp$centroid_lat", "t value"],
               centroid_lat_p = s$coefficients["tmp$centroid_lat",  "Pr(>|t|)"],
               centroid_lon_t = s$coefficients["tmp$centroid_lon", "t value"],
               centroid_lon_p = s$coefficients["tmp$centroid_lon",  "Pr(>|t|)"],
               area_t = s$coefficients["log10(tmp$nuts3Area)", "t value"],
               area_p = s$coefficients["log10(tmp$nuts3Area)",  "Pr(>|t|)"])
  })
  test_geo_lag = do.call("rbind", test_geo_lag)
  return(list(test_geo = test_geo, test_geo_lag = test_geo_lag))
}

