#' Compile lagged glm models geographic data.
#' 

compileLaggedGLMGeo = function(model_lag, map){
  test_geo = merge(model_lag, map[, c("nuts3Code", "cases", "deaths", "nuts3Name", "state", "lat", "lon", "nuts3Area")], by.x = "nuts3_code", by.y = "nuts3Code")
  
  test_geo_lag = lapply(unique(test_geo$lag), function(l){
    tmp = test_geo[test_geo$lag == l,]
    tmp_lm = lm(tmp$t ~ tmp$lat + tmp$lon + log10(tmp$nuts3Area))
    s = summary(tmp_lm)
    data.frame(tmp[1,],
               rsq = s$r.squared,
               lat_t = s$coefficients["tmp$lat", "t value"],
               lat_p = s$coefficients["tmp$lat",  "Pr(>|t|)"],
               lon_t = s$coefficients["tmp$lon", "t value"],
               lon_p = s$coefficients["tmp$lon",  "Pr(>|t|)"],
               area_t = s$coefficients["log10(tmp$nuts3Area)", "t value"],
               area_p = s$coefficients["log10(tmp$nuts3Area)",  "Pr(>|t|)"])
  })
  test_geo_lag = do.call("rbind", test_geo_lag)
  return(list(test_geo = test_geo, test_geo_lag = test_geo_lag))
}

