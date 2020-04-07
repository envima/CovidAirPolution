# Compile SF point vector dataset

makeSFPointsWAQI = function(pm_waqi){
  p = lapply(pm_waqi, function(x){
    latlon = c(x[1, "lon"],x[1, "lat"])
    class(latlon) = "numeric"
    p = st_sfc(st_point(latlon), crs = 4326)
    st_sf(aq_location = x[1, "statname"],p)
  })
  pts = do.call(rbind, p)
  
  pop = lapply(lapply(pm_waqi, function(x) x[ ,c("date","pm25","pm25_min","pm25_max")]), htmlTable)
  names(pop) = NULL
  
  return(list(pts = pts, pop = pop))
}

# Depricated
# getWAQINames = function(flist){
#   n = lapply(flist, function(f) {
#     stat_name = substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
#   })
#   stat_name = do.call(rbind, n)
#   return(stat_name)
# }
