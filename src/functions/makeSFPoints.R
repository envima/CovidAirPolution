# Compile SF points for datasets.
#' 
#' Spatial point datasets are compiled for each PM2.5 station location.
#' 

# PM data from UBA -------------------------------------------------------------
makeSFPointsUBA = function(pm_uba){
  p = lapply(pm_uba, function(s){
    latlon = c(s[1, "lon"], s[1, "lat"])
    p = st_sfc(st_point(latlon), crs = 4326)
    st_sf(stationname = s[1, "stationname"],p)
  })
  pts = do.call(rbind, p)
  
  pts = st_transform(pts, crs = 25832)
  
  pop = lapply(lapply(pm_uba, function(s) s[1 ,c("stationname")]), htmlTable)
  names(pop) = NULL
  
  return(list(pts = pts, pop = pop))
}


# PM data from WAQI ------------------------------------------------------------
makeSFPointsWAQI = function(pm_waqi){
  p = lapply(pm_waqi, function(x){
    latlon = c(x[1, "lon"],x[1, "lat"])
    class(latlon) = "numeric"
    p = st_sfc(st_point(latlon), crs = 4326)
    st_sf(stationname = x[1, "stationname"],p)
  })
  pts = do.call(rbind, p)

  pts = st_transform(pts, crs = 25832)
  
  pop = lapply(lapply(pm_waqi, function(x) x[1 ,c("stationname")]), htmlTable)
  names(pop) = NULL
  
  return(list(pts = pts, pop = pop))
}
