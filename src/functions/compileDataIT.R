# Compile dataset for Italy

compileDataIT = function(){
  # Covid-19 -------------------------------------------------------------------
  cov_it = getCovidIT()
  cov_it_polygons = makeSFPolygonsIT(cov_it$cov_nuts3)
  
  
  # Air quality data -----------------------------------------------------------
  flist = list.files(file.path(envrmt$path_data,"IT/"), 
                     pattern = "^.*\\.csv$",full.names = TRUE,recursive = TRUE)
  flist =  flist[grepl(flist,pattern = "report-data-platform")]   
  pm_waqi = makedfWAQI(flist)
  pm_waqi_points =  makeSFPointsWAQI(pm_waqi)
  
  
  # Merge air quality and COVID data -------------------------------------------
  cov_it_polygons = makeSFPolygonsIT(cov_it$cov_nuts3)
  it_nuts3 = st_join(cov_it_polygons, pm_waqi_points$pts)
  it_nuts3 = it_nuts3[!is.na(it_nuts3$aq_location), ]
  
  it_nuts3 = lapply(unique(it_nuts3$aq_location), function(l){
    m = merge(it_nuts3,  pm_waqi[[l]], 
              by.x = c("aq_location", "date"), by.y = c("statname", "date"))
    cn = names(it_nuts3)
    cn[cn=="lat"] = "lat.x"
    cn[cn=="lon"] = "lon.x"
    
    m = m[, c(cn, 
              "pm25", "pm25_min", "pm25_max",
              "lat.y", "lon.y")]
  })
  
  it_nuts3 = do.call(rbind, it_nuts3)
  
  # Compute mean air quality within each nuts 3 region -------------------------
  nuts3_names = sort(unique(it_nuts3$denominazione_provincia))
  it_nuts3_mean = lapply(nuts3_names, function(p){
    act = it_nuts3[it_nuts3$denominazione_provincia == p,]
    
    pm = aggregate(list(act$pm25, act$pm25_min, act$pm25_max), 
                   by = list(act$date), FUN = mean, na.rm=TRUE)
    names(pm) = c("date", "pm25_mean", "pm25_min_mean", "pm25_max_mean")
    
    act = merge(
      pm, act[seq(nrow(act[act$aq_location == unique(act$aq_location)[1],])), ], 
      by.x = "date", by.y = "date")
    
    act = st_set_geometry(act, act$geometry)
    act = st_transform(act, crs = 4326)
  })
  
  names(it_nuts3_mean) = nuts3_names
  
  return(list(it_nuts3_mean = it_nuts3_mean, pm_waqi_points = pm_waqi_points))
}


