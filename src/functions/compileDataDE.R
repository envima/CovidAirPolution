# Compile dataset for Germany

compileDataDE = function(){
  # Covid-19 -------------------------------------------------------------------
  cov_de = getCovidDE()
  cov_de_polygons = makeSFPolygonsDE(cov_de$cov_nuts3)
  
  
  # Air quality data -----------------------------------------------------------
  flist = list.files(file.path(envrmt$path_DE),
                     pattern = "^.*\\.csv$",full.names = TRUE, recursive = TRUE)
  flist =  flist[grepl(flist, pattern = "DE2020PM2_1SMW_20200421")]
  
  
  flistWAQI = list.files(file.path(envrmt$path_data,"DE/WAQI/"), 
                     pattern = "^.*\\.csv$",full.names = TRUE,recursive = TRUE)
  pm_waqi = makedfWAQI(flistWAQI)
  pm_waqi_points =  makeSFPointsWAQI(pm_waqi)
  
  pm_uba = makedfUBA(flist)#
  pm_uba_points =  makeSFPointsUBA(pm_uba)
  pm_uba_waqi_points=list()
  pm_uba_waqi_points$pts=st_union(pm_waqi_points$pts, pm_uba_points$pts)
  pm_uba_waqi_points$pop=c(pm_waqi_points$pop, pm_uba_points$pop)
  
  # Merge air quality and COVID data -------------------------------------------
  de_nuts3 = st_join(cov_de_polygons, pm_uba_waqi_points$pts)
  de_nuts3 = de_nuts3[!is.na(de_nuts3$stationname), ]
  
  de_nuts3 = lapply(unique(de_nuts3$stationname), function(l){
    m = merge(de_nuts3,  pm_uba[[as.character(l)]],
              by.x = c("stationname", "date"), by.y = c("stationname", "date"),
              all.y = TRUE)
    cn = names(de_nuts3)
    
    m$nuts3Code = tail(m$nuts3Code,1)
    m$cases[is.na(m$cases)] = 0
    m$deaths[is.na(m$deaths)] = 0
    m$weekday = as.factor(weekdays(m$date))
    m$date_day = as.factor(paste(m$date, substr(m$weekday, 1, 1)))
    m$nuts3Name = tail(m$nuts3Name, 1)
    m$state = tail(m$state, 1)
    m$note = tail(m$note, 1)
    m$new_cases[is.na(m$new_cases)] = 0
    m$new_cases_smooth[is.na(m$new_cases_smooth)] = 0
    m$cases_smooth[is.na(m$cases_smooth)] = 0
    m$deaths_smooth[is.na(m$deaths_smooth)] = 0
    m$new_deaths[is.na(m$new_deaths)] = 0
    m$new_deaths_smooth[is.na(m$new_deaths_smooth)] = 0
    m$name_2 = tail(m$name_2, 1)
    m$geometry = tail(m$geometry,1)

    m = m[, c(cn,
              "pm25",
              "lat", "lon")]
  })
  
  de_nuts3 = do.call(rbind, de_nuts3)
  de_nuts3$nuts3Name = as.factor(unlist(de_nuts3$nuts3Name))
  de_nuts3$state = as.factor(unlist(de_nuts3$state))
  de_nuts3$note = as.factor(unlist(de_nuts3$note))
  
  
  # Compute mean air quality within each nuts 3 region -------------------------
  nuts3_names = sort(unique(de_nuts3$nuts3Name))
  
  nuts3_names = nuts3_names[-which(nuts3_names %in% c("SK Düsseldorf", "SK Essen", "SK Krefeld", "SK Wuppertal"))]
  
  de_nuts3_mean = lapply(nuts3_names, function(p){
    act = de_nuts3[de_nuts3$nuts3Name == p,]
    
    pm = aggregate(list(act$pm25),
                   by = list(act$date), FUN = mean, na.rm=TRUE)
    names(pm) = c("date", "pm25_mean")
    
    u_stations = unique(act$stationname)
    
    new_stationname = paste(length(u_stations), u_stations, collapse = "-")
    
    act = merge(
      pm, act[seq(nrow(act[act$stationname == u_stations[1],])), ],
      by.x = "date", by.y = "date")
    
    act$stationname = new_stationname
    
    act = st_set_geometry(act, act$geometry)
    act = st_transform(act, crs = 4326)
  })
  
  names(de_nuts3_mean) = nuts3_names
  
  
  
  return(list(de_nuts3_mean = de_nuts3_mean, pm_uba_points =  pm_uba_waqi_points))
}
