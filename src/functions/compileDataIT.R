# Compile dataset for Italy

compileDataIT = function(city=TRUE, 
                         start_date = as.POSIXct("2020-02-15"), 
                         end_date = as.POSIXct("2020-04-01")){
  # Covid-19 -------------------------------------------------------------------
  cov_it = getCovidIT()
  cov_it_polygons = makeSFPolygonsIT(cov_it$cov_nuts3)
  
  
  # Air quality data -----------------------------------------------------------
  flist = list.files(file.path(envrmt$path_data,"IT/"), 
                     pattern = "^.*\\.csv$",full.names = TRUE,recursive = TRUE)
  flist =  flist[grepl(flist,pattern = "report-data-platform")]   
  if (city){
    flist = list.files(file.path(envrmt$path_world),
                       pattern = "^.*\\.csv$",full.names = TRUE, recursive = TRUE)
    flist =  flist[grepl(flist, pattern = "waqi-covid19-airqualitydata")]  
    pm_waqi = makedfWAQIworld(flist,country="IT",param="pm25")
  } else {
    pm_waqi = makedfWAQI(flist)
  }  
  
  pm_waqi_points =  makeSFPointsWAQI(pm_waqi)
  
  
  # Merge air quality and COVID data -------------------------------------------
  it_nuts3 = st_join(cov_it_polygons, pm_waqi_points$pts)
  it_nuts3 = it_nuts3[!is.na(it_nuts3$stationname), ]
  
  it_nuts3 = lapply(unique(it_nuts3$stationname), function(l){
    print(l)
    m = merge(it_nuts3,  pm_waqi[[as.character(l)]], 
              by.x = c("stationname", "date"), by.y = c("stationname", "date"), all.y = TRUE)
    
    cn = names(it_nuts3)
    cn[cn=="lat"] = "lat.x"
    cn[cn=="lon"] = "lon.x"
    
    fill_pos = which(!is.na(m$denominazione_provincia))[1]
    
    m$adm1_code = m$adm1_code[fill_pos]
    m$stato = m$stato[fill_pos]
    m$codice_regione = m$codice_regione[fill_pos]
    m$denominazione_regione = m$denominazione_regione[fill_pos]
    m$sigla_provincia = m$sigla_provincia[fill_pos]
    m$denominazione_provincia = m$denominazione_provincia[fill_pos]
    m$lat.x = m$lat.x[fill_pos]
    m$lon.x = m$lon.x[fill_pos]
    m$totale_casi[is.na(m$totale_casi)] = 0
    m$new_cases[is.na(m$new_cases)] = 0
    m$geometry = m$geometry[fill_pos]
    
    m = m[, c(cn, 
              "median",
              "lat.y", "lon.y")]
    colnames(m)[which(colnames(m) == "median")] = "pm25"
    return(m)
  })
  
  it_nuts3 = do.call(rbind, it_nuts3)
  
  it_nuts3 = st_transform(it_nuts3, crs = 25832)
  it_nuts3$area = st_area(it_nuts3)
  it_nuts3 = st_transform(it_nuts3, crs = 4326)
  
  
  # Compute mean air quality within each nuts 3 region -------------------------
  nuts3_names = sort(unique(it_nuts3$denominazione_provincia))
  it_nuts3_mean = lapply(nuts3_names, function(p){
    act = it_nuts3[it_nuts3$denominazione_provincia == p,]
    
    pm = aggregate(list(act$pm25), 
                   by = list(act$date), FUN = mean, na.rm=TRUE)
    names(pm) = c("date", "pm25_mean")
    
    u_stations = unique(act$stationname)
    
    new_stationname = paste(length(u_stations), u_stations, collapse = "-")
    
    act = merge(
      pm, act[seq(nrow(act[act$stationname == u_stations[1],])), ],
      by.x = "date", by.y = "date")
    
    act$weekday = weekdays(act$date)
    act$date_day = as.factor(paste(act$date, substr(act$weekday, 1, 1)))

    colnames(act) = c("date", "pm25_mean", "nuts3Code", "data", "stato", 
                      "codice_regione", "denominazione_regione", 
                      "codice_provincia", "nuts3Name", "sigla_provincia", 
                      "lat", "lon", "cases", "notes", "notes_en", 
                      "new_cases", "stationname", "pm25", "lat.y", "lon.y", 
                      "geometry", "area", "weekday", "date_day")
    
    act = st_set_geometry(act, act$geometry)
    act = st_transform(act, crs = 4326)
    
    return(act)
  })
  
  names(it_nuts3_mean) = nuts3_names
  
  # Compile data for overview maps ---------------------------------------------
  it_nuts3_map = compileMapDE(it_nuts3_mean)
  
  
  # Compile data averaged over country -----------------------------------------
  it_avg = compileAvgIT(it_nuts3_mean)
  
  
  # Compile clusters based on DTW ----------------------------------------------
  it_clstr = compileDTWIT(it_nuts3_mean)
  
  return(list(it_nuts3 = it_nuts3_mean, pm_waqi_points = pm_waqi_points,
              it_nuts3_map = it_nuts3_map, it_avg = it_avg, it_clstr = it_clstr))
}


