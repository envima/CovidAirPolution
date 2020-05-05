#' Compile SARS-CoV-2 and PM2.5 data for Germany.
#' 
#' Daily SARS-CoV-2 and PM2.5 data is compiled for Germany on NUTS3 level.
#' Time series are only included if they are complete from April 15th onwards.
#' 

compileDataDE = function(start_date = as.POSIXct("2020-02-15"), 
                         end_date = as.POSIXct("2020-04-01"),
                         pm = "PM2.5"){
  
  if(pm == "PM2.5"){
    pattern = "DE2020PM2_1SMW_20200421"
  } else {
    pattern = "DE2020PM1_1SMW_20200421"
  }
  
  # SARS-CoV-2 -----------------------------------------------------------------
  cov_de = getCovidDE()
  cov_de_polygons = makeSFPolygonsDE(cov_de$cov_nuts3)
  
  
  # Air quality data -----------------------------------------------------------
  # Data from UBA.
  flist = list.files(file.path(envrmt$path_DE),
                     pattern = "^.*\\.csv$",full.names = TRUE, recursive = TRUE)
  flist =  flist[grepl(flist, pattern = pattern)]
  pm_uba = makedfUBA(flist)
  pm_uba_points =  makeSFPointsUBA(pm_uba)
  
  
  # Data from WAQI for Baden-Württemberg.
  flistWAQI = list.files(file.path(envrmt$path_data,"DE/WAQI/"),
                         pattern = "^.*\\.csv$",full.names = TRUE,recursive = TRUE)
  pm_waqi = makedfWAQI(flistWAQI, pm = pm)
  pm_waqi_points =  makeSFPointsWAQI(pm_waqi)
  
  # Merge datasets from UBA and WAQI.
  pm_uba_waqi = c(pm_uba, pm_waqi)
  pm_uba_waqi_points=list()
  pm_uba_waqi_points$pts = rbind(pm_waqi_points$pts, pm_uba_points$pts)
  pm_uba_waqi_points$pop = c(pm_waqi_points$pop, pm_uba_points$pop)
  
  
  
  # Merge air quality and SARS-CoV-2 data --------------------------------------
  de_nuts3 = st_join(cov_de_polygons, pm_uba_waqi_points$pts)
  de_nuts3 = de_nuts3[!is.na(de_nuts3$stationname), ]
  
  de_nuts3 = lapply(unique(de_nuts3$stationname), function(l){
    m = merge(de_nuts3,  pm_uba_waqi[[as.character(l)]],
              by.x = c("stationname", "date"), by.y = c("stationname", "date"),
              all.y = TRUE)
    cn = names(de_nuts3)
    
    fill_pos = which(!is.na(m$nuts3Code))[1]
    
    m$nuts3Code = m$nuts3Code[fill_pos]
    m$cases[is.na(m$cases)] = 0
    m$deaths[is.na(m$deaths)] = 0
    m$weekday = as.factor(weekdays(m$date))
    m$date_day = as.factor(paste(m$date, substr(m$weekday, 1, 1)))
    m$nuts3Name = m$nuts3Name[fill_pos]
    m$state = m$state[fill_pos]
    m$note = m$note[fill_pos]
    m$new_cases[is.na(m$new_cases)] = 0
    m$new_cases_smooth[is.na(m$new_cases_smooth)] = 0
    m$cases_smooth[is.na(m$cases_smooth)] = 0
    m$deaths_smooth[is.na(m$deaths_smooth)] = 0
    m$new_deaths[is.na(m$new_deaths)] = 0
    m$new_deaths_smooth[is.na(m$new_deaths_smooth)] = 0
    m$name_2 = m$name_2[fill_pos]
    m$geometry = m$geometry[fill_pos]
    
    m = m[, c(cn,
              "pm",
              "lat", "lon")]
    return(m)
  })
  
  de_nuts3 = do.call(rbind, de_nuts3)
  de_nuts3$nuts3Name = as.factor(unlist(de_nuts3$nuts3Name))
  de_nuts3$state = as.factor(unlist(de_nuts3$state))
  de_nuts3$note = as.factor(unlist(de_nuts3$note))
  de_nuts3 = st_transform(de_nuts3, crs = 25832)
  de_nuts3$area = st_area(de_nuts3)
  de_nuts3 = st_transform(de_nuts3, crs = 4326)
  
  
  # Compute mean air quality within each nuts 3 region -------------------------
  nuts3_names = sort(unique(de_nuts3$nuts3Name))
  
  de_nuts3_mean = lapply(nuts3_names, function(p){
    act = de_nuts3[de_nuts3$nuts3Name == p,]
    pm = aggregate(list(act$pm),
                   by = list(act$date), FUN = mean, na.rm=TRUE)
    names(pm) = c("date", "pm_mean")
    
    u_stations = unique(act$stationname)
    
    new_stationname = paste(length(u_stations), u_stations, collapse = "-")
    
    act = merge(
      pm, act[seq(nrow(act[act$stationname == u_stations[1],])), ],
      by.x = "date", by.y = "date")
    
    act$stationname = new_stationname
    
    act = st_set_geometry(act, act$geometry)
    act = st_transform(act, crs = 4326)
    return(act)
  })
  
  names(de_nuts3_mean) = nuts3_names
  
  
  # Subset analysis regions to complete/valid ones -----------------------------
  de_nuts3_mean = subsetAnalysisData (de_nuts3_mean, start_date, end_date)
  
  # Exclude Böblingen because of obviously wrong PM2.5 observations.
  de_nuts3_mean = de_nuts3_mean[which(!names(de_nuts3_mean) == "LK Böblingen")]
  
  
  # Compile data for overview maps ---------------------------------------------
  de_nuts3_map = compileMapDE(de_nuts3_mean)
  
  
  # Compile data averaged over country -----------------------------------------
  de_avg = compileAvg(de_nuts3_mean)
  
  
  # Compile clusters based on DTW ----------------------------------------------
  de_clstr = compileDTW(de_nuts3_mean)
  
  return(list(de_nuts3 = de_nuts3_mean, pm_uba_points =  pm_uba_waqi_points,
              de_nuts3_map = de_nuts3_map, de_avg = de_avg, de_clstr = de_clstr))
}
