# Compile dataset for Germany

compileDataDE = function(){
  # Covid-19 -------------------------------------------------------------------
  cov_de = getCovidDE()
  cov_de_polygons = makeSFPolygonsDE(cov_de$cov_nuts3)
  
  
  # Air quality data -----------------------------------------------------------
  flist = list.files(file.path(envrmt$path_DE),
                     pattern = "^.*\\.csv$",full.names = TRUE, recursive = TRUE)
  flist =  flist[grepl(flist, pattern = "2020PM2_1SMW")]
  
  
  pm_uba = makedfUBA(flist)
  pm_uba_points =  makeSFPointsUBA(pm_uba)
  
  
  # Merge air quality and COVID data -------------------------------------------
  de_nuts3 = st_join(cov_de_polygons, pm_uba_points$pts)
  de_nuts3 = de_nuts3[!is.na(de_nuts3$stationname), ]
  
  de_nuts3 = lapply(unique(de_nuts3$stationname), function(l){
    m = merge(de_nuts3,  pm_uba[[as.character(l)]],
              by.x = c("stationname", "date"), by.y = c("stationname", "date"))
    cn = names(de_nuts3)
    # cn[cn=="lat"] = "lat.x"
    # cn[cn=="lon"] = "lon.x"
    # 
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
  
  return(list(de_nuts3_mean = de_nuts3_mean, pm_uba_points = pm_uba_points))
}


