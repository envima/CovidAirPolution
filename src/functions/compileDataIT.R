# Compile dataset for Italy

compileDataIT <- function(city = FALSE,
                          start_date = as.POSIXct("2020-02-25"),
                          end_date = as.POSIXct("2020-04-01"),
                          pm = "PM2.5", country = "italy") {
  if (pm == "PM2.5") {
    param <- "pm25"
  } else {
    param <- "pm10"
  }

  # Compile SARS-CoV-2 dataset.
  cov_it <- compileCovidIT()
  cov_it_polygons <- compileSFPolygonsIT(cov_it$cov_nuts3)
  
  # Compile population dataset.
  population_data <- compilePopulationIT()
  cov_it_polygons <- merge(cov_it_polygons, population_data, by.x = "denominazione_provincia", by.y = "nuts3Name")

  # Compile air quality dataset based on UBA.
  if (city) {
    flist <- list.files(file.path(envrmt$path_world),
      pattern = "^.*\\.csv$", full.names = TRUE, recursive = TRUE
    )
    flist <- flist[grepl(flist, pattern = "waqi-covid19-airqualitydata")]

    pm_waqi <- makedfWAQIworld(flist, country = "IT", param = param)
  } else {
    flist <- list.files(file.path(envrmt$path_data, "IT/"),
      pattern = "^.*\\.csv$", full.names = TRUE, recursive = TRUE
    )
    flist <- flist[grepl(flist, pattern = "report-data-platform")]
    pm_waqi <- compileWAQI(flist, pm = pm)
  }
  pm_waqi_points <- compileSFPointsWAQI(pm_waqi)

  dplcts <- which(duplicated(pm_waqi_points$pts$p))
  for (i in dplcts) {
    d <- which(pm_waqi_points$pts$p == pm_waqi_points$pts$p[i])
    pm_waqi_points$pts$stationname[d] <- pm_waqi_points$pts$stationname[d[1]]
  }

  # Merge SARS-CoV-2 and air quality data.
  cov_it_polygons <- st_transform(cov_it_polygons, crs = 25832)
  pm_waqi_points$pts <- st_transform(pm_waqi_points$pts, crs = 25832)
  it_nuts3 <- st_join(cov_it_polygons, pm_waqi_points$pts)
  it_nuts3 <- it_nuts3[!is.na(it_nuts3$stationname), ]

  it_nuts3 <- lapply(unique(it_nuts3$stationname), function(l) {
    print(l)
    m <- merge(it_nuts3, pm_waqi[[as.character(l)]],
      by.x = c("stationname", "date"), by.y = c("stationname", "date"), all.y = TRUE
    )

    cn <- names(it_nuts3)
    cn[cn == "lat"] <- "lat.x"
    cn[cn == "lon"] <- "lon.x"

    fill_pos <- which(!is.na(m$denominazione_provincia))[1]

    m$adm1_code <- m$adm1_code[fill_pos]
    m$stato <- m$stato[fill_pos]
    m$codice_regione <- m$codice_regione[fill_pos]
    m$denominazione_regione <- m$denominazione_regione[fill_pos]
    m$sigla_provincia <- m$sigla_provincia[fill_pos]
    m$denominazione_provincia <- m$denominazione_provincia[fill_pos]
    m$lat.x <- m$lat.x[fill_pos]
    m$lon.x <- m$lon.x[fill_pos]
    m$totale_casi[is.na(m$totale_casi)] <- 0
    m$new_cases[is.na(m$new_cases)] <- 0
    m$geometry <- m$geometry[fill_pos]

    if (city) {
      colnames(m)[which(colnames(m) == "median")] <- "pm"
    }
    m <- m[, c(
      cn,
      "pm",
      "lat.y", "lon.y"
    )]

    m <- m[which(!duplicated(as.data.frame(m))), ]
    return(m)
  })

  it_nuts3 <- do.call(rbind, it_nuts3)
  # it_nuts3 <- st_transform(it_nuts3, crs = 25832)
  it_nuts3$st_area <- st_area(it_nuts3)
  it_nuts3$pop_dens <- it_nuts3$pop_total / (it_nuts3$st_area / 1e+06)
  # it_nuts3 <- st_transform(it_nuts3, crs = 4326)


  # Compute mean/median of air quality meassurements within each NUTS region
  # if more than one station is available for the region.
  nuts3_names <- sort(unique(it_nuts3$denominazione_provincia))
  it_nuts3_mean <- lapply(nuts3_names, function(p) {
    act <- it_nuts3[it_nuts3$denominazione_provincia == p, ]

    pm_mean <- aggregate(list(act$pm),
      by = list(act$date), FUN = mean, na.rm = TRUE
    )
    names(pm_mean) <- c("date", "pm_mean")

    pm_median <- aggregate(list(act$pm),
                           by = list(act$date), FUN = median, na.rm = TRUE
    )
    names(pm_median) <- c("date", "pm_median")
    
    pm_mrgd <- merge(pm_mean, pm_median)
    
    u_stations <- unique(act$stationname)

    new_stationname <- paste(length(u_stations), u_stations, collapse = "-")

    act <- act[!is.na(act$stationname), ]

    act <- merge(
      pm_mrgd, act[seq(nrow(act[act$stationname == u_stations[1], ])), ],
      by.x = "date", by.y = "date"
    )

    act$stationname <- new_stationname

    act$weekday <- weekdays(act$date)
    act$date_day <- as.factor(paste(act$date, substr(act$weekday, 1, 1)))

    # colnames(act) <- c(
    #   "date", "pm_mean", "pm_median", "nuts3Code", "data", "stato",
    #   "codice_regione", "denominazione_regione",
    #   "codice_provincia", "nuts3Name", "sigla_provincia",
    #   "lat", "lon", "cases", "notes", "notes_en",
    #   "new_cases", "stationname", "pm", "lat.y", "lon.y",
    #   "geometry", "area", "weekday", "date_day"
    # )
    
    colnames(act)[which("adm1_code" == colnames(act))] <- "nuts3Code"
    colnames(act)[which("denominazione_provincia" == colnames(act))] <- "nuts3Name"
    colnames(act)[which("totale_casi" == colnames(act))] <- "cases"

    act <- act[!duplicated(act$date), ]

    act <- st_set_geometry(act, act$geometry)
    act <- st_transform(act, crs = 25832)

    return(act)
  })

  names(it_nuts3_mean) <- nuts3_names

  # Subset analysis regions to complete ones, check for outliers and estimate replacement values.
  it_nuts3_mean <- checkAnalysisData(it_nuts3_mean, start_date, end_date, pm, country = country)


  # Compile data for overview maps
  it_nuts3_map <- compileMapIT(it_nuts3_mean)


  # Compile data averaged over country.
  it_avg <- compileAvgIT(it_nuts3_mean)


  # Compile clusters based on DTW ----------------------------------------------
  it_clstr <- compileDTWClustersIT(it_nuts3_mean)

  return(list(
    it_nuts3 = it_nuts3_mean, pm_waqi_points = pm_waqi_points,
    it_nuts3_map = it_nuts3_map, it_avg = it_avg, it_clstr = it_clstr
  ))
}
