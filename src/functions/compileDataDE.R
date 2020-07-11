#' Compile SARS-CoV-2 and PM data for Germany.
#'
#' Daily SARS-CoV-2 and PM10/PM2.5 data is compiled for Germany on the level of
#' districts. Time series are only included if they cover the analysis time
#' completely.

compileDataDE <- function(start_date = as.POSIXct("2020-01-15"),
                          end_date = as.POSIXct("2020-04-20"),
                          pm = "PM2.5") {
  if (pm == "PM2.5") {
    pattern <- "DE2020PM2_1SMW_20200421"
  } else {
    pattern <- "DE2020PM1_1SMW_20200421"
  }

  # Compile SARS-CoV-2 dataset.
  cov_de <- compileCovidDE()
  cov_de_polygons <- compileSFPolygonsDE(data = cov_de$cov_nuts3)

  # Compile population dataset.
  population_data <- compilePopulation()
  cov_de_polygons <- merge(cov_de_polygons, population_data, by = "nuts3Code")

  # Compile air quality dataset based on UBA.
  flist <- list.files(file.path(envrmt$path_DE),
    pattern = "^.*\\.csv$", full.names = TRUE, recursive = TRUE
  )
  flist <- flist[grepl(flist, pattern = pattern)]
  pm_uba <- compileUBA(flist, start_date = start_date)
  pm_uba_points <- compileSFPointsUBA(pm_uba)

  # Compile air quality dataset based on WAQI for Baden-Wuerttemberg.
  flistWAQI <- list.files(file.path(envrmt$path_data, "DE/WAQI/"),
    pattern = "^.*\\.csv$", full.names = TRUE, recursive = TRUE
  )
  pm_waqi <- compileWAQI(flistWAQI, pm = pm, start_date = start_date)
  pm_waqi_points <- compileSFPointsWAQI(pm_waqi)

  # Merge datasets from UBA and WAQI.
  pm_uba_waqi <- c(pm_uba, pm_waqi)
  pm_uba_waqi_points <- list()
  pm_uba_waqi_points$pts <- rbind(pm_waqi_points$pts, pm_uba_points$pts)
  pm_uba_waqi_points$pop <- c(pm_waqi_points$pop, pm_uba_points$pop)

  # Merge SARS-CoV-2 and air quality data.
  de_nuts3 <- st_join(cov_de_polygons, pm_uba_waqi_points$pts)
  de_nuts3 <- de_nuts3[!is.na(de_nuts3$stationname), ]

  de_nuts3 <- lapply(unique(de_nuts3$stationname), function(l) {
    m <- merge(de_nuts3, pm_uba_waqi[[as.character(l)]],
      by.x = c("stationname", "date"), by.y = c("stationname", "date"),
      all.y = TRUE
    )
    cn <- names(de_nuts3)

    fill_pos <- which(!is.na(m$nuts3Code))[1]

    m$nuts3Code <- m$nuts3Code[fill_pos]
    m$cases[is.na(m$cases)] <- 0
    m$deaths[is.na(m$deaths)] <- 0
    m$weekday <- as.factor(weekdays(m$date))
    m$date_day <- as.factor(paste(m$date, substr(m$weekday, 1, 1)))
    # m$weekday_c <- compileDetrendedTimeSeries(data = m$weekday, comp = "weekday_c")
    m$nuts3Name <- m$nuts3Name[fill_pos]
    m$state <- m$state[fill_pos]
    m$note <- m$note[fill_pos]
    m$new_cases[is.na(m$new_cases)] <- 0
    m$new_deaths[is.na(m$new_deaths)] <- 0
    # m$cases_glm_time[is.na(m$cases_glm_time)] = 0
    # m$cases_glm_time_residuals[is.na(m$cases_glm_time_residuals)] = 0
    # m$new_cases_glm_time[is.na(m$new_cases_glm_time)] = 0
    # m$new_cases_glm_time_residuals[is.na(m$new_cases_glm_time_residuals)] = 0
    # m$deaths_glm_time[is.na(m$deaths_glm_time)] = 0
    # m$deaths_glm_time_residuals[is.na(m$deaths_glm_time_residuals)] = 0
    # m$new_deaths_glm_time[is.na(m$new_deaths_glm_time)] = 0
    # m$new_deaths_glm_time_residuals[is.na(m$new_deaths_glm_time_residuals)] = 0
    # m$cases_loess[is.na(m$cases_loess)] <- 0
    # m$new_cases_loess[is.na(m$new_cases_loess)] <- 0
    # m$deaths_loess[is.na(m$deaths_loess)] <- 0
    # m$new_deaths_loess[is.na(m$new_deaths_loess)] <- 0
    m$name_2 <- m$name_2[fill_pos]
    m$centroid_lon <- m$centroid_lon[fill_pos]
    m$centroid_lat <- m$centroid_lat[fill_pos]
    m$st_area <- m$st_area[fill_pos]
    m$geometry <- m$geometry[fill_pos]
    m$X <- m$X[fill_pos]
    m$pop_total <- m$pop_total[fill_pos]
    m$pop_male <- m$pop_male[fill_pos]
    m$pop_female <- m$pop_female[fill_pos]
    m$pop_dens <- m$pop_dens[fill_pos]

    m <- m[, c(
      cn,
      "pm",
      "lat", "lon"
    )]
    return(m)
  })

  de_nuts3 <- do.call(rbind, de_nuts3)
  de_nuts3$nuts3Name <- as.factor(unlist(de_nuts3$nuts3Name))
  de_nuts3$state <- as.factor(unlist(de_nuts3$state))
  de_nuts3$note <- as.factor(unlist(de_nuts3$note))

  # Compute mean/median of air quality meassurements within each NUTS region
  # if more than one station is available for the region.
  nuts3_names <- sort(unique(de_nuts3$nuts3Name))

  de_nuts3_mean <- lapply(nuts3_names, function(p) {
    act <- de_nuts3[de_nuts3$nuts3Name == p, ]

    pm_mean <- aggregate(list(act$pm),
      by = list(act$date), FUN = mean, na.rm = TRUE
    )
    names(pm_mean) <- c("date", "pm_mean")

    pm_median <- aggregate(list(act$pm),
      by = list(act$date), FUN = median, na.rm = TRUE
    )
    names(pm_median) <- c("date", "pm_median")

    pm <- merge(pm_mean, pm_median)

    u_stations <- unique(act$stationname)

    new_stationname <- paste(length(u_stations), paste(u_stations, collapse = "-"))

    act <- merge(
      pm, act[seq(nrow(act[act$stationname == u_stations[1], ])), ],
      by.x = "date", by.y = "date"
    )

    act$stationname <- new_stationname

    act <- st_set_geometry(act, act$geometry)
    act <- st_transform(act, crs = 25832)
    return(act)
  })

  names(de_nuts3_mean) <- nuts3_names

  # Subset analysis regions to complete/valid ones.
  de_nuts3_mean <- checkAnalysisData(de_nuts3_mean, start_date, end_date)

  # Compile data for overview maps.
  de_nuts3_map <- compileMapDE(de_nuts3_mean)

  # Compile data averaged over country.
  de_avg <- compileAvg(de_nuts3_mean)

  # Compile clusters based on DTW.
  de_clstr <- compileDTWClusters(de_nuts3_mean)

  return(list(
    de_nuts3 = de_nuts3_mean, pm_uba_points = pm_uba_waqi_points,
    de_nuts3_map = de_nuts3_map, de_avg = de_avg, de_clstr = de_clstr
  ))
}
