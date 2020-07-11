# Compile spatial points datasets.
#'
#' Spatial point datasets are compiled for each PM station location.

#' PM data from UBA.
compileSFPointsUBA <- function(pm_data) {
  p <- lapply(pm_data, function(s) {
    latlon <- c(s[1, "lon"], s[1, "lat"])
    p <- st_sfc(st_point(latlon), crs = 4326)
    st_sf(stationname = s[1, "stationname"], p)
  })
  pts <- do.call(rbind, p)

  pts <- st_transform(pts, crs = 25832)

  pop <- lapply(lapply(pm_data, function(s) s[1, c("stationname")]), htmlTable)
  names(pop) <- NULL

  return(list(pts = pts, pop = pop))
}


#' PM data from WAQI.
compileSFPointsWAQI <- function(pm_data) {
  p <- lapply(pm_data, function(x) {
    latlon <- c(x[1, "lon"], x[1, "lat"])
    class(latlon) <- "numeric"
    p <- st_sfc(st_point(latlon), crs = 4326)
    st_sf(stationname = x[1, "stationname"], p)
  })
  pts <- do.call(rbind, p)

  pts <- st_transform(pts, crs = 25832)

  pop <- lapply(lapply(pm_data, function(x) x[1, c("stationname")]), htmlTable)
  names(pop) <- NULL

  return(list(pts = pts, pop = pop))
}
