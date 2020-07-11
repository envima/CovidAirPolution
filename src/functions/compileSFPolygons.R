#' Compile spatial polygons datasets.
#'
#' Spatial polygon datasets are compiled for each NUTS-3 region covered by the
#' dataset. Identity is managed based on unique NUTS codes.

#' Compile spatial polygons for data from German.
compileSFPolygonsDE <- function(data) {
  rtyp3 <- st_read(
    file.path(
      envrmt$path_data,
      "DE/landkreise-in-germany/landkreise-in-germany.shp"
    )
  )
  rtyp3$cca_2 <- as.numeric(as.character(rtyp3$cca_2))

  rtyp3 <- merge(rtyp3, data, by.x = "cca_2", by.y = "nuts3Code")
  names(rtyp3)[1] <- "nuts3Code"
  rtyp3 <- rtyp3[, c(names(data), "name_2")]

  rtyp3 <- st_transform(rtyp3, crs = 25832)

  centroids_utm <- st_centroid(rtyp3)
  centroids_latlon <- st_transform(centroids_utm, crs = 4326)
  rtyp3$centroid_lat <- st_coordinates(centroids_latlon)[, "Y"]
  rtyp3$centroid_lon <- st_coordinates(centroids_latlon)[, "X"]
  rtyp3$st_area <- st_area(rtyp3)

  return(rtyp3)
}


#' Compile spatial polygons for data from Italy.
compileSFPolygonsIT <- function(nuts) {
  p <- lapply(unique(nuts$codice_provincia), function(p) {
    act <- nuts[nuts$codice_provincia == p, ]
    latlon <- c(act[1, "lon"], act[1, "lat"])
    p <- st_sfc(st_point(latlon), crs = 4326)
    st_sf(act, p)
  })

  plgns <- do.call(rbind, p)
  it <- ne_states(country = "italy", returnclass = "sf")
  it <- it[, "adm1_code"]

  centroids <- st_coordinates(st_centroid(it))
  it$centroid_lon <- centroids[, "X"]
  it$centroid_lat <- centroids[, "Y"]

  return(st_join(it, plgns))
}
