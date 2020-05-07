#' Compile SF polygons for datasets.
#' 
#' Spatial polygon datasets are compiled for each region covered by the 
#' respective dataset.
#' 

# Data for Germany -------------------------------------------------------------
makeSFPolygonsDE = function(covdata){
  
  rtyp3 = st_read(
    file.path(envrmt$path_data, 
              "DE/landkreise-in-germany/landkreise-in-germany.shp"))
  rtyp3$cca_2 = as.numeric(as.character(rtyp3$cca_2))
  
  rtyp3 = merge(rtyp3, covdata, by.x = "cca_2", by.y = "nuts3Code")
  names(rtyp3)[1] = "nuts3Code"
  rtyp3 = rtyp3[, c(names(covdata), "name_2")]
  
  centroids = st_coordinates(st_centroid(rtyp3))
  rtyp3$centroid_lon = centroids[, "X"]
  rtyp3$centroid_lat = centroids[, "Y"]
  
  return(rtyp3)
}


# Data for Italy ---------------------------------------------------------------
makeSFPolygonsIT = function(nuts){
  
  p = lapply(unique(nuts$codice_provincia), function(p){
    act = nuts[nuts$codice_provincia == p, ] 
    latlon = c(act[1, "lon"], act[1, "lat"])
    p = st_sfc(st_point(latlon), crs = 4326)
    st_sf(act ,p)
  })
  
  plgns = do.call(rbind, p)
  it  = ne_states(country = "italy", returnclass = "sf")
  it = it[, "adm1_code"]
  
  centroids = st_coordinates(st_centroid(it))
  it$centroid_lon = centroids[, "X"]
  it$centroid_lat = centroids[, "Y"]
  
  return(st_join(it, plgns))
}
