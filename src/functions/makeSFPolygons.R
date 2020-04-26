# Compile SF point vector dataset

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
  return(st_join(it, plgns))
}


# Data for Germany -------------------------------------------------------------
makeSFPolygonsDE = function(covdata){
  
  rtyp3 = st_read(file.path(envrmt$path_data, "DE/landkreise-in-germany/landkreise-in-germany.shp"))
  rtyp3$cca_2 = as.numeric(as.character(rtyp3$cca_2))
  
  rtyp3 = merge(rtyp3, covdata, by.x = "cca_2", by.y = "nuts3Code")
  names(rtyp3)[1] = "nuts3Code"
  rtyp3 = rtyp3[, c(names(covdata), "name_2")]
  
  return(rtyp3)
}


