# Compile SF point vector dataset

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


makeSFPolygonsDE = function(nuts){
  
  rtyp3 = st_read(file.path(envrmt$path_data, "DE/landkreise-in-germany.geojson"))
  rtyp3$cca_2 = as.numeric(as.character(rtyp3$cca_2))
  rtyp3 = merge(rtyp3, nuts, by.x = "cca_2", by.y = "nuts3",all.X =TRUE)
  # 
  # p = lapply(unique(nuts$nuts3), function(p){
  #   act = nuts[nuts$nuts3 == p, ]
  #   act = merge(rtyp3, act, by.x = "cca_2", by.y = "nuts3")
  # })
  # plgns = do.call(rbind, p)
  # it  = ne_states(country = "italy", returnclass = "sf")
  # it = it[, "adm1_code"]
  return(rtyp3)
}
