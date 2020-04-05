# Compile SF point vector dataset

makeSFPolygonsItaly = function(nuts){
  p = lapply(unique(nuts$codice_provincia), function(p){
    act = nuts[nuts$codice_provincia == p, ] 
    latlon = c(act[1, "long"], act[1, "lat"])
    p = st_sfc(st_point(latlon),crs = 4326)
    st_sf(act ,p)
  })
  pts = do.call(rbind, p)
  it  = ne_states(country = "italy", returnclass = "sf")
  it = it[, "adm1_code"]
  return(st_join(it, pts))
}
