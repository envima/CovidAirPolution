# make points
makeSFPoints <- function(flist)
  {p = lapply(flist, function(f) {
  latlon = getWAQIpos(readLines(f, n=5))
  latlon= c(latlon[2],latlon[1])
  class(latlon) <- "numeric"
  p = sf::st_sfc(sf::st_point(latlon),crs = 4326)
  sf::st_sf(info=getWAQI_1(readLines(f, n=5)),p)
})

pts <- do.call(rbind, p)
return(pts)
}

getWAQINames= function(flist){
  
 n= lapply(flist, function(f) {
    stat_name<-substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
    
  })
  
  stat_name <- do.call(rbind, n)
  return(stat_name)
}