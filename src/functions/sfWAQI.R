# Create geo objects from WAQI data

sfWAQI = function(flist){
  
  sflist = lapply(flist, function(f) {
    
    latlon = parseHeaderWAQI(readLines(f, n=5))
    
    act = read.table(f, skip = 0, header = TRUE, sep = ",")
    act$date = as.Date(act$date)
    act$pm25 = as.numeric(as.character(act$pm25))
    act = act[act$date >= as.Date("2020-02-01"), ]
    
    act$statname<-substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
    
    act = st_sf(act, st_sfc(st_point(c(latlon$Longitude, latlon$Latitude))), crs = 4326)
    
    return(act)
  })
  
  return(st_as_sf(rbindlist(sflist, fill = TRUE)))
  
}
  
