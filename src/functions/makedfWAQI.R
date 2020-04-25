# Create geo objects from WAQI data

makedfWAQI = function(flist){
  
  aq = lapply(flist, function(f) {
    latlon = getWAQIpos(readLines(f, n=5))
    act = read.table(f, skip = 0, header = TRUE, sep = ",")
    latlon= as.numeric(c(latlon[2],latlon[1]))
    act$lat= latlon[2]
    act$lon= latlon[1]
    act$date = as.POSIXct(act$date)
    for(i in seq(2, ncol(act))){
      act[, i] = as.numeric(act[, i])
    }
    act$statname = strsplit(basename(f),split = "\\.")[[1]][2]
    #act$statname = substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
    act = act[act$date >= as.POSIXct("2020-01-01"), ]
    
    # p = st_sfc(st_point(latlon),crs = 4326)
    # st_sf(aq = x,p)
    
    return(act)
  })
  
  names(aq) = sapply(aq, function(x) x[1, "statname"])
  
  return(aq)
  
}


