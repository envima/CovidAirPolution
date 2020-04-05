# Create geo objects from WAQI data

makedfWAQI = function(flist = NULL){
  
  if(is.null(flist)){
    flist = list.files(
      file.path(envrmt$`path_report-data-platform-16229-259611-lombardy`), 
      pattern = "^.*\\.csv$", full.names = TRUE)
  }
  
  aq = lapply(flist, function(f) {
    latlon = getWAQIpos(readLines(f, n=5))
    act = read.table(f, skip = 0, header = TRUE, sep = ",")
    latlon= as.numeric(c(latlon[2],latlon[1]))
    act$lat= latlon[2]
    act$lon= latlon[1]
    act$date = as.POSIXct(act$date)
    act$pm25 = as.numeric(as.character(act$pm25))
    act$statname = substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
    act = act[act$date >= as.POSIXct("2020-01-01"), ]
    
    # p = st_sfc(st_point(latlon),crs = 4326)
    # st_sf(aq = x,p)
    
    return(act)
  })
  
  names(aq) = sapply(aq, function(x) x[1, "statname"])
  
  return(aq)
  
}


