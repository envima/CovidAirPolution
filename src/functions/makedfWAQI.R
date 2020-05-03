#' Compile PM2.5 information from WAQI data.
#' 
#' Data is provided by WAQI.
#' Geographic information is included based on station locations.
#' Start date of the time series is set to 2020-02-15.
#' 


makedfWAQI = function(flist){
  
  # Combine information with geographical infromation and move each station to 
  # individual list and fill NAs.
  aq = lapply(flist, function(f) {
    latlon = getWAQIpos(readLines(f, n=5))
    act = read.table(f, skip = 0, header = TRUE, sep = ",")
    latlon= as.numeric(c(latlon[2],latlon[1]))
    act$lat= latlon[2]
    act$lon= latlon[1]
    act$date = as.POSIXct(act$date, origin = "CET")
    for(i in seq(2, ncol(act))){
      act[, i] = as.numeric(act[, i])
    }
    act$statname = strsplit(basename(f),split = "\\.")[[1]][2]
    act = act[act$date >= as.POSIXct("2020-01-01"), ]
    
    act$stationname = act$statname
    act$stationcode = NA
    act$TYPE_OF_AREA = NA
    act$TYPE_OF_STATION = NA
    act = act[, c("stationcode", "date", "pm25", "TYPE_OF_AREA", 
                  "TYPE_OF_STATION", "stationname", "lat", "lon")]
    return(act)
  })
  
  names(aq) = sapply(aq, function(x) x[1, "stationname"])
  
  return(aq)
}

