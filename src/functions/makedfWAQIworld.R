

makedfWAQIworld = function(flist,country="IT",param="pm"){

  pm =  read.csv(flist,skip = 4,header = TRUE, sep = ",", dec = ".",nrows = 10000000)
  
  
  pm = pm[pm$Country == country & pm$Specie==param,]
  
     aq = lapply(unique(pm$City), function(x){
       #latlon = as.tibble(geo_osm(x))
       act = pm[pm$City == x ,]
       act$lat= as.numeric(geo_osm(as.character(x))[1])
       act$lon= as.numeric(geo_osm(as.character(x))[2])
       act$date = as.POSIXct(act$Date, origin = "CET")
       # for(i in seq(5, ncol(act))){
       #   act[, i] = as.numeric(act[, i])
       # }
       act$statname = act$City
       #act$statname = substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
       act = act[act$date >= as.POSIXct("2020-01-01"), ]
       
       # p = st_sfc(st_point(latlon),crs = 4326)
       # st_sf(aq = x,p)
       
       act$stationname = act$statname
       act$stationcode = NA
       act$TYPE_OF_AREA = NA
       act$TYPE_OF_STATION = NA
       act

       #act = act[, c("Date","Country","City","Specie","pmcount","pmmin","pmmax","pmmedian","pmvariance","lat","lon","date","statname","stationname","stationcode","TYPE_OF_AREA","TYPE_OF_STATION")]
     })

  names(aq) = sapply(aq, function(x) x[1, "stationname"])
  
  return(aq)
  
}


