# Create geo objects from UBA data

makedfUBA = function(flist){
  
  geo = read.table(file.path(envrmt$path_DE, "station_All_convert.csv"), skip = 1, header = TRUE, sep = ";", dec = ",")
  geo = geo[, c(1, 2, 13, 14)]
  names(geo) = c("stationcode", "stationname", "lon", "lat")
  geo$stationcode = as.character(geo$stationcode)
  
  uba = read.table(flist, skip = 0, header = TRUE, sep = ";")
  uba$date = as.POSIXct(as.character(uba$Datum), format = "%Y%m%d", origin = "CET")
  
  # Aggregate hourly values and set -999 to NA
  ch01 = which(colnames(uba) == "Wert01")
  ch24 = which(colnames(uba) == "Wert24")
  uba[, ch01:ch24][uba[, ch01:ch24] < 0] = NA
  uba$pm25 = rowMeans(uba[, ch01:ch24], na.rm = TRUE)
  uba$Station = as.character(uba$Station)
  uba = uba[, c(1, 32, 33, 4:5 )]
  names(uba)[1:3] = c("stationcode", "date", "pm25")
  
  uba = merge(uba, geo, by = "stationcode")
  uba$lat = uba$lat
  uba$lon = uba$lon
  
  # Move each station to individual list and fill NAs
  # 15084  7340  5513  5334

  aq = lapply(unique(uba$stationcode), function(s) {
    act = uba[uba$stationcode == s, ]
    
    act = act[which(!duplicated(act$date)), ]
    
    lna = which(is.na(act$pm25))
    filled = na.approx(act$pm25, na.rm = FALSE)
    
    if(!is_empty(lna)){
      if(lna[1] == 1){
        first_valid = which(!is.na(filled))[1]
        filled[1:(first_valid - 1)] = filled[first_valid]
      }
      
      if(lna[length(lna)] == length(filled)){
        last_valid = tail(which(!is.na(filled)), 1)
        filled[(last_valid + 1):length(filled)] = filled[last_valid]
      }
    } 
    act$pm25 = filled
    act = act[act$date >= as.POSIXct("2020-01-01"), ]
    if(any(is.na(act$pm25))){
      print(act$stationcode)
    }
    
    return(act)
  })
  
  names(aq) = sapply(aq, function(s) s[1, "stationname"])
  
  return(aq)
  
}
