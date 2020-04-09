# Create geo objects from UBA data

makedfUBA = function(flist){
  
<<<<<<< HEAD
 #geo = read.table(file.path(envrmt$path_DE, "station_All.csv"), skip = 1, header = TRUE, sep =  ";", dec = ",")
  geo = read_delim("data/DE/station_All.csv", 
                            ";", escape_double = FALSE, col_types = cols(`Länge dezimal` = col_double()), 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE, skip = 1)
  geo = geo[, c("Stationscode", "Stationsname", "Länge dezimal", "Breite dezimal")]
  geo$Stationscode = as.character(geo$Stationscode)
=======
  geo = read.table(file.path(envrmt$path_DE, "station_All_convert.csv"), skip = 1, header = TRUE, sep = ";", dec = ",")
  geo = geo[, c(1, 2, 13, 14)]
  names(geo) = c("stationcode", "stationname", "lon", "lat")
  geo$stationcode = as.character(geo$stationcode)
>>>>>>> 3a6e4c5a67a0d0570ba2c1faa1892e602f03f967
  
  uba = read.table(flist, skip = 0, header = TRUE, sep = ";")
  uba$date = as.POSIXct(as.character(uba$Datum), format = "%Y%m%d", origin = "MEZ")
  
  # Aggregate hourly values and set -999 to NA
  ch01 = which(colnames(uba) == "Wert01")
  ch24 = which(colnames(uba) == "Wert24")
  uba[, ch01:ch24][uba[, ch01:ch24] < 0] = NA
  uba$pm25 = rowMeans(uba[, ch01:ch24], na.rm = TRUE)
  uba$Station = as.character(uba$Station)
<<<<<<< HEAD
  uba = merge(uba, geo, by.x = "Station", by.y = "Stationscode")
  uba$lat = uba$`Breite dezimal`
  uba$lon = uba[, "Länge dezimal"]
  uba = uba[, -c(which(names(uba) == "Länge dezimal"), which(names(uba) == "Breite dezimal"))]
=======
  uba = uba[, c(1, 32, 33, 4:5 )]
  names(uba)[1:3] = c("stationcode", "date", "pm25")
  
  uba = merge(uba, geo, by = "stationcode")
  uba$lat = uba$lat
  uba$lon = uba$lon
>>>>>>> 3a6e4c5a67a0d0570ba2c1faa1892e602f03f967
  
  # Move each station to individual list and fill NAs
  aq = lapply(unique(uba$stationcode), function(s) {
    act = uba[uba$stationcode == s, ]
    
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
    
    return(act)
  })
  
  names(aq) = sapply(aq, function(s) s[1, "stationname"])
  
  return(aq)
  
}


