# Create geo objects from UBA data

makedfUBA = function(flist){
  
  geo = read.table(file.path(envrmt$path_DE, "station_All_convert.csv"), skip = 1, header = TRUE, sep = ";", dec = ",")
  geo = geo[, c("Stationscode", "Stationsname", "Länge.dezimal", "Breite.dezimal")]
  geo$Stationscode = as.character(geo$Stationscode)
  
  uba = read.table(flist, skip = 0, header = TRUE, sep = ";")
  uba$date = as.POSIXct(as.character(uba$Datum), format = "%Y%m%d", origin = "MEZ")
  uba$pm25 = rowMeans(uba[, c(which(colnames(uba) == "Wert01") : which(colnames(uba) == "Wert24"))])
  uba$Station = as.character(uba$Station)
  uba = merge(uba, geo, by.x = "Station", by.y = "Stationscode")
  uba$lat = uba$Breite.dezimal
  uba$lon = uba[, "Länge.dezimal"]
  uba = uba[, -c(which(names(uba) == "Länge.dezimal"), which(names(uba) == "Breite.dezimal"))]
  
  aq = lapply(unique(uba$Station), function(s) {
    act = uba[uba$Station == s, ]
    return(act)
  })
  
  names(aq) = sapply(aq, function(x) x[1, "Stationsname"])
  
  return(aq)
  
}


