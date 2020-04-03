# Parse header of WAQI data

parseHeaderWAQI = function(header, getLatLon = TRUE){
  
  if(getLatLon){
    info = stringr::str_extract_all(string = str_subset(header, "latitude"), pattern = "\\d+\\.*\\d*")[[1]]
    latlon = data.frame(Latitude = as.numeric(info[1]),
               Longitude = as.numeric(info[2]))
  }

  return(list(latlon = latlon))  
}
