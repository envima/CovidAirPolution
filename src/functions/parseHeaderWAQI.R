# Parse header of WAQI data

getWAQIpos = function(header){
  

    info = stringr::str_extract_all(string = stringr::str_subset(header, "latitude"), pattern = "\\d+\\.*\\d*")[[1]]
    latlon = data.frame(Latitude = as.numeric(info[1]),
                        Longitude = as.numeric(info[2]))
    

  return(latlon)  
}


getWAQI_1 = function(header){
  
  
  return(header[1])  
}
