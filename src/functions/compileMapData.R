#' Compile map data.

# Compile map datasets for Germany ---------------------------------------------
compileMapDE = function(nuts){
  
  map = lapply(nuts, function(n){
    cbind(n[n$date == as.POSIXct("2020-04-01"),],
          nuts3Area = st_area(n[1, c("nuts3Name", "nuts3Code")]))
  })
  map = do.call("rbind", map)
  return(map)
}