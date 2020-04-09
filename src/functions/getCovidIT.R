# Get COVID-19 data for Italy

getCovidIT = function(){
  
  download.file(url = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv",
                destfile=file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-province.csv"))
  download.file(url = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
                destfile=file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-regioni.csv"))
  
  cov_nuts2 = read.delim(file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-regioni.csv"), header = TRUE, sep = ",", dec=".")
  cov_nuts2$date = as.POSIXct(cov_nuts2$data)
  names(cov_nuts2)[which(names(cov_nuts2) == "long")] = "lon"
  
  cov_nuts3 = read.delim(file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-province.csv"), header = TRUE, sep = ",", dec=".")  
  cov_nuts3$date = as.POSIXct(cov_nuts3$data)
  names(cov_nuts3)[which(names(cov_nuts3) == "long")] = "lon"
  
  cov_nuts3$new_cases = NA
  for(c in unique(cov_nuts3$codice_provincia)){
    cov_nuts3[cov_nuts3$codice_provincia == c, "new_cases"] = c(0, diff(cov_nuts3[cov_nuts3$codice_provincia == c, "totale_casi"]))
  }
  
  return(list(cov_nuts2 = cov_nuts2, cov_nuts3 = cov_nuts3))
  
}
