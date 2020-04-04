# Get COVID-19 data for Italy

getCovidIT = function(){
  
  download.file(url = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv",
                destfile=file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-province.csv"))
  download.file(url = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
                destfile=file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-regioni.csv"))
  
  cov_admin2 = read.delim(file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-regioni.csv"), header = TRUE, sep = ",", dec=".")
  cov_admin2$data = as.Date(cov_admin2$data)
  
  cov_admin3 = read.delim(file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-province.csv"), header = TRUE, sep = ",", dec=".")  
  cov_admin3$data = as.Date(cov_admin3$data)
  cov_admin3$new_cases = c(0, diff(cov_admin3$totale_casi))
  
  cov_admin3$new_cases = NA
  for(c in unique(cov_admin3$codice_provincia)){
    cov_admin3[cov_admin3$codice_provincia == c, "new_cases"] = c(0, diff(cov_admin3[cov_admin3$codice_provincia == c, "totale_casi"]))
  }
  
  return(list(cov_admin2 = cov_admin2, cov_admin3 = cov_admin3))
  
}
