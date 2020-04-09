# Get COVID-19 data for Germany

getCovidDE = function(){
  
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/ags.json",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "ags.json"))
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-ags.csv",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "cases-rl-crowdsource-by-ags.csv"))
  
  cov_nuts3 = read.delim(file.path(envrmt$`path_covid-19-germany-gae`, "cases-rl-crowdsource-by-ags.csv"), header = TRUE, sep = ",", dec=".")
  cov_nuts3$date = as.Date(cov_nuts3$time_iso8601)
  cov_nuts3 = cov_nuts3[, -c(which(colnames(cov_nuts3) == "time_iso8601"), which(colnames(cov_nuts3) == "sum_cases"))]
  cov_nuts3 = gather(cov_nuts3, "nuts3Code", "cases", -date)
  cov_nuts3$nuts3Code = substr(cov_nuts3$nuts3Code, 2, str_length(cov_nuts3$nuts3Code))
  
  ags_names = fromJSON(file.path(envrmt$`path_covid-19-germany-gae`, "ags.json"), flatten = TRUE)
  ags_names = as.data.frame(do.call(rbind, ags_names))
  ags_names$nuts3Code = rownames(ags_names)
  names(ags_names)[1] = "nuts3Name"
  
  cov_nuts3 = merge(cov_nuts3, ags_names)
  
  cov_nuts3$new_cases = NA
  
  for(c in unique(cov_nuts3$nuts3Code)){
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_cases"] = c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "cases"]))
  }

  return(list(cov_nuts3 = cov_nuts3))
}

