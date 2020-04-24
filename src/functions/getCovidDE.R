# Get COVID-19 data for Germany

getCovidDE = function(){
  
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/ags.json",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "ags.json"))
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-ags.csv",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "cases-rl-crowdsource-by-ags.csv"))
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rki-by-ags.csv",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "cases-rki-by-ags.csv"))
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/deaths-rki-by-ags.csv",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "deaths-rki-by-ags.csv"))
  
  cov_nuts3 = read.delim(file.path(envrmt$`path_covid-19-germany-gae`, "cases-rki-by-ags.csv"), header = TRUE, sep = ",", dec=".")
  cov_nuts3$date = as.Date(cov_nuts3$time_iso8601)
  cov_nuts3$date = as.POSIXct(cov_nuts3$time_iso8601, origin = "CET")
  cov_nuts3 = cov_nuts3[, -c(which(colnames(cov_nuts3) == "time_iso8601"), which(colnames(cov_nuts3) == "sum_cases"))]
  cov_nuts3 = gather(cov_nuts3, "nuts3Code", "cases", -date)
  cov_nuts3$nuts3Code = substr(cov_nuts3$nuts3Code, 2, str_length(cov_nuts3$nuts3Code))
  
  cov_nuts3_deaths = read.delim(file.path(envrmt$`path_covid-19-germany-gae`, "deaths-rki-by-ags.csv"), header = TRUE, sep = ",", dec=".")
  cov_nuts3_deaths$date = as.POSIXct(cov_nuts3_deaths$time_iso8601, origin = "CET")
  cov_nuts3_deaths = cov_nuts3_deaths[, -c(which(colnames(cov_nuts3_deaths) == "time_iso8601"), which(colnames(cov_nuts3_deaths) == "sum_deaths"))]
  cov_nuts3_deaths = gather(cov_nuts3_deaths, "nuts3Code", "deaths", -date)
  cov_nuts3_deaths$nuts3Code = substr(cov_nuts3_deaths$nuts3Code, 2, str_length(cov_nuts3_deaths$nuts3Code))
  
  cov_nuts3 = merge(cov_nuts3, cov_nuts3_deaths)
  cov_nuts3$weekday = as.factor(weekdays(cov_nuts3$date))
  cov_nuts3$date_day = as.factor(paste(cov_nuts3$date, substr(cov_nuts3$weekday, 1, 1)))
  
  
  ags_names = fromJSON(file.path(envrmt$`path_covid-19-germany-gae`, "ags.json"), flatten = TRUE)
  ags_names = as.data.frame(do.call(rbind, ags_names))
  ags_names$nuts3Code = rownames(ags_names)
  names(ags_names)[1] = "nuts3Name"
  
  cov_nuts3 = merge(cov_nuts3, ags_names)
  cov_nuts3 = cov_nuts3[order(cov_nuts3$date), ]
  
  cov_nuts3$new_cases = NA
  cov_nuts3$new_cases_smooth = NA
  
  for(c in unique(cov_nuts3$nuts3Code)){
    print(c)
    
    tmp = cov_nuts3[cov_nuts3$nuts3Code == c,]
    tmp$smooth = round(loess.smooth(tmp$date, tmp$cases, family = "gaussian", span=0.33)$y, 0)

    tmp$smooth[tmp$smooth < 0] = 0
    # ggplot(tmp, aes(x = date_day, y = cases)) +
    #   geom_point() +
    #   geom_point(aes(x = date_day, y = smooth), col = "red") +
    #   theme(axis.text.x = element_text(angle = 90))

    cov_nuts3[cov_nuts3$nuts3Code == c, "cases_smooth"] = tmp$smooth
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_cases"] = c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "cases"]))
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_cases_smooth"] = c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "cases_smooth"]))
    
    tmp$smooth = round(loess.smooth(tmp$date, tmp$deaths, family = "gaussian", span=0.1)$y, 0)
    tmp$smooth[tmp$smooth < 0] = 0
    cov_nuts3[cov_nuts3$nuts3Code == c, "deaths_smooth"] = tmp$smooth
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_deaths"] = c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "deaths"]))
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_deaths_smooth"] = c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "deaths_smooth"]))
    
  }

  return(list(cov_nuts3 = cov_nuts3))
}

