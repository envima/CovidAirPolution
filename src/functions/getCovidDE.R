#' Get SARS-CoV-2 data for Germany.
#' 
#' Data is downloaded from the GitHub repository jgehrcke/covid-19-germany-gae.
#' Geographic information is included based on station locations.
#' Daily new infection cases are computed from the daily total sum. A loessed
#' version of daily cases is computed (loess function) for models which do not 
#' correct for weekdays to account for the reporting delay on weekends and 
#' Mondays.
#' 

getCovidDE = function(){
  
  # Download data.
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/ags.json",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "ags.json"))
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-ags.csv",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "cases-rl-crowdsource-by-ags.csv"))
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rki-by-ags.csv",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "cases-rki-by-ags.csv"))
  download.file(url = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/deaths-rki-by-ags.csv",
                destfile=file.path(envrmt$`path_covid-19-germany-gae`, "deaths-rki-by-ags.csv"))
  
  # Compile time and format for infection information.
  cov_nuts3 = read.delim(file.path(envrmt$`path_covid-19-germany-gae`, "cases-rki-by-ags.csv"), header = TRUE, sep = ",", dec=".")
  cov_nuts3$date = as.Date(cov_nuts3$time_iso8601)
  cov_nuts3$date = as.POSIXct(cov_nuts3$time_iso8601, origin = "CET")
  cov_nuts3 = cov_nuts3[, -c(which(colnames(cov_nuts3) == "time_iso8601"), which(colnames(cov_nuts3) == "sum_cases"))]
  cov_nuts3 = gather(cov_nuts3, "nuts3Code", "cases", -date)
  cov_nuts3$nuts3Code = substr(cov_nuts3$nuts3Code, 2, str_length(cov_nuts3$nuts3Code))
  
  # Compile time and format for death information.
  cov_nuts3_deaths = read.delim(file.path(envrmt$`path_covid-19-germany-gae`, "deaths-rki-by-ags.csv"), header = TRUE, sep = ",", dec=".")
  cov_nuts3_deaths$date = as.POSIXct(cov_nuts3_deaths$time_iso8601, origin = "CET")
  cov_nuts3_deaths = cov_nuts3_deaths[, -c(which(colnames(cov_nuts3_deaths) == "time_iso8601"), which(colnames(cov_nuts3_deaths) == "sum_deaths"))]
  cov_nuts3_deaths = gather(cov_nuts3_deaths, "nuts3Code", "deaths", -date)
  cov_nuts3_deaths$nuts3Code = substr(cov_nuts3_deaths$nuts3Code, 2, str_length(cov_nuts3_deaths$nuts3Code))
  
  # Merge the datasets.
  cov_nuts3 = merge(cov_nuts3, cov_nuts3_deaths)
  cov_nuts3$weekday = as.factor(weekdays(cov_nuts3$date))
  cov_nuts3$date_day = as.factor(paste(cov_nuts3$date, substr(cov_nuts3$weekday, 1, 1)))
  cov_nuts3$weekday_c = compileDetrendedTimeSeries(cov_nuts3$weekday, comp = "weekday_c")
  
  # Compile and add geographic information.
  ags_names = fromJSON(file.path(envrmt$`path_covid-19-germany-gae`, "ags.json"), flatten = TRUE)
  ags_names = as.data.frame(do.call(rbind, ags_names))
  ags_names$nuts3Code = rownames(ags_names)
  names(ags_names)[1] = "nuts3Name"
  
  cov_nuts3 = merge(cov_nuts3, ags_names)
  cov_nuts3 = cov_nuts3[order(cov_nuts3$date), ]
  
  # Compile daily new cases (original and loessed version).
  
  for(c in unique(cov_nuts3$nuts3Code)){
    
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_cases"] = 
      c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "cases"]))
    
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_deaths"] = 
      c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "deaths"]))
    
    tmp = cov_nuts3[cov_nuts3$nuts3Code == c,]
    tmp$loess = round(loess.smooth(seq(length(tmp$date)), tmp$cases, 
                                   family = "gaussian", span=0.33, 
                                   evaluation = length(tmp$date))$y, 0)
    tmp$loess[tmp$loess < 0] = 0
    cov_nuts3[cov_nuts3$nuts3Code == c, "cases_loess"] = tmp$loess
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_cases_loess"] = 
      c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "cases_loess"]))
    
    tmp$loess = round(loess.smooth(seq(length(tmp$date)), tmp$deaths, 
                                   family = "gaussian", span=0.1, 
                                   evaluation = length(tmp$date))$y, 0)
    tmp$loess[tmp$loess < 0] = 0
    cov_nuts3[cov_nuts3$nuts3Code == c, "deaths_loess"] = tmp$loess
    cov_nuts3[cov_nuts3$nuts3Code == c, "new_deaths_loess"] = 
      c(0, diff(cov_nuts3[cov_nuts3$nuts3Code == c, "deaths_loess"]))
    
    # tmp = compileDetrendedTimeSeries(data = cov_nuts3[cov_nuts3$nuts3Code == c, ],
    #                                  frml = "new_cases ~ date + weekday",
    #                                  comp = "detr")
    # cov_nuts3[cov_nuts3$nuts3Code == c, "cases_glm_time"] = tmp$pred_val
    # cov_nuts3[cov_nuts3$nuts3Code == c, "cases_glm_time_residuals"] = tmp$res_val
    # 
    # 
    # tmp = compileDetrendedTimeSeries(data = cov_nuts3[cov_nuts3$nuts3Code == c, ],
    #                                  frml = "new_cases ~ date + weekday",
    #                                  comp = "detr")
    # cov_nuts3[cov_nuts3$nuts3Code == c, "new_cases_glm_time"] = tmp$pred_val
    # cov_nuts3[cov_nuts3$nuts3Code == c, "new_cases_glm_time_residuals"] = tmp$res_val
    # 
    # 
    # tmp = compileDetrendedTimeSeries(data = cov_nuts3[cov_nuts3$nuts3Code == c, ],
    #                                  frml = "deaths ~ date + weekday",
    #                                  comp = "detr")
    # cov_nuts3[cov_nuts3$nuts3Code == c, "deaths_glm_time"] = tmp$pred_val
    # cov_nuts3[cov_nuts3$nuts3Code == c, "deaths_glm_time_residuals"] = tmp$res_val
    # 
    # 
    # tmp = compileDetrendedTimeSeries(data = cov_nuts3[cov_nuts3$nuts3Code == c, ],
    #                                  frml = "new_deaths ~ date + weekday",
    #                                  comp = "detr")
    # cov_nuts3[cov_nuts3$nuts3Code == c, "new_deaths_glm_time"] = tmp$pred_val
    # cov_nuts3[cov_nuts3$nuts3Code == c, "new_deaths_glm_time_residuals"] = tmp$res_val
    
  }
  
  return(list(cov_nuts3 = cov_nuts3))
}

