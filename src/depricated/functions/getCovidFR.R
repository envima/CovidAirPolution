# Get COVID-19 data for France

getCovidFR = function(){

  download.file(url = "https://raw.githubusercontent.com/kalisio/covid-19/master/departements-france/departements-france.zip",
                destfile=file.path(envrmt$`path_COVID-19`, "departements.zip"))
  unzip(file.path(envrmt$`path_COVID-19`, "departements.zip"),overwrite = TRUE,exdir = envrmt$`path_COVID-19`)
  flist = list.files(file.path(envrmt$`path_COVID-19`),
                    pattern = "^.*\\.json$",full.names = TRUE,recursive = TRUE)

  flist =  flist[grepl(flist,pattern = "departements")]
  covid = lapply(flist, function(f){
   cov = as.data.frame(jsonlite::fromJSON(f,flatten = TRUE) )
   cov$date =strsplit(tools::file_path_sans_ext(basename(f)),split = "departements-france-")[[1]][2]
   return(cov)
  })


  plyr::rbind.fill(covid)
  cov_nuts3 = rbind.fill(covid)
  cov_nuts3$date = as.POSIXct(cov_nuts3$date)

  cov_nuts3$new_cases = c(0, diff(cov_nuts3$features.properties.Emergencies.Total))


  cov_nuts3$new_cases = NA
  for(c in unique(cov_nuts3$codice_provincia)){
    cov_nuts3[cov_nuts3$codice_provincia == c, "new_cases"] = c(0, diff(cov_nuts3[cov_nuts3$codice_provincia == c, "totale_casi"]))
  }

  return( cov_nuts3 = cov_nuts3)

}
