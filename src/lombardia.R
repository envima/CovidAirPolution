# Set up working environment and defaults --------------------------------------
library(envimaR)
root_folder = path.expand("~/project/CovidAirPolution/")
source(file.path(root_folder, "src/functions/000_setup.R"))


# Air quality data -------------------------------------------------------------
flist=Sys.glob(file.path(envrmt$path_data,"lombardia/*.csv"))
namelist<-list()
for (i in 1:length(flist)) {
  temp<-strsplit(flist,split = ".",fixed = T)[[i]][2]
  namelist[[i]]=strsplit(temp,split = "---",fixed = T)[[1]][1]
}
namelist<-unlist(namelist)

i=1
aq=list()
for (f in flist){
  aq[[i]]=read.table(f, skip = 5, header = T, sep = ",")
  aq[[i]]$date = as.Date(aq[[i]]$date)
  aq[[i]]$pm25 = as.numeric(as.character(aq[[i]]$pm25))
  aq[[i]] = aq[[i]][aq[[i]]$date >= as.Date("2020-02-01"), ]
  i= i + 1
}


# Covid-19 ---------------------------------------------------------------------
utils::download.file(url="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv",
                      destfile=file.path(envrmt$`path_COVID-19`, "dpc-covid19-ita-province.csv"))
utils::download.file(url="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
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
