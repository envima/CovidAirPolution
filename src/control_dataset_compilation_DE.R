#' Control compilation of COVID and PM dataset for Germany.

# Set up working environment and defaults.
library(envimaR)
if (Sys.info()[["nodename"]] == "PC19616") {
  source("~/plygrnd/CovidAirPolution/CovidAirPolution/src/functions/000_setup.R")
} else {
  source("~/project/cov/CovidAirPolution/src/functions/000_setup.R")
}

start_date <- as.POSIXct("2020-02-15")
end_date <- as.POSIXct("2020-04-20")

pm_vars <- c("PM2.5", "PM10")

Sys.setlocale("LC_TIME", "English")


# Compile comprehensive analysis data or load precomputed dataset.
for(pm in pm_vars){
  cmpldata_file <- paste0("germany_", pm, "_extended.RDS")
  
  cmpldata <- compileDataDE(start_date = start_date, end_date = end_date, pm = pm)
  saveRDS(cmpldata, file.path(envrmt$path_analysis, cmpldata_file))
}


# Metadata: The following districts have been excluded within the preprocessing
# of cmpldata due to unsufficient temporal coverage.
# Check initially failed:
# [1] "LK Böblingen"                "LK Breisgau-Hochschwarzwald" "LK Erzgebirgskreis"          
#     "LK Esslingen"                "LK Ludwigsburg"             
# [6] "LK Mansfeld-Südharz"         "LK Nordfriesland"            "LK Oberhavel"                
#     "LK Schwarzwald-Baar-Kreis"   "LK Uelzen"                  
# [11] "SK Freiburg i.Breisgau"      "SK Heidelberg"               "SK Heilbronn"                
# "SK Pforzheim"    
# Check failed after NA interpolation of max. two consecutive NAs
# [1] "LK Erzgebirgskreis"  "LK Mansfeld-Südharz"
