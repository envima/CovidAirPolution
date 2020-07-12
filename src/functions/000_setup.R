#' Set up libraries, functions and analysis environment.

if (Sys.info()[["nodename"]] == "PC19616") {
  root_folder <- path.expand("~/plygrnd/CovidAirPolution/")
} else {
  root_folder <- path.expand("~/project/CovidAirPolution/")
}

fcts_folder <- file.path(root_folder, "CovidAirPolution/src/functions/")

project_folders <- c(
  "data/",
  "data/COVID-19/",
  "data/covid-19-germany-gae/",
  "data/DE/",
  "data/FR/",
  "data/IT/",
  "data/world/",
  "data/population/",
  "data/report-data-platform-16133-94427-misc/",
  "data/report-data-platform-16229-259611-lombardy",
  "data/tmp/",
  "data/figures/"
)

libs <- c(
  "biwavelet", "caret", "cowplot", "data.table", "dtwclust",
  "forecast", "ggfortify", "ggplot2", "ggspatial", "htmltools",
  "lme4", "mapview", "MASS", "mgcv", "plotly", "rnaturalearth",
  "sf", "stringr", "tidygeocoder", "WaveletComp", "wavelets",
  "htmlTable", "jsonlite", "rnaturalearthhires", "sf",
  "tidyverse", "vegan", "doParallel", "forcats", "tidyquant",
  "tidyr", "timetk"
)

envrmt <- createEnvi(
  root_folder = root_folder,
  fcts_folder = fcts_folder,
  folders = project_folders,
  path_prefix = "path_", libs = libs,
  alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
  alt_env_root_folder = "F:\\BEN\\edu"
)
