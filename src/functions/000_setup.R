# Set environment for environmental information systems analysis

if(Sys.info()[["nodename"]] == "PC19616"){
  root_folder = path.expand("~/plygrnd/CovidAirPolution/")
} else {
  root_folder = path.expand("~/project/CovidAirPolution/")
}

fcts_folder = file.path(root_folder, "CovidAirPolution/src/functions/")

project_folders = c("data/",
                    "data/FR/",
                    "data/IT/",
                    "data/DE/",
                    "data/COVID-19/",
                    "data/covid-19-germany-gae/",
                    "data/report-data-platform-16133-94427-misc/",
                    "data/report-data-platform-16229-259611-lombardy",
                    "data/tmp/")

libs = c("doParallel", "caret", "data.table", "dtwclust", "ggplot2", "htmlTable", "htmltools", 
         "jsonlite", "plotly", "mapview", "stringr", "sf",
         "sf", "wavelets", "wmtsa", "mgcv", "biwavelet", "rnaturalearth", 
         "rnaturalearthhires",
         "tidyverse","tidyquant","timetk","forcats","tidyr", "WaveletComp")

envrmt = createEnvi(root_folder = root_folder,
                    fcts_folder = fcts_folder,
                    folders = project_folders, 
                    path_prefix = "path_", libs = libs,
                    alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
                    alt_env_root_folder = "F:\\BEN\\edu")

# More settings
# 
