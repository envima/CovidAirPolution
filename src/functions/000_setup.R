# Set environment for environmental information systems analysis

if(Sys.info()[["nodename"]] == "PC19616"){
  root_folder = path.expand("~/plygrnd/CovidAirPolution/")
} else {
  root_folder = path.expand("~/project/cov/CovidAirPolution/")
}

fcts_folder = file.path(root_folder, "src/functions/")

project_folders = c("data/",
                    "data/COVID-19/",
                    "data/report-data-platform-16133-94427-misc/",
                    "data/report-data-platform-16229-259611-lombardy",
                    "data/tmp/")

libs = c("data.table", "ggplot2", "htmlTable", "plotly", "mapview", "stringr", 
         "sf", "wavelets", "wmtsa", "biwavelet", 
         "tidyverse","tidyquant","timetk","forcats","tidyr", "WaveletComp",
         "rnaturalearth", "rnaturalearthhighres")

envrmt = createEnvi(root_folder = root_folder,
                    fcts_folder = fcts_folder,
                    folders = project_folders, 
                    path_prefix = "path_", libs = libs,
                    alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
                    alt_env_root_folder = "F:\\BEN\\edu")

# More settings
