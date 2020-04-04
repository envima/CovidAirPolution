# Set environment for environmental information systems analysis

root_folder = path.expand("~/plygrnd/CovidAirPolution/")
fcts_folder = file.path(root_folder, "CovidAirPolution/src/functions/")

project_folders = c("data/",
                    "data/COVID-19/",
                    "data/report-data-platform-16133-94427-misc/",
                    "data/report-data-platform-16229-259611-lombardy",
                    "data/tmp/")

libs = c("data.table", "plotly", "ggplot2", "mapview", "stringr", "sf", "wavelets", "wmtsa", "biwavelet")

envrmt = createEnvi(root_folder = root_folder,
                    fcts_folder = fcts_folder,
                    folders = project_folders, 
                    path_prefix = "path_", libs = libs,
                    alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
                    alt_env_root_folder = "F:\\BEN\\edu")

# More settings

