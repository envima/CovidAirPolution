#' Compile population dataset

compilePopulation <- function() {
  pdata <- read.table(file.path(envrmt$path_population, "destatis_population.csv"),
    header = TRUE, sep = ";", dec = ","
  )
  pdata <- pdata[, c("nuts3Code", "X", "pop_total", "pop_male", "pop_female", "pop_dens")]
  return(pdata)
}


compilePopulationIT <- function() {
  pdata <- read.table(file.path(envrmt$path_population, "istat_population.csv"),
                      header = TRUE, sep = ";", dec = ","
  )
  
  pdata <- pdata[pdata[, 3] == "Totale", c("Provincia", "Totale.Maschi", "Totale.Femmine")]
  pdata$pop.total <- pdata$Totale.Maschi + pdata$Totale.Femmine
  names(pdata) <- c("nuts3Name", "pop_male", "pop_female", "pop_total")

  # Adjust some names to match the covid dataset
  pdata$nuts3Name[pdata$nuts3Name == "Valle dAosta/VallÃ©e dAoste"] <- "Aosta"
  pdata$nuts3Name[pdata$nuts3Name == "Bolzano/Bozen"] <- "Bolzano"
  pdata$nuts3Name[pdata$nuts3Name == "Massa-Carrara" ] <- "Massa Carrara"
  pdata$nuts3Name[pdata$nuts3Name == "Reggio nellEmilia"] <- "Reggio nell'Emilia"
  pdata$nuts3Name[pdata$nuts3Name == "LAquila"] <- "L'Aquila"

  return(pdata)
}