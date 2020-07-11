#' Compile population dataset

compilePopulation <- function() {
  pdata <- read.table(file.path(envrmt$path_population, "destatis_population.csv"),
    header = TRUE, sep = ";", dec = ","
  )
  pdata <- pdata[, c("nuts3Code", "X", "pop_total", "pop_male", "pop_female", "pop_dens")]
  return(pdata)
}
