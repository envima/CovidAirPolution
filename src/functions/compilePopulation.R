# Compile population

compilePopulation = function(){
  
  pdata = read.table(file.path(envrmt$path_population, "destatis_population.csv"),
                     header = TRUE, sep = ";", dec = ",")
}


