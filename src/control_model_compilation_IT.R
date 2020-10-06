#' Control compilation of COVID and PM gam and gam mixed effect models for Germany.

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

lag_vars_set <- c(
  "pm_median", "pm_median_estm", "pm_median_estm_best",
  "pm_mean", "pm_mean_estm", "pm_mean_estm_best"
)

Sys.setlocale("LC_TIME", "English")


for(pm in pm_vars){
  cmpldata_file <- paste0(pm, "_italy_extended.RDS")
  cmpldata <- readRDS(file.path(envrmt$path_analysis, cmpldata_file))
  cntry_indv <- cmpldata$it_clstr$clstr
  
  cntry_indv
  
  for(l in unique(cntry_indv$nuts3Code)){
    cntry_indv$new_cases[cntry_indv$nuts3Code == l & cntry_indv$new_cases < 0] <- 0
  }
  
  # Individual district models with date, weekday and PM as explanatroy variables.
  gam_lag <- lapply(lag_vars_set, function(lag_var){
    gam_lag <- compileLaggedGAM(
      data = cntry_indv, lag_var = lag_var,
      frml = "new_cases ~ s(seq(length(date))) + weekday + pm_median_lag",
      nlags = 15,
      subset_var = "new_cases", subset_thv = 10, individual = "start", ndays = c(0, 30)
    )
    saveRDS(gam_lag, file.path(envrmt$path_analysis, paste0(pm, "_gam_lag_", lag_var, ".rds")))
    return(gam_lag)
  })
  names(gam_lag) <- lag_vars_set
  saveRDS(gam_lag, file.path(envrmt$path_analysis, paste0(pm, "_gam_lag_vars_set.rds")))

  
  # Country wide mixed effect model with date, weekday and PM as explanatroy variables and district as random effect.
  cntry_indv <- cmpldata$it_clstr$clstr

  for(l in unique(cntry_indv$nuts3Code)){
    cntry_indv$new_cases[cntry_indv$nuts3Code == l & cntry_indv$new_cases < 0] <- 0
  }
  
  frml <- "new_cases ~ s(date_seq) + weekday + pm_median_lag"

  gamm_lag_mixed <- lapply(lag_vars_set, function(lag_var) {
    gamm_lag_mixed <- compileLaggedMixedModel(
      data = cntry_indv, lag_var = lag_var,
      frml = frml,
      nlags = 15,
      subset_var = "new_cases", subset_thv = 10, individual = "start", ndays = c(0, 30)
    )
    saveRDS(gamm_lag_mixed, file.path(envrmt$path_analysis, paste0(pm, "_gamm_lag_mixed_", lag_var, ".rds")))
    return(gamm_lag_mixed)
  })
  names(gamm_lag_mixed) <- lag_vars_set
  saveRDS(gamm_lag_mixed, file.path(envrmt$path_analysis, paste0(pm, "_gamm_lag_mixed_vars_set.rds")))
}
