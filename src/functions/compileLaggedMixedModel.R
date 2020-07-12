#' Compile lagged mixed effect model.

compileLaggedMixedModel <- function(data, lag_var = "pm_median", frml, nlags = 14,
                             subset_var = "cases", subset_thv = 1,
                             individual = "start", ndays = c(-14, 32),
                             obsprd_start = NULL, obsprd_end = NULL,
                             model = "gam") {
  
  data <- st_drop_geometry(data)
  
  data_lag <- lapply(unique(data$nuts3Code[data[, subset_var] >= subset_thv]), function(n) {
    tmp <- data[data$nuts3Code == n, ]

    if (individual == "start") {
      obsprd_start <- tmp$date[which(tmp$cases >= 1)[1] + ndays[1]]
      obsprd_end <- tmp$date[which(tmp$cases >= 1)[1] + ndays[2]]
    } else if (individual == "max") {
      obsprd_start <- tmp$date[tail(which(tmp$new_cases == max(tmp$new_cases)), n = 1) + ndays[1]]
      obsprd_end <- tmp$date[tail(which(tmp$new_cases == max(tmp$new_cases)), n = 1) + ndays[2]]
    } else {
      obsprd_start <- as.POSIXct("2020-02-15")
      obsprd_end <- as.POSIXct("2020-04-01")
    }
    if (is.na(obsprd_end)) obsprd_end <- 0

    if (obsprd_end > obsprd_start) {
      data_lag <- lapply(seq(0, nlags), function(l) {
        tmp <- data[data$nuts3Code == n, ]

        tmp <- tmp %>%
          mutate(pm_median_lag = dplyr::lag(get(lag_var), n = (l), default = NA))
        
        tmp_data = tmp[tmp$date >= obsprd_start & tmp$date <= obsprd_end, ]
        
        df = data.frame(
          nuts3Code = rep(n, nrow(tmp_data)),
          cases = tmp_data$cases,
          new_cases = tmp_data$new_cases,
          pop_total = tmp_data$pop_total,
          date_seq = seq(length(tmp_data$date)),
          weekday = tmp_data$weekday,
          pm_median_lag = tmp_data$pm_median_lag,
          lag = -l
        )
        return(df)
      })
      data_lag <- do.call("rbind", data_lag)
    } else {
      data_lag <- NULL
    }
  })
  data_lag <- do.call("rbind", data_lag)
  
  
  model_lag <- lapply(unique(data_lag$lag), function(l) {
    tmp <- data_lag[data_lag$lag == l, ]
    
    tmp$nuts3CodeFactor = as.factor(tmp$nuts3Code)
    tmp$pop_total_log10 = log10(tmp$pop_total)
    tmp$pm_median_lag_log10 = log10(tmp$pm_median_lag)
    
    frml <- as.formula(frml)
    
    if(model == "gamm"){
      set.seed(01042020)
      gamm_mixed <- gamm(frml,
                         random = list(nuts3CodeFactor=~1),
                         family = quasipoisson(link = "log"),
                         data = tmp
      )
      # summary(gamm_mixed$gam)
      # anova(gamm_mixed$gam)
      # summary(gamm_mixed$lme)
      # VarCorr(gamm_mixed$lme)
      # anova(gamm_mixed$lme)
      
      test_gam <- summary(gamm_mixed$gam)
      test_lme <- summary(gamm_mixed$lme)
      results = data.frame(
        lag = l,
        t_gam = test_gam$p.t["pm_median_lag"],
        p_gam = test_gam$p.pv["pm_median_lag"],
        pm_gam_estimate = test_gam$p.table["pm_median_lag", "Estimate"],
        pm_gam_std_error = test_gam$p.table["pm_median_lag", "Std. Error"],
        t_lme = test_lme$tTable["Xpm_median_lag", "t-value"],
        p_lme = test_lme$tTable["Xpm_median_lag", "p-value"],
        pm_lme_estimate = test_lme$tTable["Xpm_median_lag", "Value"],
        pm_lme_std_error = test_lme$tTable["Xpm_median_lag", "Std.Error"]
      )
      
      } else if(model == "gam"){
        set.seed(01042020)
        gam_mixed <- gam(frml,
                         data = tmp,method = "REML",
                         family = quasipoisson(link = "log"))
        # gam_mixed <- gam(frml,
        #                  offset = log10(pop_total_log10),
        #                  data = tmp,method = "REML",
        #                  family = quasipoisson(link = "log"))
        # summary(gam_mixed)
        # anova(gam_mixed)
        
        test <- summary(gam_mixed)
        results = data.frame(
          lag = l,
          t = test$p.t["pm_median_lag"],
          p = test$p.pv["pm_median_lag"]
        )
      }

    return(results)
    
  })
  model_lag <- do.call("rbind", model_lag)

  return(model_lag)
}
