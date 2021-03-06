#' Control compilation of publication media.

# Set up working environment and defaults.
library(envimaR)
if (Sys.info()[["nodename"]] == "PC19616") {
  source("~/plygrnd/CovidAirPolution/CovidAirPolution/src/functions/000_setup.R")
} else {
  source("~/project/cov/CovidAirPolution/src/functions/000_setup.R")
}

start_date <- as.POSIXct("2020-02-15")
end_date <- as.POSIXct("2020-04-15")

pm_vars <- c("PM2.5", "PM10")

lag_vars_set_lut <- (c(
  "median", "median, outlier removed w/o dust event", "median, outlier removed",
  "mean", "mean, outlier removed w/o dust event", "mean, outlier removed"
))
names(lag_vars_set_lut) <- c(
  "pm_median", "pm_median_estm", "pm_median_estm_best",
  "pm_mean", "pm_mean_estm", "pm_mean_estm_best"
)

lag_var <- "pm_mean_estm_best"

Sys.setlocale("LC_TIME", "English")

cmpldata <- lapply(pm_vars, function(pm) {
  cmpldata_file <- paste0("germany_", pm, "_extended.RDS")
  return(readRDS(file.path(envrmt$path_analysis, cmpldata_file)))
})
names(cmpldata) <- pm_vars



# Countrywide temporal development of PM values and SARS-CoV2 infections
figure_cntry_avg <- lapply(pm_vars, function(pm) {
  cntry_avg <- cmpldata[[pm]]$de_avg

  if(pm == "PM2.5"){
    pm_thv <- 25
  } else {
    pm_thv <- 50
  }
  
  cntry_avg_gam <- cntry_avg[cntry_avg$date >= as.POSIXct(start_date) & cntry_avg$date <= as.POSIXct(end_date), ]
  gam_model <- gam(new_cases ~ s(seq(length(date))) + weekday, family = quasipoisson, data = cntry_avg_gam)
  cntry_avg_gam$predicted <- predict(gam_model, cntry_avg_gam, type = "response")
  cntry_avg_gam$residuals <- residuals(gam_model, type = "response")

  cntry_indv <- cmpldata[[pm]]$de_clstr$clstr
  cntry_indv <- cntry_indv[cntry_indv$date >= as.POSIXct(start_date) & cntry_indv$date <= as.POSIXct(end_date), ]

  figure_cntry_avg <- ggplot() +
    geom_line(data = cntry_avg_gam, aes(x = date, y = get(lag_var), color = "Daily mean PM")) +
    geom_point(data = cntry_avg_gam, aes(x = date, y = get(lag_var), color = "Daily mean PM")) +
    geom_line(data = cntry_avg_gam, aes(x = date, y = new_cases, color = "Daily new cases")) +
    geom_point(data = cntry_avg_gam, aes(x = date, y = new_cases, color = "Daily new cases")) +
    geom_line(data = cntry_avg_gam, aes(x = date, y = predicted, color = "Daily new cases, explained by time")) +
    geom_point(data = cntry_avg_gam, aes(x = date, y = predicted, color = "Daily new cases, explained by time")) +
    geom_hline(yintercept = pm_thv, linetype = "dotted", color = "red") +
    geom_vline(xintercept = as.POSIXct("2020-03-14"), linetype = "dotted", color = "black") +
    geom_vline(xintercept = as.POSIXct("2020-03-17"), linetype = "dotted", color = "black") +
    labs(x = "Date and day of week", y = paste0("Infections, ", cntry_indv$pm_size[1])) +
    theme_bw() +
    scale_x_datetime(date_labels = "%d.%m, %a", date_breaks = "2 day", date_minor_breaks = "1 day") +
    scale_color_manual(
      values = c("#b2df8a", "#1f78b4", "#a6cee3"),
      labels = c(
        paste0("Daily ", cntry_indv$pm_size[1]), "Daily SARS-CoV-2 infections",
        "Daily SARS-CoV-2 infections, explained by time"
      )
    ) +
    theme(
      text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 10),
      legend.position = c(0.2, 0.7), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
      legend.title = element_blank(), panel.background = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    )

  figure_cntry_avg_pm <- ggplot() +
    geom_line(data = cntry_indv, aes(x = date, y = get(lag_var), group = nuts3Code, color = "All regions")) +
    geom_hline(yintercept = pm_thv, linetype = "dotted", color = "red") +
    geom_vline(xintercept = as.POSIXct("2020-03-14"), linetype = "dotted", color = "black") +
    geom_vline(xintercept = as.POSIXct("2020-03-17"), linetype = "dotted", color = "black") +
    labs(x = "Date and day of week", y = bquote(~ .(cntry_indv$pm_size[1]) ~ " [" ~ µm / m^3 ~ "]")) +
    theme_bw() +
    scale_x_datetime(date_labels = "%d.%m, %a", date_breaks = "2 day", date_minor_breaks = "1 day") +
    scale_color_manual(
      values = c("#4f4f4f"),
      labels = c(paste0("Daily mean ", pm))
    ) +
    theme(
      text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 10),
      legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
      legend.title = element_blank(), panel.background = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    )

  figure_cntry_avg_covid <- ggplot() +
    geom_line(data = cntry_indv, aes(x = date, y = new_cases, group = nuts3Code, color = "All regions")) +
    geom_vline(xintercept = as.POSIXct("2020-03-14"), linetype = "dotted", color = "black") +
    geom_vline(xintercept = as.POSIXct("2020-03-17"), linetype = "dotted", color = "black") +
    labs(x = "Date and day of week", y = "Infections") +
    theme_bw() +
    scale_x_datetime(date_labels = "%d.%m, %a", date_breaks = "2 day", date_minor_breaks = "1 day") +
    scale_color_manual(values = c("#4f4f4f")) +
    theme(
      text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 10),
      legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
      legend.title = element_blank(), panel.background = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    )
  return(list(
    cntry_avg = figure_cntry_avg, cntry_avg_pm = figure_cntry_avg_pm,
    cntry_avg_covid = figure_cntry_avg_covid
  ))
})
names(figure_cntry_avg) <- pm_vars
saveRDS(figure_cntry_avg, file.path(envrmt$path_figures, "germany_figure_cntry_avg.rds"))



# Correlation of PM and SARS-CoV2 infections individually by district
figure_gam_lag_set <- lapply(pm_vars, function(pm) {
  cntry_indv <- cmpldata[[pm]]$de_clstr$clstr
  gam_lag_set <- readRDS(file.path(envrmt$path_analysis, paste0("germany_", pm, "_gam_lag_vars_set.rds")))


  # Make figure (set).
  figure_gam_lag_set <- lapply(names(gam_lag_set), function(n) {
    gam_lag <- gam_lag_set[[n]]

    tstat <- lapply(seq(0, -15), function(i) {
      tmp <- t.test(gam_lag$t[gam_lag$lag == i])
      data.frame(
        lag = i,
        t = tmp$statistic,
        p = tmp$p.value,
        estimate = tmp$estimate,
        conf_min = tmp$conf.int[1],
        conf_max = tmp$conf.int[2]
      )
    })
    tstat <- do.call("rbind", tstat)

    gam_lag$fillcol <- FALSE
    gam_lag$fillcol[gam_lag$lag %in% tstat[tstat$p < 0.1, "lag"]] <- TRUE

    if (n == "pm_mean" | n == "pm_median") {
      ylabel_effect <- paste0("Effect size of ", cntry_indv$pm_size[1], ", raw data")
    } else if (n == "pm_mean_estm" | n == "pm_median_estm") {
      ylabel_effect <- paste0("Effect size of ", cntry_indv$pm_size[1], ", estimate")
    } else {
      ylabel_effect <- paste0("Effect size of ", cntry_indv$pm_size[1])
    }

    figure_gam_effectsize <- ggplot(data = gam_lag, aes(x = as.factor(lag), y = t, fill = fillcol, alpha = fillcol)) +
      geom_boxplot() +
      theme_bw() +
      geom_hline(yintercept = c(0), linetype = "dashed", color = "black") +
      labs(x = "Time lag [days]", y = ylabel_effect) +
      theme(
        text = element_text(size = 10), axis.title = element_text(size = 10),
        panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c("grey", "darkgreen")) +
      scale_alpha_manual(values = c(0.2, 0.6)) +
      theme(legend.position = "none")
    return(figure_gam_effectsize)
  })
  names(figure_gam_lag_set) <- names(gam_lag_set)
  return(figure_gam_lag_set)
})
names(figure_gam_lag_set) <- pm_vars
saveRDS(figure_gam_lag_set, file.path(envrmt$path_figures, "germany_figure_gam_lag_set.rds"))



# Correlation of PM and SARS-CoV2 infections with district as random effect
figure_gam_lag_mixed_set <- lapply(pm_vars, function(pm) {
  cntry_indv <- cmpldata[[pm]]$de_clstr$clstr
  gam_lag_mixed_set <- readRDS(file.path(envrmt$path_analysis, paste0("germany_", pm, "_gamm_lag_mixed_vars_set.rds")))


  # Make figure (set).
  figure_gam_lag_mixed_set <- lapply(names(gam_lag_mixed_set), function(n) {
    gam_lag_mixed <- gam_lag_mixed_set[[n]]$results

    gam_lag_mixed$fillcol <- FALSE
    gam_lag_mixed$fillcol[gam_lag_mixed$p_lme < 0.1] <- TRUE

    if (n == "pm_mean" | n == "pm_median") {
      ylabel_effect <- paste0("Effect size of ", cntry_indv$pm_size[1], ", raw data")
      ylabel_estimate <- paste0("Estimate of ", cntry_indv$pm_size[1], ", raw data")
    } else if (n == "pm_mean_estm" | n == "pm_median_estm") {
      ylabel_effect <- paste0("Effect size of ", cntry_indv$pm_size[1], ", estimate")
      ylabel_estimate <- paste0("Estimate of ", cntry_indv$pm_size[1], ", estimate")
    } else {
      ylabel_effect <- paste0("Effect size of ", cntry_indv$pm_size[1])
      ylabel_estimate <- paste0("Estimate of ", cntry_indv$pm_size[1])
    }

    figure_effectsize_mixed <- ggplot(
      data = gam_lag_mixed,
      aes(x = as.factor(lag), y = t_lme, color = fillcol, fill = fillcol, alpha = fillcol)
    ) +
      geom_point(size = 3) +
      theme_bw() +
      geom_hline(yintercept = c(0), linetype = "dashed", color = "black") +
      labs(x = "Time lag [days]", y = ylabel_effect) +
      theme(
        text = element_text(size = 10), axis.title = element_text(size = 10),
        panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c("black", "darkgreen")) +
      scale_color_manual(values = c("black", "darkgreen")) +
      scale_alpha_manual(values = c(1, 1)) +
      theme(legend.position = "none")
    figure_effectsize_mixed

    figure_estimate_mixed <- ggplot(
      data = gam_lag_mixed,
      aes(x = as.factor(lag), y = pm_lme_estimate, color = fillcol, fill = fillcol, alpha = fillcol)
    ) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = pm_lme_estimate - pm_lme_std_error, ymax = pm_lme_estimate + pm_lme_std_error)) +
      theme_bw() +
      geom_hline(yintercept = c(0), linetype = "dashed", color = "black") +
      labs(x = "Time lag [days]", y = ylabel_estimate) +
      theme(
        text = element_text(size = 10), axis.title = element_text(size = 10),
        panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c("black", "darkgreen")) +
      scale_color_manual(values = c("black", "darkgreen")) +
      scale_alpha_manual(values = c(1, 1)) +
      theme(legend.position = "none")
    figure_estimate_mixed

    return(list(effect = figure_effectsize_mixed, estimate_mixed = figure_estimate_mixed))
  })
  names(figure_gam_lag_mixed_set) <- names(gam_lag_mixed_set)
  return(figure_gam_lag_mixed_set)
})
names(figure_gam_lag_mixed_set) <- pm_vars
saveRDS(figure_gam_lag_mixed_set, file.path(envrmt$path_figures, "germany_figure_gam_lag_mixed_set.rds"))



# Longer-term correlation of PM and SARS-CoV2 infections
model_figure_cumulative_effect <- lapply(pm_vars, function(pm) {
  cntry_indv <- cmpldata[[pm]]$de_clstr$clstr

  add_info <- lapply(unique(cntry_indv$nuts3Code), function(n) {
    tmp <- cntry_indv[cntry_indv$nuts3Code == n, ]
    tmp$date_numeric <- seq(nrow(tmp))

    tmp <- data.frame(
      nuts3Code = tmp$nuts3Code[1],
      date_start = tmp$date[which(tmp$cases > 0)[1]],
      date_max = tmp$date[tail(which(tmp$new_cases == max(tmp$new_cases)), n = 1)],
      daily_max = tmp$new_cases[tail(which(tmp$new_cases == max(tmp$new_cases)), n = 1)],
      cases_at_max = tmp$cases[tail(which(tmp$new_cases == max(tmp$new_cases)), n = 1)],
      cases_on_0401 = tmp$cases[tmp$date == as.POSIXct("2020-04-01")]
    )
    return(tmp)
  })

  add_info <- do.call("rbind", add_info)
  add_info$difftime_start_max <- difftime(add_info$date_max, add_info$date_start, units = "days")
  add_info$difftime_first_shutdown <- difftime(add_info$date_start, as.POSIXct("2020-03-17"), units = "days")
  add_info$difftime_max_shutdown <- difftime(add_info$date_max, as.POSIXct("2020-03-17"), units = "days")

  tmp <- cntry_indv[cntry_indv$date <= as.POSIXct("2020-04-01"), ]

  cntry_agg <- aggregate(st_drop_geometry(tmp[lag_var]), by = list(tmp$nuts3Code), FUN = mean)
  colnames(cntry_agg) <- c("nuts3Code", "PM_tavrg")

  tmp <- tmp[tmp$date == as.POSIXct("2020-04-01"), ]
  cntry_agg <- merge(cntry_agg, tmp)
  cntry_agg <- merge(cntry_agg, add_info)

  cntry_agg$cases_log10 <- log10(cntry_agg$cases)
  cntry_agg$PM_tavrg_log10 <- log10(cntry_agg$PM_tavrg)
  cntry_agg$pop_total_log10 <- log10(cntry_agg$pop_total)
  cntry_agg$st_area_log10 <- log10(cntry_agg$st_area)


  # Test of the long-term correlation between PM and SARS-CoV2 infections
  tavrg <- cntry_agg[, c(
    "cases_log10", "centroid_lat", "centroid_lon", "pop_total_log10", "st_area_log10",
    "difftime_first_shutdown", "PM_tavrg_log10"
  )]

  lm_tavrg <- lm(cases_log10 ~ PM_tavrg_log10 + centroid_lat + centroid_lon +
    pop_total_log10 + st_area_log10 + difftime_first_shutdown, data = tavrg)
  lm_tavrg_smry <- summary(lm_tavrg)

  lm_tavrg <- lm(cases_log10 ~ 1, data = tavrg)
  lm_tavrg_stpAIC <- stepAIC(lm_tavrg,
    direction = "both",
    scope = list(
      lower = lm_tavrg,
      upper = ~ PM_tavrg_log10 + centroid_lat + centroid_lon +
        pop_total_log10 + st_area_log10 + difftime_first_shutdown
    )
  )
  lm_tavrg_stpAIC_smry <- summary(lm_tavrg_stpAIC)

  tavrg_fig <- cntry_agg[, c(
    "cases", "PM_tavrg", "pop_total", "st_area",
    "difftime_first_shutdown", "centroid_lat", "centroid_lon"
  )]

  tavrg_long <- gather(tavrg_fig, key = "var", value = "value", -cases)

  tavrg_long$var <- factor(tavrg_long$var,
    levels = c(
      "PM_tavrg", "pop_total", "st_area",
      "difftime_first_shutdown", "centroid_lat", "centroid_lon"
    ),
    labels = c(
      tmp$pm_size[1], "Population", "Area",
      "Time difference first case and shutdown", "Latitude", "Longitude"
    )
  )

  figure_cumulative_effect_log <- ggplot(
    tavrg_long[tavrg_long$var %in% c("Population", "Area", "PM2.5", "PM10"), ],
    aes(x = value, y = cases)
  ) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "", y = "Cumulative SARS-CoV-2 infections") +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    facet_wrap(~var, scales = "free") +
    theme_bw() +
    facet_wrap(~var, scales = "free")

  figure_cumulative_effect <- ggplot(
    tavrg_long[!tavrg_long$var %in% c("Population", "Area", "PM2.5", "PM10"), ],
    aes(x = value, y = cases)
  ) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "", y = "Cumulative SARS-CoV-2 infections") +
    scale_y_continuous(trans = "log10") +
    facet_wrap(~var, scales = "free") +
    theme_bw() +
    facet_wrap(~var, scales = "free")

  return(list(
    lm_tavrg_smry = lm_tavrg_smry, lm_tavrg_stpAIC_smry = lm_tavrg_stpAIC_smry,
    cumulative_effect_log = figure_cumulative_effect_log, cumulative_effect = figure_cumulative_effect,
    add_info = add_info
  ))
})
names(model_figure_cumulative_effect) <- pm_vars
saveRDS(model_figure_cumulative_effect, file.path(envrmt$path_figures, "germany_model_figure_cumulative_effect.rds"))



# Maps
# SARS-CoV2 infections on 2020-04-01
map_covid_infections <- lapply(pm_vars, function(pm) {
  background_map <- st_transform(ne_countries(scale = "large", continent = "europe", returnclass = "sf"),
    crs = "+init=epsg:25832"
  )
  layer <- st_transform(cmpldata[[pm]]$de_nuts3_map, crs = "+init=epsg:25832")
  map_covid_infections <- ggplot(data = background_map) +
    geom_sf() +
    geom_sf(data = layer, aes(fill = cases)) +
    scale_fill_viridis_c(trans = "log10") +
    theme(
      text = element_text(size = 10), axis.title = element_text(size = 10),
      legend.position = "bottom"
    ) +
    labs(fill = "Cumulative infections on 01.04.2020") +
    coord_sf(
      xlim = c(
        (st_bbox(layer)["xmin"] - 10000),
        (st_bbox(layer)["xmax"] + 10000)
      ),
      ylim = c(
        (st_bbox(layer)["ymin"] - 25000),
        (st_bbox(layer)["ymax"] + 10000)
      ), expand = FALSE
    ) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      height = unit(0.75, "cm"), width = unit(0.75, "cm"),
      pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
      style = north_arrow_minimal
    )
  return(map_covid_infections)
})
names(map_covid_infections) <- pm_vars
saveRDS(map_covid_infections, file.path(envrmt$path_figures, "germany_map_covid_infections.rds"))


# Long-term PM mean between 2020-02-15 and 2020-04-01
map_pm_mean <- lapply(pm_vars, function(pm) {
  background_map <- st_transform(ne_countries(scale = "large", continent = "europe", returnclass = "sf"),
    crs = "+init=epsg:25832"
  )
  layer <- st_transform(cmpldata[[pm]]$de_nuts3_map, crs = "+init=epsg:25832")
  map_pm_mean <- ggplot(data = background_map) +
    geom_sf() +
    geom_sf(data = layer, aes(fill = pm_mean_estm_best)) +
    scale_fill_viridis_c(trans = "log10") +
    theme(
      text = element_text(size = 10), axis.title = element_text(size = 10),
      legend.position = "bottom"
    ) +
    labs(fill = paste0("Mean ", cmpldata[[pm]]$de_nuts3_map$pm_size[1], " 15.02.2020 to 01.04.2020")) +
    coord_sf(
      xlim = c(
        (st_bbox(layer)["xmin"] - 10000),
        (st_bbox(layer)["xmax"] + 10000)
      ),
      ylim = c(
        (st_bbox(layer)["ymin"] - 25000),
        (st_bbox(layer)["ymax"] + 10000)
      ), expand = FALSE
    ) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      height = unit(0.75, "cm"), width = unit(0.75, "cm"),
      pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
      style = north_arrow_minimal
    )
  return(map_pm_mean)
})
names(map_pm_mean) <- pm_vars
saveRDS(map_pm_mean, file.path(envrmt$path_figures, "germany_map_pm_mean.rds"))



# Correlation between PM10 and PM2.5 and influence of outlier
cntry_indv_PM10 <- st_drop_geometry(cmpldata$PM10$de_clstr$clstr[, c("date", "nuts3Code", "pm_mean", "pm_mean_estm", "pm_mean_estm_best", "cases")])
colnames(cntry_indv_PM10) <- c("date", "nuts3Code", paste0(c("pm_mean", "pm_mean_estm", "pm_mean_estm_best", "cases"), "_PM10"))
cntry_indv_PM2.5 <- st_drop_geometry(cmpldata$PM2.5$de_clstr$clstr[, c("date", "nuts3Code", "pm_mean", "pm_mean_estm", "pm_mean_estm_best", "cases")])
colnames(cntry_indv_PM2.5) <- c("date", "nuts3Code", paste0(c("pm_mean", "pm_mean_estm", "pm_mean_estm_best", "cases"), "_PM2.5"))
cntry_indv_ovrlp <- merge(cntry_indv_PM10, cntry_indv_PM2.5, by = c("nuts3Code", "date"))
cntry_indv_ovrlp$pm_mean_diff_PM10 <- cntry_indv_ovrlp$pm_mean_PM10 - cntry_indv_ovrlp$pm_mean_estm_best_PM10
cntry_indv_ovrlp$pm_mean_diff_PM2.5 <- cntry_indv_ovrlp$pm_mean_PM2.5 - cntry_indv_ovrlp$pm_mean_estm_best_PM2.5

cntry_indv_ovrlp_cor <- cor(cntry_indv_ovrlp$pm_mean_estm_best_PM10, cntry_indv_ovrlp$pm_mean_estm_best_PM2.5)

# max(cntry_indv_ovrlp$pm_mean_estm_best_PM10, cntry_indv_ovrlp$pm_mean_estm_best_PM2.5)

figure_corr_PM10_PM2.5 <- ggplot(data = cntry_indv_ovrlp, aes(x = pm_mean_estm_best_PM2.5, y = pm_mean_estm_best_PM10)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = bquote(~"PM2.5 [" ~ µm / m^3 ~ "]"), y = bquote(~"PM10 [" ~ µm / m^3 ~ "]")) +
  coord_equal() +
  theme_bw() +
  xlim(0, 75) +
  ylim(0, 75) +
  theme(
    text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 10),
    legend.position = c(0.2, 0.7), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
    legend.title = element_blank(), panel.background = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )


# max(cntry_indv_ovrlp$pm_mean_PM10, cntry_indv_ovrlp$pm_mean_estm_best_PM10)

figure_PM10_outliers <- ggplot(data = cntry_indv_PM10, aes(x = pm_mean_PM10, y = pm_mean_estm_best_PM10)) +
  geom_point(alpha = 0.2) +
  labs(x = bquote(~"PM10, raw data [" ~ µm / m^3 ~ "]"), y = bquote(~"PM10 [" ~ µm / m^3 ~ "]")) +
  coord_equal() +
  theme_bw() +
  xlim(0, 75) +
  ylim(0, 75) +
  theme(
    text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 10),
    legend.position = c(0.2, 0.7), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
    legend.title = element_blank(), panel.background = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

figure_PM10_outliers_nbrs <- sum(cntry_indv_PM10$pm_mean_PM10 - cntry_indv_PM10$pm_mean_estm_best_PM10 != 0)


# max(cntry_indv_ovrlp$pm_mean_PM2.5, cntry_indv_ovrlp$pm_mean_PM2.5)

figure_PM2.5_outliers <- ggplot(data = cntry_indv_PM2.5, aes(x = pm_mean_PM2.5, y = pm_mean_estm_best_PM2.5)) +
  geom_point(alpha = 0.2) +
  labs(x = bquote(~"PM2.5, raw data [" ~ µm / m^3 ~ "]"), y = bquote(~"PM2.5 [" ~ µm / m^3 ~ "]")) +
  coord_equal() +
  theme_bw() +
  xlim(0, 75) +
  ylim(0, 75) +
  theme(
    text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 10),
    legend.position = c(0.2, 0.7), axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
    legend.title = element_blank(), panel.background = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

figure_PM2.5_outliers_nbrs <- sum(cntry_indv_PM2.5$pm_mean_PM2.5 - cntry_indv_PM2.5$pm_mean_estm_best_PM2.5 != 0)

cor_figure_corr_PM10_PM2.5 <- list(
  figure_corr_PM10_PM2.5 = figure_corr_PM10_PM2.5,
  cntry_indv_ovrlp_cor = cntry_indv_ovrlp_cor,
  figure_PM10_outliers = figure_PM10_outliers,
  figure_PM10_outliers_nbrs = figure_PM10_outliers_nbrs,
  figure_PM2.5_outliers = figure_PM2.5_outliers,
  figure_PM2.5_outliers_nbrs = figure_PM2.5_outliers_nbrs,
  n_PM10_nuts3_regions = length(unique(cntry_indv_PM10$nuts3Code)),
  n_PM2.5_nuts3_regions = length(unique(cntry_indv_PM2.5$nuts3Code)),
  n_PM10_PM2.5_nuts3_regions_overlap = length(unique(cntry_indv_ovrlp$nuts3Code))
)

saveRDS(cor_figure_corr_PM10_PM2.5, file.path(envrmt$path_figures, "germany_cor_figure_corr_PM10_PM2.5.rds"))
