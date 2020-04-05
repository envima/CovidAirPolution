# Set up working environment and defaults --------------------------------------
library(envimaR)
root_folder = path.expand("~/project/cov/CovidAirPolution/")
source(file.path(root_folder, "src/functions/000_setup.R"))
source(file.path(root_folder, "src/functions/parseHeaderWAQI.R"))
source(file.path(root_folder, "src/functions/makeSFPoints.R"))
source(file.path(root_folder, "src/functions/getCovidIT.R"))
# Air quality data -------------------------------------------------------------
#flist=Sys.glob(file.path(envrmt$`path_report-data-platform-16229-259611-lombardy`,"*.csv"))
flist=list.files(file.path(envrmt$`path_report-data-platform-16229-259611-lombardy`), pattern = "^.*\\.csv$", full.names = TRUE)

# make tables
aq<- sfWAQI(flist)
#make corresponding points
pts<- makeSFPoints(flist)
waqnames <- getWAQINames(flist)
act$statname<-substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
# create mapview data
pm25<-lapply(aq, function(x) x[,1:4])
for (i in 1:length(pm25))names(pm25[[i]])<-c("data","pm25","pm25min","pm25max")
pop = lapply(pm25, htmlTable)
mapview(pts, popup = pop,legend = FALSE, ncol="pm25max")

res<-getCovidIT()
cov_admin2<-res[[1]]
cov_admin3 <-res[[2]]





#---------------------------------------------------------
# plot data with plotly

# ggplot() + 
#   geom_line(data = cmb, aes(x = date, y = pm25, colour = stat)) + 
#   geom_line(data = cov_lombardia, aes(x = data, y = nuovi_positivi/10)) + 
#   scale_y_continuous(sec.axis = sec_axis(~.*10, name = "New cases")) +
#   geom_line(data = cov_Bergamo, aes(x = data, y = new_cases/10, colour = "Covid Bergamo")) + 
#   scale_y_continuous(sec.axis = sec_axis(~.*10, name = "New cases")) +
#   geom_line(data = cov_Milano, aes(x = data, y = new_cases/10, colour = "Covid Milano")) + 
#   scale_y_continuous(sec.axis = sec_axis(~.*10, name = "New cases"))

fig = plot_ly() 
for(l in seq_along(pm25)){
  
  fig = fig %>% add_trace(data = pm25[[l]], x = ~date, y = ~pm25, type = 'scatter', name = paste("pm 2.5",waqnames[l]), mode = 'lines+markers')
  
}

fig = fig %>% add_trace(data = cov_admin3[cov_admin3$denominazione_provincia == "Bergamo",], x = ~data, y = ~new_cases, yaxis = "y2", name = "New cases Bergamo", line=list(color ="red", width = 5, dash = 'dot') ,mode = 'lines+markers')

fig = fig %>% add_trace(data = cov_admin3[cov_admin3$denominazione_provincia == "Milano",], x = ~data, y = ~new_cases, yaxis = "y2", name = "New cases Milano", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dot'),mode = 'lines+markers')

fig = fig %>% layout(yaxis2 = list(overlaying = "y", side = "right", title = "new cases"))
fig



#---------------------------------------------------
# add waveletComp example for bergamo

# merge data colums
bergamo<-merge(x = cov_admin3[cov_admin3$denominazione_provincia == "Bergamo",], y = pm25[[8]], by = "data",sort = T)
head(bergamo)
plot(bergamo$new_cases,type = "l")
plot(bergamo$pm25,type = "l")

# convert date to factor
bergamo$data<-as.factor(as.character(bergamo$data))

# new_cases only is there a periodicy?

bergamo.w <- analyze.wavelet(bergamo, "new_cases",loess.span = 0,dt = 1, dj = 1/50,lowerPeriod = 1, upperPeriod = 28,make.pval = TRUE, n.sim = 10)

wt.image(bergamo.w, color.key = "interval", n.levels = 250,legend.params = list(lab = "wavelet power levels"),periodlab = "period (days)", timelab = "")
wt.image(bergamo.w, color.key = "interval", n.levels = 250,legend.params = list(lab = "wavelet power levels"),periodlab = "period (days)",label.time.axis = TRUE)

# - change scale 
max.power <- max(bergamo.w$Power)

wt.image(bergamo.w, color.key = "interval", n.levels = 250,legend.params = list(lab = "wavelet power levels"),periodlab = "period (days)",maximum.level = 1.001*max.power,show.date = TRUE, date.format = "%F", timelab = "")
wt.image(bergamo.w, color.key = "interval", n.levels = 250,legend.params = list(lab = "wavelet power levels",
                                                                           label.digits = 2),
           periodlab = "periods (days)", maximum.level = 1.25, 
         # Concerning item 1 above --- plot the square root of power:
         exponent = 0.5,
         # Concerning item 2 above --- time axis:
         show.date = FALSE, date.format = "%m%d", timelab = "", spec.time.axis = list(at = c(paste(2005:2014, "-01-01", sep = "")),labels = c(2005:2014)),timetcl = -0.5,  
         # draws outward ticks# Concerning item 3 above --- period axis:
         spec.period.axis = list(at = c(3, 7, 14, 21, 28)),periodtck = 1, periodtcl = NULL  )
         # draws horizontal lines)

#--------------------------------------------------------------------
# cross wavelet anaklysis

bergamo.wc <- analyze.coherency(bergamo,my.pair = c("new_cases", "pm25"),
                                loess.span = 0,dt = 1, dj = 1/50,lowerPeriod = 1, upperPeriod = 28,
                                make.pval = TRUE, n.sim = 10)

max.power <- max(bergamo.wc$Power.xy)  # for plotting
#save(bergamo.wc, file = "cross_wavelet_transform_temperature_over_humidity")

#load("cross_wavelet_transform_temperature_over_humidity")
exponent <- 0.5
wc.image(bergamo.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         color.key = "interval",
         maximum.level = (1.001*max.power)^exponent, exponent = exponent,
         # time axis:
         label.time.axis = TRUE, 
         show.date = TRUE,
         spec.time.axis = list(at = paste(2020-02-01:2020-04-02, "-01-01", sep = ""),labels = 2020-02:2020-04),timetcl = -0.5, 
         # outward ticks
         # period axis:
         periodlab = "period (days)",
         spec.period.axis = list(at = c(3, 7, 14, 21, 28)),periodtck = 1, periodtcl = NULL)





#---------------------------------------------------------
# timesereis tidy lags



#----------------
# Use tq_mutate() to get lags 1:28 using lag.xts()
k <- 1:15
col_names <- paste0("lag_", k)
#select(cov_admin3, -stato,  -sigla_provincia,   -lat,  -long ,-totale_casi, -note_it, -note_en)
tidyverse_lags <- select(cov_admin3, -stato,  -sigla_provincia,   -lat,  -long ,-totale_casi, -note_it, -note_en) %>%
  tibble::as_tibble() %>%
  tq_mutate(
    select     = new_cases,
    mutate_fun = lag.xts,
    k          = 1:15,
    col_rename = col_names
  )
#  select(tidyverse_lags, -stato,  -sigla_provincia,   -lat,  -long ,-totale_casi, -note_it, -note_en) %>% filter(denominazione_regione == "Lombardia" ) 
#summarise(all = sum(new_cases), n = n())

#------------------------
# Calculate the autocorrelations and 95% cutoffs
tidyverse_count_autocorrelations <-tidyverse_lags    %>% 
  gather(key = "lag", value = "lag_value", -c(data  ,    codice_regione,denominazione_regione,codice_provincia,denominazione_provincia,             new_cases)) %>%
  mutate(lag = str_sub(lag, start = 5) %>% as.numeric) %>%
  group_by(denominazione_regione, lag) %>%
  summarize(
    cor = cor(x = new_cases, y = lag_value, use = "pairwise.complete.obs"),
    cutoff_upper = 2/(n())^0.5,
    cutoff_lower = -2/(n())^0.5
  )
# tidyverse_count_autocorrelations %>%
#   group_by(codice_provincia) %>%
#   summarise(sum =sum(new_cases), n = n())

#------- final flow
tidyverse_count_autocorrelations %>%
  ggplot(aes(x = lag, y = cor, color = denominazione_regione  , group = denominazione_regione  )) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add cutoffs
  geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
  geom_line(aes(y = cutoff_lower), color = "blue", linetype = 2) +
  # Add facets
  facet_wrap(~ denominazione_regione  , ncol = 3) +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  scale_color_tq() +
  theme_tq() +
  labs(
    title = paste0("Tidyverse ACF Plot: Lags ", rlang::expr_text(k)),
    subtitle = "In Lombardia it appears to be a weekly pattern",
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
####---------------





# Get the absolute autocorrelations
tidyverse_absolute_autocorrelations <- tidyverse_count_autocorrelations %>%
  ungroup() %>%
  mutate(
    lag = as_factor(as.character(lag)),
    cor_abs = abs(cor)
  ) %>%
  select(lag, cor_abs) %>%
  group_by(lag) 
tidyverse_absolute_autocorrelations

#Visualize boxplot of absolute autocorrelations
break_point <- 1.5*IQR(tidyverse_absolute_autocorrelations$cor_abs) %>% signif(3)
tidyverse_absolute_autocorrelations %>%    
  ggplot(aes(x = fct_reorder(lag, cor_abs, .desc = TRUE) , y = cor_abs)) +
  # Add boxplot
  geom_boxplot(color = palette_light()[[1]]) +
  # Add horizontal line at outlier break point
  geom_hline(yintercept = break_point, color = "red") +
  annotate("text", label = paste0("Outlier Break Point = ", break_point), 
           x = 12, y = break_point + .03, color = "red") +
  # Aesthetics
  expand_limits(y = c(0, 1)) +
  theme_tq() +
  labs(
    title = paste0("Absolute Autocorrelations: Lags ", rlang::expr_text(k)),
    subtitle = "Weekly pattern is NOT above outlier break point",
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )