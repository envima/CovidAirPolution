#setwd("C:/Users/Brandl/Desktop/PM Covid/covid data 10 extended")
# D A N G E R
# code clears objects from the environment
rm(list = ls())
user<-par(no.readonly=T)

data <- readRDS("germany 100 2020-05-21.rds")$de_nuts3
n.areas <- names(data)
n <- length(n.areas)

#############################################
## Important parameters for fit of the models
## and graphic output
##
time.lag = 15
## 
##
#############################################
data.env <- data.frame(lat=rep(NA,n), lon=NA, area=NA, 
                       ratio=NA, density=NA, 
                       first=NA, 
                       pop=NA, 
                       covid=NA, # max slope
                       cases=NA, # total cases 1. April
                       covid.date=NA,
                       pm=NA, # mean pm
                       pm.lag=NA
                       )

for (i in (1:n)) {
  data.env$lat[i] <- data[[i]]$lat[1]
  data.env$lon[i] <- data[[i]]$lon[1]
  data.env$area[i] <- data[[i]]$st_area[1]/100000000
  data.env$ratio[i] <- data[[i]]$pop_male[1]/
                       data[[i]]$pop_female[1]
  data.env$density[i] <- data[[i]]$pop_dens[1]
  data.env$first[i] <- difftime(data[[i]]$date[
                               which(data[[i]]$cases>0)[1]],
                                 data[[i]]$date[1],units="days")
  data.env$pop[i] <- data[[i]]$pop_total[1]
  scr <- residuals(glm(cases ~ date + weekday, 
                    quasipoisson, data = data[[i]]))
  data.env$covid[i] <- max(scr) # corrected for dynamics via glm
  data.env$cases[i] <- max(data[[i]]$cases)
  data.env$covid.date[i] <- which(scr == max(scr))
  data.env$pm[i]<- mean(data[[i]]$pm_median)
}

data.env$area <- log10(data.env$area)
data.env$density <- log10(data.env$density)
data.env$pop <- log10(data.env$pop)
data.env$covid <- log10(data.env$covid)
data.env$cases <- log10(data.env$cases)
data.env$pm <- log10(data.env$pm)
pc.env <- princomp(~ area+first+lat+lon+pop+ratio,
                   cor=T,data=data.env)

# test of the slope between population and covid cases
erg.slope <- lm(cases ~ pop, offset = pop, data = data.env)
summary(erg.slope)

erg.slope <- lm(covid ~ pop, offset = pop, data = data.env)
summary(erg.slope)

erg.model <- lm(cases ~ lat + lon + pop + area + first + pm,  data=data.env)
summary(erg.model)
summary(MASS::stepAIC(erg.model))

