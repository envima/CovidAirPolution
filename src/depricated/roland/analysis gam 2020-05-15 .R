#setwd("C:/Users/Brandl/Desktop/PM Covid/covid data 10 extended")
# D A N G E R
# code clears objects from the environment
rm(list = ls())

data <- readRDS("germany_extended_100.rds")$de_nuts3
n.areas <- names(data)

user<-par(no.readonly=T)
pdf("nuts_individual dynamics.gam.pdf")
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)

#############################################
## Important parameters for fit of the models
## and graphic output
##
time.span <- 30
time.lag <- 15
delta <-0
colour.lag <- rainbow(time.lag+1)
## 
##
#############################################

result.areas <- matrix(NA,ncol = time.lag+1,
                       nrow = length(n.areas))
result.areas.R2 <- matrix(NA,ncol = time.lag+1,
                       nrow = length(n.areas))
row.names(result.areas) <- n.areas

for (i in (1:length(n.areas))) {
  covid.start <- which(data[[i]]$new_cases > 0)[1] - delta
  covid.end <- covid.start+time.span-1
  if (dim(data[[i]])[1] > covid.end) {
    for (k in (0:time.lag)) {
      result.lag <-mgcv::gam(new_cases[covid.start:covid.end] ~ 
                         s(c(covid.start:covid.end)) +
                         weekday_c[covid.start:covid.end] +  
                         pm_mean[(covid.start-k):(covid.end-k)], 
                         quasipoisson, data = data[[i]])
      result.areas[i,k+1] <- summary(result.lag)$p.t[4]
      result.areas.R2[i,k+1] <- summary(result.lag)$dev.expl
      if (k==0)
         plot(data[[i]]$new_cases[covid.start:covid.end] ~ 
             data[[i]]$date[covid.start:covid.end],
             pch=21, cex=2, col="red", bg="red",
             ylab=n.areas[i], xlab="Days")
      lines(predict(result.lag, type="response")
            ~ data[[i]]$date[covid.start:covid.end],
            lwd=2, col=colour.lag[k])
      mondays <- which(data[[i]]$weekday=="Montag")
      abline(v=data[[i]]$date[mondays], col="grey", lwd=0.5)
#      fridays <- which(data[[i]]$weekday=="Freitag")
#      abline(v=data[[i]]$date[fridays], col="red", lwd=0.5)
    } 
  }
}
dev.off()
pdf("main.results.gam.pdf")
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5, mfcol=c(2,2))

mean.t <- colMeans(result.areas, na.rm = T)
mean.t.good <- numeric()
for (k in (0:time.lag)) {
  scr <- result.areas[,k+1]
  mean.t.good [k+1] <- mean(scr[result.areas.R2[,k+1]>0.5], na.rm=T)
}
sd.t <- apply(result.areas,2,sd, na.rm =T)
p.test.t <- apply(result.areas,2,function(a) t.test(a)$p.value)


plot(mean.t ~ c(0:-time.lag),type="l",
     ylim = c(-2.2,2.2),
     xlab="Time lag", ylab="Mean effect size")
arrows(x0=c(0:-time.lag), y0=mean.t-sd.t, 
       x1=c(0:-time.lag), y1=mean.t+sd.t, 
       code=3, angle=90, length=0.1)
points(c(0:-time.lag), mean.t, pch=21, cex=1.5,
       col = ifelse((p.test.t < 0.05), "red", "grey"),
       bg = ifelse((p.test.t < 0.05), "red", "grey"))
abline(h=0, col="grey")
lines(mean.t.good ~ c(0:-time.lag), col="blue", lwd=2)

# plotting proportion of positive cofficients against lag
# correlation Covid - t
proportion <- numeric()
binom.p <- numeric()
cor.twert.lat <- numeric() -> cor.twert.lon
cor.pwert.lat <- numeric() -> cor.pwert.lon
data.lat <- numeric()
data.lon <- numeric()
data.area <- numeric()

for (i in (1:length(n.areas))) {
  data.lat[i] <- data[[i]]$centroid_lat[1]
  data.lon[i] <- data[[i]]$centroid_lon[1]
  data.area[i] <- data[[i]]$area[1]
}
  
  for (i in c(1:(time.lag+1))) { 
  proportion[i] <- sum(result.areas[,i] > 0, na.rm=T)
  binom.p[i] <- binom.test(proportion[i],
                           sum(!is.na(result.areas[,i])))$p.value
  cor.twert.lat[i] <- cor(data.lat,
                      result.areas[,i], 
                      use = "complete.obs")
  cor.pwert.lat[i] <- cor.test(data.lat,
                           result.areas[,i])$p.value
  cor.twert.lon[i] <- cor(data.lon,
                      result.areas[,i], 
                      use = "complete.obs")
  cor.pwert.lon[i] <- cor.test(data.lon,
                           result.areas[,i])$p.value
}
proportion <- 100*proportion/
  apply(result.areas,2,function(a) sum(!is.na(a)))

plot(proportion ~ c(0:-time.lag), type="l",
     ylim = c(0,100), xlim=c(-time.lag,0), 
     xlab="Time lag", ylab="Positiv]e estimates[%]",
     lwd=3, col="blue")

points(c(0:-time.lag), proportion, pch=21, cex=1.5,
       col=ifelse(binom.p< 0.05,"red","grey"),
       bg=ifelse(binom.p< 0.05,"red","grey"))

abline(h=50, col="grey")

# correlation t-value with latitude
plot(cor.twert.lat ~ c(0:-time.lag), type="l",
     ylim = c(-0.5,0.4), xlim=c(-time.lag,0), 
     xlab="Time lag", ylab="Correlation latitude",
     lwd=3, col="blue")

points(c(0:-time.lag), cor.twert.lat, pch=21, cex=1.5,
       col=ifelse(cor.pwert.lat< 0.05,"red","grey"),
       bg=ifelse(cor.pwert.lat< 0.05,"red","grey"))
abline(h=0, col="grey")

# correlation t-value with longitude
plot(cor.twert.lon ~ c(0:-time.lag), type="l",
     ylim = c(-0.5,0.4), xlim=c(-time.lag,0), 
     xlab="Time lag", ylab="Correlation longitude",
     lwd=3, col="blue")

points(c(0:-time.lag), cor.twert.lon, pch=21, cex=1.5,
       col=ifelse(cor.pwert.lon< 0.05,"red","grey"),
       bg=ifelse(cor.pwert.lon< 0.05,"red","grey"))
abline(h=0, col="grey")

par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5, mfcol=c(1,2))
# histogram t-value sum of covid cases
hist(result.areas, 
     xlab="Effect size", ylab="Frequency", main= "",
     col="grey")
# histogram t-value sum of covid cases
hist(result.areas.R2, 
     xlab="Explained deviance", ylab="Frequency", main= "",
     col="grey")

par(user)
dev.off()
