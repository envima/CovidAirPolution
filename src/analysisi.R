setwd("C:/Users/Brandl/Desktop/PM2.5_Covid/covid data")

library(WaveletComp)

data <- readRDS("data.rds")
space <- names(data)
mean.pm <- data[[1]]$pm25_mean
mean.covid <- data[[1]]$new_cases
counter <- 1

data.series<-matrix(NA, nrow=length(space),ncol=3)
for ( i in (1:length(space))) {
  data.series[i,1] <- min(data[[i]]$date)
  data.series[i,2] <- max(data[[i]]$date)
  data.series[i,3] <- length(data[[i]]$date)
}

for ( i in (2:length(space))) {
  if (data.series[i,3] == 27) {
    counter <- counter +1
    mean.pm <- mean.pm + data[[i]]$pm25_mean
    mean.covid <- mean.covid + data[[i]]$new_cases
  }
}

erg1.ccf <-ccf(mean.pm/counter, mean.covid/counter)

erg<- glm(mean.covid ~ c(1:27), poisson)
erg2.ccf <- ccf(residuals(erg),mean.pm/counter)

user<-par(no.readonly=T)
par(mfcol=c(2,1), mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(mean.pm/counter ~ data[[1]]$date, type="l",
     ylim=c(0,30), xlab="", ylab="PM2,5/Covid")
lines(mean.covid/counter ~ data[[1]]$date, type="l",
      col="blue", lwd=3)
plot(residuals(erg) ~ data[[1]]$date, type="l",
     xlab="Date", ylab="Corona detrendet")
par(user)

data.wavelet <- data.frame(x=mean.pm/counter, 
                          y=residuals(erg))
erg.wavelet <- analyze.coherency(data.wavelet, 
                                 my.pair=c("x","y"),
                                 loess.span = 0)
wc.image(erg.wavelet)


