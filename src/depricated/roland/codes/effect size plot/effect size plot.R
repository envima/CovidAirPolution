##setwd("C:/Users/Brandl/Desktop/PM Covid/covid data 10 extended")
## D A N G E R
## code clears all objects from the environment
rm(list = ls())

data <- readRDS("germany 100 2020-05-21.rds")$de_nuts3
n.areas <- names(data)

############################################
##Selecting only nuts with a minimum of 
##new cases - infections
test.sel <- numeric()
for (i in (1:length(n.areas)))
   test.sel[i] <- max(data[[i]]$new_cases)

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
      #############################################################
      ## data.lag data frame for each analysis 
      ## Here one can select variable
      
      data.lag <- data.frame(
        cases = data[[i]]$new_cases[covid.start:covid.end],
        time = c(covid.start:covid.end),
        day = data[[i]]$weekday_c[covid.start:covid.end],
        pm = data[[i]]$pm_mean[(covid.start-k):(covid.end-k)])
      ##
      ##############################################################
      if (test.sel[i] > 9) {
        result.lag <-mgcv::gam(cases ~  s(time) + day + pm, 
                           quasipoisson, data = data.lag)
        result.areas[i,k+1] <- summary(result.lag)$p.t["pm"]
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
}
dev.off()

par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)

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


