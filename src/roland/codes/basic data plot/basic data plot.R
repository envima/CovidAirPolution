##setwd("C:/Users/Brandl/Desktop/PM Covid/covid data 10 extended")
## D A N G E R
## code clears all objects from the environment
rm(list = ls())

data <- readRDS("germany 100 2020-05-21.rds")$de_nuts3
n.areas <- names(data)

pm.test <- numeric()
for (i in (1:length(n.areas))) pm.test[i] <- mean(data[[i]]$pm_median)
hist(pm.test)
n.areas[which(pm.test == max(pm.test))]

user<-par(no.readonly=T)

par(mar=c(4,6,1,1)+0.4,lwd=2,las=1,cex=1.5, mfcol=c(3,1))

plot(data[[1]]$cases ~ as.Date(data[[1]]$date), type="n",
     xlim=as.Date(c("2020-03-01 CET", "2020-04-19 CEST")), axes=F,
     xlab="", ylab="")
ticks.x <- c("2020-03-01", "2020-03-15", "2020-04-01", "2020-04-15")
shut.down <- c("2020-03-14", "2020-03-17")
#mean.var <- (rep(0,length(data[[1]]$cases)))
n <- 0
axis(1, at=as.Date(ticks.x), labels=ticks.x)
axis(2)
for (i in (1:length(n.areas))) {
  lines(data[[i]]$cases ~ as.Date(data[[i]]$date), col="grey")
# mean.var <- mean.var + data[[i]]$cases
}
#mean.var <- mean.var/length(n.areas)
#lines(mean.var ~ as.Date(data[[1]]$date), col="blue")
abline(v=as.Date(ticks.x))
abline(v=as.Date(shut.down), col="red")
box()
mtext("Sum of infections", side=2, line=4,las=0)


plot(data[[1]]$new_cases ~ as.Date(data[[1]]$date), type="n",
     ylim=c(0,300),xlim=as.Date(c("2020-03-01 CET", "2020-04-19 CEST")), axes=F,
     xlab="", ylab="")
axis(1, at=as.Date(ticks.x), labels=ticks.x)
axis(2)
#mean.var <- (rep(0,length(data[[1]]$cases)))
for (i in (1:length(n.areas))) {
  lines(data[[i]]$new_cases ~ as.Date(data[[i]]$date), col="grey")
#  mean.var <- mean.var + data[[i]]$new_cases
}  
#mean.var <- mean.var/length(n.areas)
#lines(mean.var ~ as.Date(data[[1]]$date), col="blue")
abline(v=as.Date(ticks.x))
abline(v=as.Date(shut.down), col="red")
box()
mtext("New infections", side=2, line=4,las=0)

plot(data[[1]]$pm_median ~ as.Date(data[[1]]$date), type="n",
     ylim=c(0,100),xlim=as.Date(c("2020-03-01 CET", "2020-04-19 CEST")), axes=F,
     xlab="",ylab="")
axis(1, at=as.Date(ticks.x), labels=ticks.x)
axis(2)
for (i in (1:length(n.areas)))
  lines(data[[i]]$pm_median ~ as.Date(data[[i]]$date), col="grey")
abline(v=as.Date(ticks.x))
abline(v=as.Date(shut.down), col="red")
box()
mtext("Particulate matter", side=2, line=4,las=0)
mtext("Date", side=1, line=3,las=1)
par(user)


