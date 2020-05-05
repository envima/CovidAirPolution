setwd("C:/Users/Brandl/Desktop/PM2.5_Covid/covid data Germany")

data <- readRDS(file.path(envrmt$path_tmp,"Germany 2020-04-30.rds"))
n.areas <- names(data)

# first data set with insuficient length
begin <- which(data[[2]]$date == "2020-02-15 CET")
end <- which(data[[2]]$date == "2020-04-01 CEST")
dates.used <- data[[2]]$date[begin:end]

basic.pm <- matrix(data=NA,nrow=length(n.areas),
                   ncol=length(dates.used))
basic.covid <- basic.pm

# selecting only araes with complete data between
# February 15 and April 1, 2020
# incomplete data NA
for (i in (1:length(n.areas))) {
  begin <- which(data[[i]]$date == "2020-02-15 CET")
  end <- which(data[[i]]$date == "2020-04-01 CEST")
  if (length(begin-end+1)>0) {
    if ((end-begin+1) == length(dates.used)) {
      basic.pm[i,] <- data[[i]]$pm_mean[begin:end]
      basic.covid[i,] <- data[[i]]$new_cases[begin:end]
    }}}

# calculating mean for PM 2,5
mean.pm <- colMeans(basic.pm,na.rm = T)
# calculating sum of covid cases
sum.covid <- colSums(basic.covid,na.rm = T)
# retrieving weekday of the dates
week.day <- weekdays(dates.used)

# recoding weekdays into three categories:
# M  for Monday
# SS for Weekend
# W  for week
week.day.c <- week.day
for (i in (1:length(week.day)))
  week.day.c[i] <- ifelse((week.day[i] == "Montag"), "M", 
                          ifelse((week.day[i] == "Sonntag") | 
                                   (week.day[i] == "Samstag"),
                                 "SS","W"))
week.day.c <- as.factor(week.day.c)

# simple glm with log-link to correct for 
# increase in covid cases
result.covid <- glm(sum.covid ~ dates.used + 
                      week.day.c, poisson)
summary(result.covid)

# plotting results
user<-par(no.readonly=T)
par(mfcol=c(2,1), mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(mean.pm ~ dates.used, type="l",
     ylim=c(0,30), xlab="", ylab="PM2,5/Covid")
lines(sum.covid/100 ~ dates.used, type="l",
      col="blue", lwd=3)
lines(exp(predict(result.covid))/100 ~ dates.used, type="l",
      col="red", lwd=3)
plot(residuals(result.covid) ~ dates.used, type="l",
     xlab="Date", ylab="Corona residuals", lwd=2, col="red")
par(user)

#cross correlation
erg2.ccf <- ccf(mean.pm, residuals(result.covid))
#######################
# modelling covid cases
#######################
# accounting for overdispersion
# 1. quasipoisson
result.covid.pm.quasi <- glm(sum.covid ~ dates.used +
                               week.day.c + mean.pm, quasipoisson)
summary(result.covid.pm.quasi)

#2. negative binomial - error when not setting initial 
#   value of theta (init.theta) 
library(MASS)
result.covid.pm.nb <- glm.nb(sum.covid ~ dates.used +
                               week.day.c + mean.pm,
                             init.theta = 2)
summary(result.covid.pm.nb)

#3. subject specific random term
library(lme4)
result.covid.pm.glmer <- glmer(sum.covid ~ c(1:length(dates.used)) + 
                                 week.day.c + mean.pm + 
                                 (1|as.factor(c(1:length(dates.used)))),
                               family=poisson)
summary(result.covid.pm.glmer)


# using pm with time lag for summed covid data
# quasipoisson
time.lag <- 0:15
t.pm.lag <- numeric()
t.pm.p <- numeric()
for (i in time.lag) {
  result.lag <- glm(sum.covid[16:length(dates.used)] ~ 
                      dates.used[16:length(dates.used)]+
                      week.day.c[16:length(dates.used)]+ 
                      mean.pm[(16-i):(length(dates.used)-i)], 
                    quasipoisson)
  t.pm.lag[i+1] <- summary(result.lag)$coefficients[5,3]
  t.pm.p[i+1] <- summary(result.lag)$coefficients[5,4]
}

# plot of time lag for sum of covid cases across all areas
# significant coefficients in red
user<-par(no.readonly=T)
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(t.pm.lag ~ time.lag, type="l", lwd=2,
     xlab="time lag", ylab="t of PM coefficient")

points(time.lag, t.pm.lag, pch=21, cex=1.5,
       col=ifelse((t.pm.p< 0.06),"red", "blue"),
       bg=ifelse((t.pm.p< 0.06),"red", "blue"))
par(user)

# using pm with time lag; quasipoisson
# areas analysed separately
# using only araes with at least min.cases of 
# persons with corona
time.lag <- 0:15
min.cases <- 0
result.areas <- matrix(NA,ncol=16,
                       nrow=sum(rowSums(basic.covid)>min.cases,
                                na.rm=T))
k <- 0 # counts the areas
for (a in (1:length(n.areas))) {
  if (rowSums(basic.covid,na.rm=T)[a] >min.cases) {
    k <- k+1
    covid <- basic.covid[a,]
    pm <- basic.pm[a,]
    for (i in time.lag) {
      result.lag <- glm(covid[16:length(dates.used)] ~ 
                          dates.used[16:length(dates.used)]+
                          week.day.c[16:length(dates.used)]+ 
                          pm[(16-i):(length(dates.used)-i)], 
                        quasipoisson)
      result.areas[k,i+1] <- summary(result.lag)$coefficients[5,3]
    }
  }
}  

# plotting t-values for each area and time lag
# adding the mean
user<-par(no.readonly=T)
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(result.areas[,1] ~ rep(0,nrow(result.areas)),
     ylim = c(-6,6), xlim=c(0,15), pch=21, bg="grey",
     xlab="time lag", ylab="t of PM coefficients")
for (i in (1:15))
  points(rep(i,nrow(result.areas)), result.areas[,i+1],
         pch=21, bg = "grey")
lines(colMeans(result.areas)~time.lag, lwd=4,col="blue")
par(user)

# plotting results with lines for each area
# adding mean 
user<-par(no.readonly=T)
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(result.areas[1,] ~ time.lag, type="l",
     ylim = c(-6,6), xlim=c(0,15), col="grey",
     xlab="time lag", ylab="t of PM coefficients")
for (i in (2:nrow(result.areas)))
  lines(result.areas[i,] ~ time.lag, type="l", col="grey")
lines(colMeans(result.areas)~time.lag, lwd=4,col="blue")
par(user)

# proportion of significant coefficients
sum(abs(result.areas)>2)/
  (nrow(result.areas)*ncol(result.areas))
# 16 % significant! data of an area not independent

# plotting proportion of positive cofficients against lag
# correlation Covid - t
proportion <- numeric()
binom.p <- numeric()
cases <- rowSums(basic.covid[
  which(rowSums(basic.covid)>min.cases),])
cor.twert <- numeric()
cor.pwert <- numeric()
for (i in c(1:16)) { 
  proportion[i] <- sum(result.areas[,i] > 0)
  binom.p[i] <- binom.test(proportion[i],
                           nrow(result.areas))$p.value
  cor.twert[i] <- cor(log10(cases),result.areas[,i])
  cor.pwert[i] <- cor.test(log10(cases),result.areas[,i])$p.value
}
proportion <- 100*proportion/nrow(result.areas)

user<-par(no.readonly=T)
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(proportion ~ time.lag, type="l",
     ylim = c(0,100), xlim=c(0,15), 
     xlab="time lag", ylab="% positive estimates",
     lwd=3, col="blue")

points(time.lag, proportion, pch=21, cex=1.5,
       col=ifelse(binom.p< 0.05,"red","grey"),
       bg=ifelse(binom.p< 0.05,"red","grey"))

abline(h=50, col="grey")
par(user)

# correlation t-value sum of covid cases
user<-par(no.readonly=T)
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(cor.twert ~ time.lag, type="l",
     ylim = c(-0.4,0.2), xlim=c(0,15), 
     xlab="time lag", ylab="correlation",
     lwd=3, col="blue")

points(time.lag, cor.twert, pch=21, cex=1.5,
       col=ifelse(cor.pwert< 0.05,"red","grey"),
       bg=ifelse(cor.pwert< 0.05,"red","grey"))

par(user)

# ploting the most extreme correlation
i <- which(cor.twert==min(cor.twert))
user<-par(no.readonly=T)
par(mar=c(4,4,1,1),lwd=2,las=1,cex=1.5)
plot(result.areas[,i] ~ log10(cases), pch=21, cex=1.5,
     xlab="Sum of corona", ylab="t-value",
     axes=F, xlim =log10(c(10,4000)),
     col=ifelse(abs(result.areas[,i])>2,"red","grey"),
     bg=ifelse(abs(result.areas[,i])>2,"red","grey"))
ticks.x <- c(10,25, 100,250,1000, 2500)
axis(1, at=log10(ticks.x), labels = ticks.x)
axis(2)
box()
abline(lm(result.areas[,i] ~ log10(cases)))

par(user)