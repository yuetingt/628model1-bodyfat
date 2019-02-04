setwd("/Users/tyt/Documents/2019spring/stat628/module1")
dat<-read.csv("BodyFat.csv",row.names=1)
dim(dat)

#####################
### data cleaning ###
#####################
dat$HEIGHT=dat$HEIGHT*2.54
dat$WEIGHT=round(dat$WEIGHT*0.453592,2)
summary(dat)#bodyfat=0, weight=164.72,height=74.93,age=81
dat[dat$AGE==81,]#a very thin old man
dat[dat$WEIGHT==164.72,]#reasonable with adiposity
dat[dat$HEIGHT==74.93,]
dat$HEIGHT[42]=round(sqrt(92.99/29.9)*100,2)
dat=dat[-182,]#can't be imputed by density,495/1.1089 - 450 less than zero

plot(dat$BODYFAT~I(dat$DENSITY^(-1)),xlim=c(0.90,1.01),ylab="Bodyfat",xlab="1/Density",col="skyblue",pch=19,cex=0.7,main="Bodyfat vs 1/Density")
x=seq(0.9,1.01,0.01)
lines(x,y=x*495-450,col="navyblue",lwd=1.2)
text(0.99,43,"y=495/x-450",col="navyblue",cex=1)
text(1/1.0991,18.3,"96",col="navyblue",cex=1)
text(1/1.0665,5.4,"48",col="navyblue",cex=1)
text(1/1.0666,19.3,"76",col="navyblue",cex=1)

res=dat$BODYFAT-(495/dat$DENSITY - 450)
sort(abs(res),decreasing=T)[1:10]
order(abs(res),decreasing=T)[1:3]
dat[c(96,48,76),]
dat$BODYFAT[48]=round(495/1.0665-450,1)

#####################
### model fitting ###
#####################
m_full<-lm(BODYFAT~.-DENSITY,data=dat)
summary(m_full)
par(mfrow=c(2,2))
plot(m_full)
par(mfrow=c(1,1))
plot(m_full, which=4)
abline( h = 4/(251-14),lty=2,col="red")

library(car)
vif(m_full)

base=base <- lm(BODYFAT~1,data=dat)
AIC.base <- step(base,direction="forward",scope=list(lower=~1,upper=m_full),trace=T)

#abdomen is the best for one-factor model
m1<-lm(BODYFAT~ABDOMEN,data=dat)
m2<-lm(BODYFAT~ABDOMEN+WEIGHT,data=dat)
m3<-lm(BODYFAT~ABDOMEN+WEIGHT+WRIST,data=dat)
m4<-lm(BODYFAT~ABDOMEN+WEIGHT+WRIST+FOREARM,data=dat)

par(mfrow=c(2,2))
plot(dat$BODYFAT~dat$ABDOMEN)
abline(v=116,col="skyblue",lty="dashed")
text(122,43.5,"216",col="navyblue",cex=1)
text(146.5,32.5,"39",col="navyblue",cex=1)
plot(dat$ABDOMEN~dat$WEIGHT)
plot(dat$BODYFAT~dat$WRIST)
text(18.4,43.5,"216",col="navyblue",cex=1)
text(21.4,32.5,"39",col="navyblue",cex=1)
plot(dat$ABDOMEN~dat$WRIST)
abline(v=20,col="skyblue",lty="dashed")
text(18.4,124,"216",col="navyblue",cex=1)
text(21.4,146,"39",col="navyblue",cex=1)

cor(dat$ABDOMEN,dat$WEIGHT)
vif(m2)
vif(m4)

#ridge
m2_ridge=lm.ridge(BODYFAT~ABDOMEN+WEIGHT,data=dat,lambda = seq(0,10,0.1))
select(m2_ridge)
m2_ridge=lm.ridge(BODYFAT~ABDOMEN+WEIGHT,data=dat,lambda = 0.2)
#calculate MSE
pred.ridge <- coef(m2_ridge)[1] + coef(m2_ridge)[2]*dat$ABDOMEN + coef(m2_ridge)[3]*dat$WEIGHT
y=dat$BODYFAT
y_predicted=pred.ridge
sst <- sum((y - mean(y))^2) #Sum of Squares Total
sse <- sum((y_predicted - y)^2) #SSE
rsq <- 1 - sse / sst # R squared
MSE=mean((y_predicted - y)^2)

plot(dat$BODYFAT~dat$ABDOMEN)
curve(1.5*(x-75)^(0.75)+5,from=-10,to=150,add=T)#exists nonlinearity
library(car)
crPlots(m1)

library(ggplot2)
library(jtools)
interact_plot(lm(BODYFAT~WEIGHT*ABDOMEN,data=dat), pred = "WEIGHT", modx = "ABDOMEN",main="Interaction Plot")
interact_plot(lm(BODYFAT~WEIGHT*WRIST,data=dat), pred = "WRIST", modx = "WEIGHT",main="Interaction Plot")

#final model
m5<-lm(BODYFAT~ABDOMEN+WEIGHT:WRIST,data=dat)
summary(m5)
vif(m5)
mean(m5$residuals^2)#mse

#########################
### model diagonostic ###
#########################
par(mfrow=c(2,2))
plot(m5)
#normality
shapiro.test(m5$residuals)
#homoscedasticity
ncvTest(m5)

confint(m5)

#robustness
rlm12=rlm(dat$BODYFAT~dat$ABDOMEN+dat$WEIGHT:dat$WRIST);summary(rlm12)
## https://en.wikipedia.org/wiki/M-estimator

#####################
### rule of thumb ###
#####################

sample <- data.frame(ABDOMEN=85, WEIGHT=75, WRIST=17)
predict.lm(m5, newdata = sample, interval = 'confidence')

