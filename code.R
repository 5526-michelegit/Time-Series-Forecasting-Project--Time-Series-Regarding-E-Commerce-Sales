
#importo le librerie

library(xts)
library(forecast)
library(tseries)
library(dplyr)
library(tidyverse)
library(magrittr) 
library(lubridate)
library(zoo)
library(rugarch)
library(forecast)
library(fGarch)
library(VineCopula)
library(copula)
library(psych)
library(fitdistrplus)
library(moments)
library(lmtest)
library(PerformanceAnalytics)
library(quantmod)
library(astsa)
library(tseries)
library(knitr)
library(kableExtra)
library(copula)





#importo il dataset
df <- read.csv("/Users/annamaria/Documents/Data Science/data science lab/serie-storiche-ecommerce5.csv", sep=";",header=TRUE)
head(data.frame(df))

##############  FASE DI PULIZIA DEL DATASET   ##################  


df1<-df%>%mutate(data=as.Date(data, '%d/%m/%y'),
                 fatturato=as.numeric(gsub(",", ".", gsub("\\.", "", fatturato))),
                 settore=as.character(settore))




fatturato.ts<-ts(df1$fatturato,start=c(2014,1), end=c(2021,12), frequency=12)
fatturato.ts 





df2<-df1 %>% mutate(Month=month(data),Year=year(data)) %>%
  group_by(Year,Month) %>% summarise(fatturato_total=sum(fatturato))%>% 
  mutate(data=as.yearmon(paste(Year, Month), "%Y %m"))%>%
  mutate(data2=as.Date(data, frac = 1))




df3<-df2 %>% filter(Year!='2022' & Year!='2013')



##############   DATA EXPLORATION ##############  

fatturato.ts<-ts(df3$fatturato_total,start=c(2014,1), end=c(2021,12), frequency=12)
fatturato.ts


plot(decompose(fatturato.ts))
boxplot(fatturato.ts ~ cycle(fatturato.ts))




plot(decompose(log(fatturato.ts),type='multiplicative'))
boxplot(log(fatturato.ts) ~ cycle(log(fatturato.ts)), xlab='cycle(log(fatturato))', ylab='log(fatturato)', main='Ciclicità')




fatturato_diff<-diff(log(fatturato.ts), lag = 1)
fatturato_diff   
plot(decompose(fatturato_diff,type='multiplicative'))






g= zoo(fatturato_diff)
(fit.std = fitdistr(g,"t"))

mu.std = fit.std$estimate[["m"]]
lambda = fit.std$estimate[["s"]]
nu = fit.std$estimate[["df"]]


hist(g, breaks = 20, density=20, prob=T, xlab='log differenza fatturato', main='Test fitting')
lines(density(g), col = "blue", lwd = 2)
x2 <- seq(min(g), max(g), length = 107)
f <- dnorm(x2, mean(g), sd(g))
lines(x2, f, col = "red", lwd = 2)
curve(dt((x-mu.std)/lambda, df=1)/lambda, col="green", lwd=2, add=TRUE, yaxt="n")


legend("topright", c("Histogram", "Density", "Normal","T-Student with df=1"), cex=0.5, box.lty = 0,
       lty = 1, col = c("black", "blue", "red","green"), lwd = c(1, 2, 2, 2))


kurtosis<-kurtosis(fatturato_diff)
skewness<-skewness(fatturato_diff)



##############   DATA MODELS ##############  




acfpl<-acf(fatturato_diff)
pacfpl<-pacf(fatturato_diff)
acfpl$lag <- acfpl$lag * 12
pacfpl$lag <- pacfpl$lag * 12



plot(acfpl, xlab="Lag (months)", main='Series - ACF')
plot(pacfpl, xlab="Lag (months)", main='Series - PACF')



arma<-auto.arima(fatturato_diff,ic='bic',trace = TRUE)




model1 <- arima(fatturato_diff, order = c(0,0,0))

model1 <- arima(fatturato_diff, order = c(0,0,0),
                seasonal = list(order = c(1,0,0)))

confint(model1)
cf <- coef(model1)
coeftest(model1)
checkresiduals(model1)



residuals<-residuals(model1)



acfpl<-acf(residuals)
pacfpl<-pacf(residuals)
acfpl$lag <- acfpl$lag * 12
pacfpl$lag <- pacfpl$lag * 12



plot(acfpl, xlab="Lag (months)")
plot(pacfpl, xlab="Lag (months)")




## PREVISIONE ##


predict(model1,n.ahead=1)
previsione<-forecast(model1,level=c(95),h=1)


previsione_post<-forecast(model1,level=c(95))
plot(previsione)


forecast1=predict(model1, 10)
plot(forecast1)




### MODELLO GARCH ##

plot(fatturato_diff, ylab='log(y_t) - log(y_t-1)')



garchspec<-ugarchspec(
  mean.model = list(armaOrder=c(0,0)),
  variance.model = list(model='eGARCH'),
  distribution.model = 'sstd')

garchfit<-ugarchfit(data=fatturato_diff,spec=garchspec)
round(garchfit@fit$matcoef,6)
plot(garchfit, which=4)
plot(garchfit, which=10)



garchvol <-sigma(garchfit)
str(garchvol1)
garchvol1 <- as.xts(garchvol,start=c(2014,1), end=c(2021,12))
au <- as.xts(fatturato_diff,start=c(2014,1), end=c(2021,12))
str(au)


plotvol <- plot(au, col = "steelblue1",main="Estimated Volatility vs. Observations",xlim=as.Date(c("2014-01-01","2021-12-01")))
plotvol <- addSeries(garchvol, col = "black", on = 1, lwd=2)


garchforecast <- ugarchforecast(fitORspec = garchfit,n.ahead=5)
cbind(head(fitted(garchforecast),5),head(sigma(garchforecast),5))


garchforecast <- ugarchforecast(fitORspec = garchfit,n.ahead=5)
cbind(head(fitted(garchforecast),5),head(sigma(garchforecast),5))
plot(garch_forecast, which=1)



######   COPULE    ######  


calcio<-df1%>%filter(settore=='Calcio')

fitness<-df1%>%filter(settore=='Fitness')

casual<-df1%>%filter(settore=='Casual')



serie1<-calcio %>% mutate(Month=month(data),Year=year(data)) %>%
  group_by(Year,Month) %>% summarise(fatturato_total=sum(fatturato))%>% 
  mutate(data=as.yearmon(paste(Year, Month), "%Y %m"))%>%
  mutate(data2=as.Date(data, frac = 1))


serie2<-fitness %>% mutate(Month=month(data),Year=year(data)) %>%
  group_by(Year,Month) %>% summarise(fatturato_total=sum(fatturato))%>% 
  mutate(data=as.yearmon(paste(Year, Month), "%Y %m"))%>%
  mutate(data2=as.Date(data, frac = 1))


serie3<-casual %>% mutate(Month=month(data),Year=year(data)) %>%
  group_by(Year,Month) %>% summarise(fatturato_total=sum(fatturato))%>% 
  mutate(data=as.yearmon(paste(Year, Month), "%Y %m"))%>%
  mutate(data2=as.Date(data, frac = 1))





calcio<-ts(serie1$fatturato_total,start=c(2014,1), end=c(2021,12), frequency=12)
calcio


casual<-ts(serie3$fatturato_total,start=c(2014,1), end=c(2021,12), frequency=12)
casual


fitness<-ts(serie2$fatturato_total,start=c(2014,1), end=c(2021,12), frequency=12)
fitness


m <- pobs(as.matrix(cbind(calcio,casual,fitness)))
pairs.panels(m)
cor(m, method='spearman')
GGally::ggpairs(as.data.frame(m), progress=FALSE)

res <- RVineTreeStructureSelect(m)

library(rgl)
options(rgl.useNULL = TRUE)
library(rgl)
plot3d(m[,1],m[,2],m[,3],pch=20,col='navyblue', type="h")
points(m, pch='.', col="red", cex=3)
rglwidget()


install.packages("scatterplot3d") 
library("scatterplot3d")
scatterplot3d(m, pch = 16, type='h')







library(fitdistrplus)
descdist(m[,1], discrete=FALSE, boot=500)
descdist(m[,2], discrete=FALSE, boot=500)
descdist(m[,3], discrete=FALSE, boot=500)

fit1 <- fitdist(m[,1], "unif")
summary(fit1)

fit2 <- fitdist(m[,2], "unif")
summary(fit2)

fit3 <- fitdist(m[,3], "unif")
summary(fit3)




n.cop <- normalCopula(dim=3)
set.seed(500)
fit <- fitCopula(n.cop,m,method='ml')
coef(fit)

set.seed(100)
myMvd <- mvdc(normalCopula(0.21,dim = 3), margins=c("unif", "unif", "unif"),
              paramMargins=list(list(min=0,  max=1),
                                list(min=0, max=1), 
                                list(min=0,max=1)) )





new_data <- rMvdc(106, myMvd)
pairs.panels(new_data)

s3d<-scatterplot3d(m, pch = 16,color='red')
s3d$points3d(new_data[,1], new_data[,2], new_data[,3],
             col = "black", pch = 3)
legend('topleft',c('Observed','Simulated'),col=c('red','black'),pch=21)
cor(new_data,method='spearman')





t.cop <- tCopula(dim=3)
set.seed(500)
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)



set.seed(10)
myMvd <- mvdc(copula=tCopula(0.17,dim=3,df=3), margins=c("unif", "unif", "unif"),
              paramMargins=list(list(min=0,  max=1),
                                list(min=0, max=1), 
                                list(min=0,max=1)) )





new_data <- rMvdc(100, myMvd)
pairs.panels(new_data)

s3d<-scatterplot3d(m, pch = 16,color='red')
s3d$points3d(new_data[,1], new_data[,2], new_data[,3],
             col = "black", pch = 3)
legend('topleft',c('Observed','Simulated'),col=c('red','black'),pch=21)
cor(new_data,method='spearman')




















