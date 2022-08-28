#Tome los datos de la confianza del consumidor en USA, 
#analice su comportamiento y genere un pronóstico para los próximos 12 meses
#con base en dos modelos, uno de suavizado y otro de corte autoregresivo.
#Clave: UMCSENT

library(quantmod)
library(TSstudio)
library(urca)
library(lmtest)
library(vars)
library(tseries) 
library(dynlm) 
library(ggplot2)
library(knitr) 
library(forecast) 
library(fBasics)
library(zoo)
library(PerformanceAnalytics)

#--- PRIMERO BAJO el Data Set de University Of Michigan Sentiment donde 100 es de 1960 aprox.

simbolo <- getSymbols("UMCSENT",src="FRED", auto.assign = FALSE)   #University of Michigan Sentiment
simbolo_series <- ts(simbolo, c(1952,11), frequency = 12)
confianza <- window(simbolo_series, 1990) # valores completos desde 1978

#--- Display Data Set
confianza
#--- Summary of Data Set
summary(confianza)
#--- Plot Data Set
plot(confianza)

#---- Plot decomposed data
ddata <- decompose(confianza, "multiplicative")
plot(ddata)

#--- El valor de Dickey-Fuller es de 0.5887 > 0.05
#--- La serie NO ES ESTACIONARIA... tenemos que hacer algo
adf.test(confianza)

#--- Hacemos diferencias y pum! ya tenemos estacionariedad p value < 5%
e_confianza <- diff(log(confianza))
plot(e_confianza)
adf.test(e_confianza)

#--- Si hacemos un Autocorrelation Function veo un Tail, ergo: Mejor usar un modelo AR
acf(e_confianza, main='Autocorrelations')
#--- vemos que hay cut-offs en Partial Correlation Function, por tanto puede ser un MA(2)
pacf(e_confianza, main='Partial Autocorrelations')

#--- con esto, vamos que nos arroja la función auto Simple Exponential Smoothing
#alpha: the "base value". Higher alpha puts more weight on the most recent observations.
#beta: the "trend value". Higher beta means the trend slope is more dependent on recent trend slopes.
#gamma: the "seasonal component". Higher gamma puts more weighting on the most recent seasonal cycles.
ses_modelo <- HoltWinters(e_confianza, alpha=0.2, beta=0.1, gamma=0.1)
#Visually evaluate the fits
plot(e_confianza, ylab="consumer confidence", xlim=c(2015,2022))
lines(ses_modelo$fitted[,1], lty=2, col="blue")

#Visually look at predictions
ses_modelo.pred <- predict(ses_modelo, 12, prediction.interval = TRUE, level=0.95)
#Visually evaluate the prediction
plot(e_confianza, ylab="consumer confidence", xlim=c(2020,2023))
lines(ses_modelo$fitted[,1], lty=2, col="blue")
lines(ses_modelo.pred[,1], col="red")
lines(ses_modelo.pred[,2], lty=2, col="orange")
lines(ses_modelo.pred[,3], lty=2, col="orange")

#--- Se ajusta bien nuestro modelo?
#--- Claro que si! todos los puntos por arriba del 5% lo cual indica residuos como ruido blanco
Box.test(residuals(ses_modelo), type = "Ljung-Box")

#--- con esto, vamos que nos arroja la función auto ARIMA
arima_modelo <- auto.arima(e_confianza)
summary(arima_modelo)
plot.ts(arima_modelo$residuals)

#--- Se ajusta bien nuestro modelo?
#--- Claro que si! todos los puntos por arriba del 5% lo cual indica residuos como ruido blanco
tsdiag(arima_modelo)
Box.test(residuals(arima_modelo), type = "Ljung-Box")

#--- ok ahora solamente utilizamos un forecast 
pronostico <- forecast(arima_modelo,h=12)
pronostico
plot(pronostico, ylab="consumer confidence", xlim=c(2020,2023))
