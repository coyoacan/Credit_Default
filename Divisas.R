# Ejercicio
# Genere un modelo capaz de explicar la relación entre
# divisas

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

# MODELOS VAR
# Son una extensión de los Modelos Autoregresivos Univariantes
# su particularidad es que no hay variables endÃ³genas y exÃ³genas

"**************************"
#PRIMERO BAJO TOKIO MOROKIO
"**************************"

moeuro <- getSymbols("EXUSEU",src="FRED",from ="1999-01-01", to="2022-07-01")   #EURSUD mensual
moeuro = ts(EXUSEU, start = c(1999, 1),frequency = 12)
mopound <- getSymbols("EXUSUK",src="FRED",from ="1999-01-01", to="2022-07-01")   #EURSUD mensual
mopound = ts(EXUSUK, start = c(1971, 1),frequency = 12)
mocad <- getSymbols("EXCAUS",src="FRED",from ="1999-01-01", to="2022-07-01")   #EURSUD mensual
mocad = ts(EXCAUS, start = c(1971, 1), frequency = 12)

"******************************************************************"
#PLOTEO PERO TODO SALE SUPER FEO, TENGO QUE CORTAR TIEMPECILLOS
"*********************************************************************"

ggplot2::autoplot(cbind(moeuro,mopound,mocad), main ="EX Rates", ylab = "Whatever")

#SIENDO ASI HAGO UN WINDOW PARA OBTENER TODAS LAS SERIES EN EL MISMO PERIODO

mopound2 <- window(mopound, start = 1999)
mocad2 <- window(mocad, start = 1999)

#PLOTEO Y TODO ME SALE ULTRA CHIDISIMO
ggplot2::autoplot(cbind(moeuro,mopound2,mocad2), main ="EX Rates", ylab = "Whatever")

#USD CAD LE HAGO LA INVERSA PARA QUE TODO QUEDE IGUALITO
mocad3 = 1/mocad2

#PLOTEO Y TODO ME SALE ULTRA CHIDISIMO
ggplot2::autoplot(cbind(moeuro,mopound2,mocad3), main ="EX Rates", ylab = "Exchange Rate")

#ARREGLO TODO BONITO PARA HACER LO DEMÁS
xeuro <- moeuro
xpound <- mopound2
xcad <- mocad3

# Tasas de Crecimiento
teuro <- diff(log(xeuro))
tpound <- diff(log(xpound))
tcad <- diff(log(xcad))

ts_plot(teuro,
        title = "Tasa de Crecimiento: EURO",
        Ytitle = "%",
        Xtitle = "AÃ±o")
ts_plot(tpound,
        title = "Tasa de Crecimiento: BRITISH POUND",
        Ytitle = "%",
        Xtitle = "AÃ±o")
ts_plot(tcad,
        title = "Tasa de Crecimiento: CAD",
        Ytitle = "%",
        Xtitle = "AÃ±o")

#ExploraciÃ³n
# Comprobar estacionariedad
ggplot2::autoplot(cbind(teuro,tpound,tcad), main ="Tasas de Crecimiento", ylab = "%")

# ----------------------------------------------------------------------------
#Hasta aquí todo chido, ahora a hacer pruebas para conocer el comportamiento
#----------------------------------------------------------------------------

# Primero hacemos un Augmented Dickey-Fuller TESTING
# si p-value > 0.05 entonces la serie de tiempo es:
# No estacionaria y tiene cierta parte independiente y sin variación constante en el tiempo
# No se rechaza la Ho (Hipótesis nula)
adf.test(teuro)  # p-value < 0.05 - es estacionaria
adf.test(tpound) # p-value < 0.05 - es estacionaria
adf.test(tcad) # p-value < 0.05 - es estacionaria

# Prueba Phillips-Perron H0: La serie no es estacionaria
pp.test(teuro)  # p-value < 0.05 - es estacionaria
pp.test(tpound) # p-value < 0.05 - es estacionaria
pp.test(tcad) # p-value < 0.05 - es estacionaria

# ---------------------------------------------------------------------------------------------
#OK, las pruebas nos dicen que son estacionarias, ahora a analizar Auto Correlation Functions
#----------------------------------------------------------------------------------------------

#The basic guideline for interpreting the ACF and PACF plots are as following:
#-1-Look for tail off pattern in either ACF or PACF.
#-2-If tail off at ACF ??? AR model ??? Cut off at PACF will provide order p for AR(p).
#-3-If tail off at PACF ??? MA model ??? Cut off at ACF will provide order q for MA(q).
#-4-Tail of at both ACF and PACF ??? ARMA model

# ANALISIS ACF y PACF - Auto Correlation Functions y Partial Correlation Function
ts_cor(teuro) #Como no se ve un tail en ninguna usamos ARMA MA(1) y RA(1) 
ts_cor(tpound) #Aunque se ve un pequeño tail en ACF aún podemos usar ARMA MA(1) y RA(1) 
ts_cor(tcad) #Como no se ve un tail en ninguna usamos ARMA MA(1) y RA(1)

#------------------------------------------------------------------------------------
#Con lo anterior ya sabemos que podemos usar un ARMA / ARIMA para el forecasteo
#Por 2 cosas ACF y PACF no parecen tener un tail muy claro en las 3 series de tiempo
#-------------------------------------------------------------------------------------


# Pruebas de cointegración Phillips-Oularis
# P-value < 5%, es decir, Ho se rechaza y hay poca cointegración entre las series de tiempo
tseries::po.test(cbind(teuro, tpound, tcad), demean = TRUE, lshort = TRUE)
#---------------------------------------------------------------------------------
#Ahora hacemos un VECTOR AUTOREGRESSIVE MODEL - VAR
#---------------------------------------------------------------------------------
#Obtenermos un AIC HQ SC FPE de (1) nuestro modelo es parsimonious, es decir buen fit
tvar <- cbind(teuro,tpound,tcad)
VARselect(tvar, lag.max = 5, type="const")
mod <- VAR(tvar, type="const", lag.max=5, ic="SC")
mod
summary(mod) #Correlación euro-libra 0.63 euro-cad 0.42 y libra-cad 0.42 
roots(mod)
plot(mod)
acf(residuals(mod))
#No hay vectores cointegrados se rechaza Ho donde dice que hay al menos 1 o 2 cointegrados
johansen<-ca.jo(tvar, type="eigen",spec="transitory",ecdet="none",K=2)
summary(johansen)
