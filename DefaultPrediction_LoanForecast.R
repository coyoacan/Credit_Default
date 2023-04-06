# Default Predictor Project
# Forecasting Next Year's Personal Loan Behaviour

# All theory was extracted from Google, I'm not a PhD, bro
# First and foremost, we tell RStudio where is our Working Directory
# We made sure all CSV files were included there before starting
# Be aware your Directory may not be the same in your PC
setwd('C://Users//andre//Downloads')

library(ggplot2) #-- nice charts
library(tseries) #-- Test Series
library(forecast) #--ARIMA
library(tidyverse) #--data manipulation, data visualization, and data science
library(gridExtra) #-- for arranging plots
library(dplyr) #-- ggplot
library(highcharter) #---more charts
library(tidyr) #--ggplot
library(magrittr) #-- mutate in place
library(reshape2) #-- reshape
library(lubridate) #--change dates
library(quantmod) #-- The quantmod package for R is designed to assist the quantitative trader 
library(rugarch) #--generalized autoregressive conditional heteroskedasticity (GARCH)
library(rmgarch) #--multivariate GARCH
library(xts) #--merge, cbind, rbind, c, Ops, lag, diff, coredata
library(PerformanceAnalytics) #--performance and risk analysis of financial instruments or portfolios
library(quantmod) #-- quantitative financial modeling framework

# Forecasting next year here

# Data is already in tabular form, almost pristine 
# Download loan behaviour from CNBV Comision Nacional Bancaria y de Valores
# Personal Loans balance from Scotiabank Mexico
mx <- read.csv("DefaultPrediction_personales.csv", header = TRUE, sep = ",")
# To change this, we call magrittr library and apply lubridate to MES
mx %<>% 
  mutate(MES = lubridate::dmy(MES))

# Look at the variables, tell me what they are
sapply(mx, class)
# Replace commas by nothing to have numbers AND then transform char to num
mx %<>%
  mutate(VIGENTE = str_replace(VIGENTE,"[:punct:]", ""))

mx %<>%
  mutate(VENCIDA = str_replace(VENCIDA,"[:punct:]", ""))

# Transform. From string to numeric.
mx$VIGENTE = as.numeric(mx$VIGENTE)
mx$VENCIDA = as.numeric(mx$VENCIDA)

# check types again
sapply(mx, class)

# Create a dataframe with variable & value
df <- mx %>%
  select(MES,VIGENTE,VENCIDA) %>%
  gather(key = "variable", value = "value", -MES)

# look at your new dataframe 
head(df,10)

# Make a chart to see how personal loans behaved in our balance sheet

p <- ggplot(df,
            aes(x = MES,
                y = value,
                col = variable)) +
  geom_line() 

p + ggtitle("Evolución del Balance de Préstamos Personales Scotiabank") +
  xlab("Periodo") + ylab("MXN Millones")

# Profitable Loans
# transform loan balances into time series AND get month to month change
# then, decompose the signal
cartera_vigente <- diff(ts(mx$VIGENTE, start=c(2016,8,1), frequency=12))
plot(decompose(cartera_vigente, "multiplicative"))

# Non Profit Loans
# transform loan balances into time series AND get month to month change
# then, decompose the signal
cartera_vencida <- diff(ts(mx$VENCIDA, start=c(2016,8,1), frequency=12))
plot(decompose(cartera_vencida, "multiplicative"))

# bind both time series and put them in a chart
xh <- cbind(cartera_vigente,cartera_vencida)
hchart(xh) %>%
  hc_title(text = "Cambios de cartera vigente y vencida Scotiabank m/m (MXN millones)")

# Sationarity Testing

# Atime series is said to be “stationary” if it has no trend, exhibits constant variance o
# ver time, and has a constant autocorrelation structure over time.

# One way to test whether a time series is stationary is to perform an augmented 
# Dickey-Fuller test, which uses the following null and alternative hypotheses:
# H0: The time series is non-stationary. In other words, it has some time-dependent structure and does not have constant variance over time.
# HA: The time series is stationary.

# A stationary process, another way to say something that generates a stationary time series, 
# has specific statistical properties enabling predicting a likely outcome.

# Seasonality refers to a series whose distribution changes predictably through time. 

# Non Profitable Loans
pp.test(cartera_vencida) #Voila! Stationarity thanks to many more observations! pv = 0.01
adf.test(cartera_vencida) #Voila! again stationarity pv=0.02 note 0.05 and up is non-stationary
#Nevertheless we still see outliers, but at least a normal distibution centered in 0
boxplot(cartera_vencida) #quite normal around zero

# Repeat for profitable loans
pp.test(cartera_vigente) #Voila! Stationarity thanks to many more observations! pv = 0.01
adf.test(cartera_vigente) # This detects non-stationarity pv = 0.27
#Nevertheless we still see outliers, but at least a normal distibution centered in 0
boxplot(cartera_vigente) #quite normal

#---- STATIONARITY CHECK, NOW LETS GET AMBITIOUS!!!

acf(cartera_vencida, lag.max=20) # correlations & geometric decay
pacf(cartera_vencida, lag.max=20)
acf(cartera_vigente, lag.max=20) # I fear: noise in here, what do you say?
pacf(cartera_vigente, lag.max=20)

# Lest do Auto ARIMA, this works most of the time
#1. The Data series as input should be stationary.
#2. As ARIMA takes past values to predict the future output, the input data must be invariant.
modelo_c_vencida <-auto.arima(cartera_vencida)
modelo_c_vigente <-auto.arima(cartera_vigente)
#look at the summary
summary(modelo_c_vencida)
summary(modelo_c_vigente)

# p = 1 number of auto regressive terms
# d = 1 is times the lagged indicators have been subtracted to make the data stationary.
# q = 1 number of forecast errors in the model
# ARIMA(1, 1, 1) - AR, MA and I values

# LJung Box
# The Ljung-Box test uses the following hypotheses:
# H0: The residuals are independently distributed.
# HA: The residuals are not independently distributed; they exhibit serial correlation.
# Ideally, we would like to fail to reject the null hypothesis. 
# That is, we would like to see the p-value of the test be greater than 0.05 because 
# this means the residuals for our time series model are independent, 
# which is often an assumption we make when creating a model


Box.test(residuals(modelo_c_vencida), type = "Ljung-Box")
#Fortunately P-value of 47% > 5% residuals are independently distributed!!!!
Box.test(residuals(modelo_c_vigente), type = "Ljung-Box")
#Fortunately P-value of 67% > 5% residuals are independently distributed!!!!

# The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess
# if a set of data plausibly came from some theoretical distribution such as a Normal 
# or exponential. We basically like those points to fall into a straight line.

qqnorm(modelo_c_vencida$residuals)
qqline(modelo_c_vencida$residuals)

qqnorm(modelo_c_vigente$residuals)
qqline(modelo_c_vigente$residuals)

#Forecast
pronostico_vencida <- forecast(modelo_c_vencida,h=11)
pronostico_vencida
plot(pronostico_vencida, ylab="consumer confidence", xlim=c(2016,2024))
#Looks neat

multi_forecast_vencida <- modelo_c_vencida %>%
  forecast(h = 11)

multi_forecast_vencida %>%
  autoplot()

#Forecast
pronostico_vigente <- forecast(modelo_c_vigente,h=11)
pronostico_vigente
plot(pronostico_vigente, ylab="consumer confidence", xlim=c(2016,2024))
#Looks neat

multi_forecast_vigente <- modelo_c_vigente %>%
  forecast(h = 11)

multi_forecast_vigente %>%
  autoplot()

print("the_end")
             
