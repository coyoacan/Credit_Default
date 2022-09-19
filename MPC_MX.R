# WORKING DIRECTORY AND LIBRARIES----
# First and foremost, we tell RStudio where is our Working Directory
# We made sure all CSV files were included there before starting
# Be aware your Directory may not be the same
setwd('C://Users//andre//Downloads')

# The easiest way to get ggplot2 is to install the whole tidyverse:
install.packages("tidyverse")

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

# MARGINAL PROPENSITY TO CONSUME ----

# Data is already in tabular form, balanced, almost pristine 
mx <- read.csv("Demand_and_Total_Income.csv", header = TRUE, sep = ",")
# look at the data frame, notice PERIODO is a column of characters
head(mx)
# To change this, we call magrittr library and apply lubridate to PERIODO
mx %<>% 
  mutate(PERIODO = lubridate::dmy(PERIODO))
mx # take a look

# Lets plot some stuff, for this we first arrange data as follows
# PERIOD variable and then value, 3 columns
df <- mx %>%
  select(PERIODO,DEMANDA,SALARIOS,VAGREGADO) %>%
  gather(key = "variable", value = "value", -PERIODO)
head(df, 3)

# We plot the previous dataframe to get a taste of how 
# our variable behaves, notice Demand almost equals Agg Value

P <- ggplot(df, aes(x = PERIODO, y = value)) +
    geom_line(aes(color = variable), size = 1) +
    scale_color_manual(values=c("#00AFBB", "#E7B800","#ce4dff"))+
    geom_point() +
    ggtitle("Evolución por año de la Demanda Comercial, Salarios e Ingreso Disponible") +
    theme_minimal() +
    ylab("Valores en millones de MXN")

# We call the plot
P

# MPC= (Consume 1- Consume 0) / (Income 1 - Income 0)
c = diff(mx$DEMANDA)
i = diff(mx$VAGREGADO)
MPC = c/i
# MPC time series
MPCts <-  ts(MPC, start=2004, end=2019, frequency=1)
# take a look, there is a huge spike in 2013
hchart(MPCts)
# Create Data Frame with years and mpc
years <- c(2004:2019)
mpc <- c(MPC)
df <- data.frame(years, mpc)
df
#---stationarity: statistical properties of time series do not change over time.
#--- Phillips-Perron H0: Series is not stationary 
#-- p-value > 0.05 no enough evidence to reject the Null hypothesis
#-- meaning that the time series of MPC is no-stationary.
pp.test(MPCts)
#Its obvious these is non-stationary and we have a freaking outlier
#Specifically 2013 Enrique Peña Nieto as a new president of Mexico (early days)
#financial reform in early years
boxplot(df$mpc)

# STATIONARITY TESTS----

df$mpc[10] = mean(df$mpc) #Impute outlier with mean in df
MPCts[10] = mean(MPCts) #Impute outlier with mean in ts

#Lets plot what we have with our MPC calculation
P <- ggplot(df, aes(x = years, y = mpc)) +
  geom_line(size = 1) +
  geom_point() +
  ggtitle("Maginal Propensity to Consume by Year") +
  theme_minimal() +
  ylab("Change in Consumption / Change on Income")

# Check plots
P
hchart(MPCts)
# run phillips-perron test again
# this time data is stationary
pp.test(MPCts)
# run Augmented Dickey-Fuller Test
# This drops a -2.5 value < 5% ergo, this is stationary
# Unit root is a characteristic of a time series that makes it non-stationary
# Technically speaking, a unit root is said to exist in a time series of the value of alpha = 1
# Null Hypothesis (H0): alpha=1 this is the case when P>5%
adf.test(MPCts) 

# TIME FOR ARIMA ----

# Have a look on how would a linear regression may look like
scatter.smooth(df$years, df$mpc, main='MPC evolution in time')
#maybe not hehe, it seems like a job for ARIMA

#If we are to call Lord ARIMA for help we must understand his terms of engagement
#Autocorrelation is the correlation between two observations at different points in a time series.

#Autocorrelation Function for MPC time series
#Lags are really close to zero, implying noise (hopefully they are not but close)
#Stationarity since lags decline rapidly
#No signs of Trends, boundaries do not grow as lags decrease
#No signs of seasonality, lags alternate in non visible patterns here
#Unfortunately no statistically significant lags
acf(MPCts, lag.max=20)
# Partial Autocorrelation Model
# Everything is within boundaries, no stat. significant lags
# Maybe suggesting AR model in laga 2 or 3 or 8
pacf(MPCts, lag.max=20)

#Lest do Auto ARIMA, this works most of the time
#1. The Data series as input should be stationary.
#2. As ARIMA takes past values to predict the future output, the input data must be invariant.
modelo<-auto.arima(MPCts)
#look at the summary
summary(modelo)
# p= 0 number of autoregressive terms
# d= 1 is times the lagged indicators have been subtracted to make the data stationary.
# q = 0 number of forecast errors in the model
# ARIMA(0, 1, 0) - known as the random walk model

#TEST Ljung-Box
#H0: The residuals are independently distributed.
#HA: The residuals are not independently distributed; they exhibit serial correlation.
#Ideally, we would like to fail to reject the null hypothesis
#That is, we would like to see the p-value of the test be greater than 0.05 
#because this means the residuals for our time series model are independent, 
#which is often an assumption we make when creating a model.

tsdiag(modelo)
Box.test(residuals(modelo), type = "Ljung-Box")
#Unfortunately P-value of 3% < 5% residuals are dependent :( search is on

# Not to throw everything away, we do a forecasting exercise
# We end up with 0.87 MPC for the next 3 years
pronostico <- forecast(modelo,h=3)
pronostico
plot(pronostico, ylab="MPC", xlim=c(2004,2022))

#Sorry folks maybe GARCH will be up to par
#Lets clean this mess right now
rm(pronostico, P,modelo,df,c,i,mpc,MPC,MPCts,years)

# WILL GARCH BE THE ANSWER?------

#Assumption autoregressive moving average (ARMA) model is assumed for the error variance

# MPC= (Consume 1- Consume 0) / (Income 1 - Income 0)
c = diff(mx$DEMANDA)
i = diff(mx$VAGREGADO)
MPC = c/i
# MPC time series
MPCts <-  ts(MPC, start=2004, end=2019, frequency=1)
# take a look, there is a huge spike in 2013
hchart(MPCts)
# Create Data Frame with years and mpc
years <- c(2004:2019)
mpc <- c(MPC)
df <- data.frame(years, mpc)
df

#Again, we impute the outlier with the mean, because... we want to
df$mpc[10] = mean(df$mpc) #Impute outlier with mean in df
MPCts[10] = mean(MPCts) #Impute outlier with mean in ts

#Notice we are just placing the AutoRegressive part of GARCH here
specs = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,0)),
                   mean.model=list(armaOrder=c(0,0)), 
                   distribution.model="norm")

modelo <-ugarchfit(data=MPCts, spec=specs, out.sample = 3)

modelo

plot(modelo, which = 9)


predictor = ugarchforecast(modelo, n.ahead=3)
predictor
plot(predictor, ylab="consumer confidence", xlim=c(2004,2022), which=1)



# IS QUANTITY THE ANSWER? -------------------------------------------------

#lets clear the desk once again
rm(df,modelo,mx,predictor,specs,c,i,mpc,MPC,MPCts,years)

# Data is already in tabular form, balanced, almost pristine 
mx <- read.csv("IMCPMI.csv", header = TRUE, sep = ",")
# To change this, we call magrittr library and apply lubridate to PERIODO
mx %<>% 
  mutate(PERIODO = lubridate::dmy(PERIODO))
mx # take a look

# Lets plot some stuff,
df <- mx %>%
  select(PERIODO,CONSUMO) %>%
  gather(key = "variable", value = "value", -PERIODO)
head(df, 3)

# We plot the previous dataframe to get a taste of how 


P <- ggplot(df, aes(x = PERIODO, y = value)) +
  geom_line(aes(color = variable), size = 2) +
  scale_color_manual(values=c("#00AFBB", "#E7B800"))+
  ggtitle("Consumo Privado en el Mercado Interior ") +
  theme_minimal() +
  ylab("Indice base 2013")

# We call the plot
P

consumo <-  ts(mx$CONSUMO, start=c(1993,1,1), frequency=12)

ddata <- decompose(consumo, "multiplicative")
plot(ddata)

# take a look, there are spikes everywhere!!!
hchart(consumo)



pp.test(consumo) #Voila! Stationarity thanks to many more observations!
adf.test(consumo) #Voila! again stationarity 
#Nevertheless we still see outliers, but at least a normal distibution centered in 0
boxplot(consumo) #quite normal

#---- STATIONARITY CHECK, NOW LETS GET AMBITIOUS!!!


acf(consumo, lag.max=20)
pacf(consumo, lag.max=20)

#Lest do Auto ARIMA, this works most of the time
#1. The Data series as input should be stationary.
#2. As ARIMA takes past values to predict the future output, the input data must be invariant.
modelo<-auto.arima(consumo)
#look at the summary
summary(modelo)
# p= 1 number of auto regressive terms
# d= 1 is times the lagged indicators have been subtracted to make the data stationary.
# q = 1 number of forecast errors in the model
# ARIMA(1, 1, 1) - AR, MA and I values
# sigma^2 is 6.4 and ME is 0.269 the model fits... OK, we guess.

tsdiag(modelo)
Box.test(residuals(modelo), type = "Ljung-Box")
#Fortunately P-value of 36% > 5% residuals are independent!!!!

#Forecast
pronostico <- forecast(modelo,h=25)
pronostico
plot(pronostico, ylab="consumer confidence", xlim=c(1993,2024))
#Looks neat

multi_forecast <- modelo %>%
  forecast(h = 25)

multi_forecast %>%
  autoplot()

print("the_end")
             