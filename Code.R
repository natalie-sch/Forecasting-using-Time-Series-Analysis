## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# update.packages(ask = FALSE, checkBuilt = TRUE)
# tinytex::tlmgr_update()



## ----results = 'hide'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls(all = TRUE)) #clear everything
Sys.setenv(LANG = "en") #system language English

# install and load packages
#install.packages("ggplot2")
#install.packages("tidyquant") #to download data from yahoo finance
#install.packages("tseries")
#install.packages("urca")
#install.packages("ggfortify")  #to interpret ts objects in ggplot
#install.packages("quantmod")
#install.packages("gridExtra")
#install.packages("forecast")

#load packages
library(ggplot2)
library(tidyquant) 
library(tseries)
library(urca)
library(ggfortify)
library(quantmod)
library(gridExtra)
library(forecast)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#acquire monthly average stock prices
getSymbols("MSFT", from = "1987-08-01", to = "2021-07-31", src = 'yahoo', periodicity = 'daily')
output <- aggregate(MSFT$MSFT.Close, list(format(index(MSFT), "%Y-%m")), mean)
colnames(output) <- c('ClosingPrice')

#create time series for closing stock prices
price = ts(output$ClosingPrice, frequency=12, start=c(1987,08))
summary(price)



## ---- fig.show="hold", out.width="50%"---------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Plot time series and its autocorrelation
grid.arrange((autoplot(price)
+ ylab("")
+ ggtitle("Original Data")), #Microsoft stock prices
(ggAcf(price)
+ ylab("")
+ ggtitle("ACF")),
nrow = 1)

## ---- fig.show="hold", out.width="50%"---------------------------------------------------------------------------------------------------------------------------------------------------------------------

autoplot(decompose(price, type="multiplicative")) + 
xlab("Year") +
ggtitle("Classical multiplicative decomposition of Microsoft stock prices")


## ---- fig.show="hold", out.width="50%"---------------------------------------------------------------------------------------------------------------------------------------------------------------------

#transform data by taking the logs and differentiating them
l.price <- log(price)

#Plot time series and its autocorrelation
grid.arrange((autoplot(l.price)
+ ylab("")
+ ggtitle("Original Data")),
(ggAcf(l.price)
+ ylab("")
+ ggtitle("ACF")),
nrow = 1)


## ---- fig.show="hold", out.width="50%"---------------------------------------------------------------------------------------------------------------------------------------------------------------------

autoplot(decompose(l.price, type="multiplicative")) + 
xlab("Year") +
ggtitle("Classical multiplicative decomposition of US Employment level")



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# KPSS Unit Root Test
summary(ur.kpss(l.price, type = "tau"))

#Augmented Dickey-Fuller (ADF) Test Unit
summary(ur.df(l.price,selectlags="AIC", type="trend"))



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# KPSS Unit Root Test
summary(ur.kpss(l.price, type = "mu"))

#Augmented Dickey-Fuller (ADF) Test Unit
summary(ur.df(l.price,selectlags="AIC", type="drift"))



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

growth.price <- diff(l.price)
growth.price <- na.omit(growth.price) #remove NA value of the first observation

# KPSS Unit Root Test
summary(ur.kpss(growth.price, type = "mu"))

#Augmented Dickey-Fuller (ADF) Test Unit
summary(ur.df(growth.price,selectlags="AIC", type="drift"))
summary(ur.df(growth.price,selectlags="AIC", type="none"))


## ---- figures-side, fig.show="hold", out.width="50%"-------------------------------------------------------------------------------------------------------------------------------------------------------

#Look at ACF and PACF. What model would you choose?

grid.arrange((ggAcf(growth.price)
+ ylab("")
+ ggtitle("ACF")),
(ggPacf(growth.price)
+ ylab("")
+ ggtitle("PACF")),
nrow = 1)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model.hand1 <- Arima(growth.price, order=c(1,1,1), seasonal=c(0,0,1))
summary(model.hand1)
checkresiduals(model.hand1) #Test if the residuals are white noise and normally distributed.

model.hand2 <- Arima(growth.price, order=c(0,1,1), seasonal=c(0,0,1))
summary(model.hand2)
checkresiduals(model.hand2)

model.hand3 = auto.arima(l.price,ic="aic", seasonal = FALSE, stepwise=FALSE, approximation=FALSE)
summary(model.hand3)
checkresiduals(model.hand3)

model.hand4 = auto.arima(l.price,ic="aic", seasonal = TRUE, stepwise=FALSE, approximation=FALSE)
summary(model.hand4)
checkresiduals(model.hand4)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model.hand5= ets(l.price, model="AAA")
summary(model.hand5)
checkresiduals(model.hand5)

model.hand6= ets(l.price, model="ZZZ")
summary(model.hand6)
checkresiduals(model.hand6)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Split the data into two parts
train.price <- window(l.price, end=c(2015,12))
test.price <- window(l.price, start=2016)

aut.arima <- Arima(train.price, order = c(0,1,1), include.drift = TRUE)
auto.ets <- ets(train.price, model="ZZZ")

h <- length(test.price)

for.autoar <- forecast(aut.arima, h=h)
for.autoets <- forecast(auto.ets, h=h)
grid.arrange((autoplot(for.autoar)
              + ylab("")
              + autolayer(test.price)
              + ggtitle("ARIMA(0,1,1) with drift")),
              (autoplot(for.autoets)
              + autolayer(test.price)
              + ylab("")
              + ggtitle("ETS(A,A,N)")),
              nrow = 1)


#Compare the accuracy of your forecasts against the actual values
accuracy(for.autoets, test.price)
accuracy(for.autoar, test.price)


