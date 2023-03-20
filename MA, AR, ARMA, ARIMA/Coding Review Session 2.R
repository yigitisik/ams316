# AMS 316 Time Series Models 2022
# PACKAGES!

#remotes::install_github(repo = "r-lib/devtools",
#                        dependencies = TRUE,
#                        upgrade = TRUE)
#remotes::install_github(repo = "dgrtwo/broom", 
#                        dependencies = TRUE, upgrade = TRUE)
#remotes::install_github(repo = "cran/bstats",
#                        dependencies = TRUE,
#                        upgrade = TRUE, debug = TRUE)
#remotes::install_github(repo = "bdemeshev/sophisthse",
#                     dependencies = TRUE,
#                     upgrade = TRUE)

library("lubridate")  # data

library("zoo")  # time series
library("xts")  # more time series
library("dplyr")  # data manipulation
library("ggplot2")  # graphs
library("forecast")

library("quantmod")  # from finance.google.com
library("sophisthse")  # from sophist.hse.ru

# simulating process AR(1) y_t=0.7y_{t-1}+\e_t
y <- arima.sim(n = 100, list(ar = 0.7))
plot(y)  # graph
Acf(y)
Pacf(y)
tsdisplay(y)  # all 3 graphs at once

# simulating process MA(1) y_t=\e_t-0.8\e_{t-1}
y <- arima.sim(n = 100, list(ma = -0.8))
tsdisplay(y)

# simulating process ARMA(1,1) y_t=0.5y_{t-1}+\e_t-0.8\e_{t-1}
y <- arima.sim(n = 100, list(ma = -0.8, ar = 0.5))
tsdisplay(y)

# simulating process ARMA(1,1) y_t=-0.5y_{t-1}+\e_t-0.8\e_{t-1}
y <- arima.sim(n = 500, list(ma = -0.8, ar = -0.5))
tsdisplay(y)

# random walk y_t=y_{t-1}+\e_t
y <- arima.sim(n = 100, list(order = c(0, 1, 0)))
tsdisplay(y)

# same random walk with 500 observations
y <- arima.sim(n = 500, list(order = c(0, 1, 0)))
tsdisplay(y)

# adding a trend to  AR(1) process
y <- seq(0, 10, length = 100) + arima.sim(n = 100, list(ar = 0.7))
tsdisplay(y)

# adding a weaker trend to AR(1) process
y <- seq(0, 2, length = 100) + arima.sim(n = 100, list(ar = 0.7))
tsdisplay(y)

# Annual measurements of the level, in feet, of Lake Huron
y <- LakeHuron
tsdisplay(y)


# let's estimate AR(2)
mod_1 <- Arima(y, order = c(2, 0, 0))
# let's estimate ARMA(1,1)
mod_2 <- Arima(y, order = c(1, 0, 1))

# estimation results:
summary(mod_1)
summary(mod_2)

# AIC (lower = > better)
AIC(mod_1)
AIC(mod_2)

#  ARMA(2,1)
mod_3 <- Arima(y, order = c(2, 0, 1))
summary(mod_3)
AIC(mod_3)

# forecasting using model 2 for 5 steps ahead
pred <- forecast(mod_2, h = 5)
pred

# plotting our forecast
plot(pred)

# let's estimate ARIMA(1,1,0)
mod_4 <- Arima(y, order = c(1, 1, 0))
AIC(mod_4)

# forecasting using ARIMA(1,1,0)
pred_4 <- forecast(mod_4, h = 5)
plot(pred_4)

# automatic model selection using AIC
mod_a <- auto.arima(y, ic = "aic", stepwise = FALSE,
                approximation = FALSE    )
summary(mod_a)

# forecasting using automatic model selection
pred_a <- forecast(mod_a, h = 5)
plot(pred_a)


# Google stock price data
getSymbols(Symbols = "GOOG", from = "2014-01-01", to = "2014-12-01")


head(GOOG)  # data head
y <- GOOG$GOOG.Close  # closure price -> y

tsdisplay(y)  # plot, afc, pafc
dy <- diff(y)
tsdisplay(dy)  # three graphs for dy

# looks like a random walk, let's ARIMA(0,1,0)
mod_1 <- Arima(y, order = c(0, 1, 0))
summary(mod_1)

# forecasting for 20 steps ahead
pred_1 <- forecast(mod_1, h = 20)
pred_1

# plotting the forecast
plot(pred_1)

# automatic model selection
mod_a <- auto.arima(y, stepwise = FALSE, 
                    approximation = FALSE)
summary(mod_a)

pred_a <- forecast(mod_a, h = 20)
plot(pred_a)



