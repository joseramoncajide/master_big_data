##########################################################################
# Jose Cajide - @jrcajide
# Master Big Data: Time series manipulation, analysis and forecasting
##########################################################################

rm(list=ls()) 
cat("\014")

list.of.packages <- c("tibbletime", "tidyverse", "tidyquant", "zoo", "ggmap", "forecast", "smooth", "Metrics", "timetk", "sweep", "ggseas")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tibbletime)
library(tidyverse)
library(tidyquant)
library(zoo)
library(ggmap)
library(forecast)
library(smooth)
library(Metrics)
library(timetk)
library(sweep)
library(ggseas)

ipi.df <- read_csv('data/ipi.csv')

#----------------------------------------------------------------------------
# Exercise 1. Transform ipi.df from:
# 
# date      ipi
# <chr>   <dbl>
# 2017M11 104 
# to:
# 
# date         ipi
# <date>     <dbl>
# 2017-11-01 104  
#----------------------------------------------------------------------------

# Sol: 
ipi.df %<>% 
  separate(date, into = c("year", "month"), sep = "M") %>% 
  mutate(date = as.Date(paste(year, month, 01, sep = "-"))) %>% 
  select(date, ipi)

#----------------------------------------------------------------------------
# Exercise 2. Plot the data as a time series using ggplot2
#----------------------------------------------------------------------------

ipi.df %>%
  ggplot(aes(x = date, y = ipi)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_smooth(method = "loess") +
  labs(title = "Indice De Produccion Industrial De Cantabria", x = "", y = "IPI") +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()

#----------------------------------------------------------------------------
# Data as a vector
#----------------------------------------------------------------------------

(ipi <- ipi.df$ipi)
(ipi <- ipi.df %>% 
    pull(ipi))

#----------------------------------------------------------------------------
# Time series witb base R
#----------------------------------------------------------------------------

(ipi.ts <- ts(data = ipi,frequency = 12, start = c(2002, 1)))
start(ipi.ts)
end(ipi.ts)
frequency(ipi.ts)

summary(ipi.ts)
head(ipi.ts)
max(ipi.ts)

plot(ipi.ts, main =  "IPI")
autoplot(ipi.ts) + labs(title = "IPI")


stats::lag(ipi.ts, 1)

plot(ipi.ts,stats::lag(ipi.ts),xlab="IPI(t)",ylab="IPI(t+1)")
lag.plot(ipi.ts)
lag.plot(ipi.ts,lags=4,layout=c(2,2))

# Diferenciación
(ipi_diff.ts <- diff(ipi.ts,1))
plot(ipi_diff.ts, main="IPI")

(ipi_diff.ts <- diff(ipi.ts,12))
plot(ipi_diff.ts, main="IPI con una diferencia estacional")




#----------------------------------------------------------------------------
# Modeling Time Series
#----------------------------------------------------------------------------

ipi.ts
class(ipi.ts)

# Average method
ipi.mean <- meanf(ipi.ts, h = 12)
summary(ipi.mean)
plot(ipi.mean)


# Naive
ipi.naive <- naive(ipi.ts, 12)
summary(ipi.naive)
plot(ipi.naive)

# Fitted Values and Residuals
# It is important to always check that the residuals are well-behaved
# Essential assumptions for an appropriate forecasting model include residuals being uncorrelated and centered at mean zero
checkresiduals(ipi.naive) # p<0.05 => Residuals are not white noise

# Evaluating Forecast Accuracy
# https://www.otexts.org/fpp/2/5

accuracy(ipi.naive)

# Training & Test Sets
(n <- length(ipi.ts))
n_test <- 12
(n_train <- n - n_test)

(ipi_train <- ts(data = ipi.ts[1:n_train],frequency = 12, start = c(2002, 1)) )
(ipi_test <- ts(data = ipi.ts[(n_train+1):n],frequency = 12, end = c(2017, 11)) )

autoplot(ipi_train) 
autoplot(ipi_test) 

ipi.naive_fc <- naive(ipi_train, h = length(ipi_test))
ipi.mean_fc <- meanf(ipi_train, h = length(ipi_test))
ipi.snaive_fc <- snaive(ipi_train, h = length(ipi_test))
forecast::accuracy(ipi.mean_fc, ipi_test)
forecast::accuracy(ipi.naive_fc, ipi_test)
forecast::accuracy(ipi.snaive_fc, ipi_test)

autoplot(ipi_train) +
  autolayer(ipi.mean_fc$mean, series="Mean") +
  autolayer(ipi.naive_fc$mean, series="Naïve") +
  autolayer(ipi.snaive_fc$mean, series="Seasonal naïve") +
  ggtitle("IPI Forecasts") +
  xlab("Year") + ylab("IPI") +
  guides(colour=guide_legend(title="Forecast"))



# Simple Moving Average

# Predict IPI would be for next 12 months based on the the last 12 months
# Order 2
fit2<-sma(ipi.ts, order = 2, h = 12, holdout = T, level = .95)
round(fit2$forecast,0)
plot(forecast(fit2))
round(mae(fit2$holdout, fit2$forecast),2)

# Automatic
fitX<-sma(ipi.ts, h = 12, holdout = T, level = .95, ic = 'AIC')
plot(fitX)
round(fitX$forecast,0)
plot(forecast(fitX))
round(mae(fitX$holdout, fitX$forecast),2)



# ts components -----------------------------------------------------------
# the trend is the long-term increase or decrease in the data
# the seasonal pattern occurs when a time series is affected by seasonal factors such as the time of the year or the day of the week
# the cycle occurs when the data exhibit rises and falls that are not of a fixed period
#----------------------------------------------------------------------------

ipi.decomp <- decompose(ipi.ts)
names(ipi.decomp)
plot(ipi.decomp)

ggseasonplot(ipi.ts)

ggseasonplot(ipi.ts , polar = TRUE)

ggsubseriesplot(ipi.ts)

# Autocorrelation -----------------------------------------------------------
acf(ipi.ts)
ggAcf(ipi.ts)
gglagplot(ipi.ts)



# Seasonally adjusted or deseasonalized data ------------------------------
# Obteining a seasonal stationary ts
ipi_des.ts  <- ipi.ts-ipi.decomp$seasonal
plot(ipi.ts,main="IPI. Serie normal Vs desestacionalizada")
lines(ipi_des.ts, col='red')


# Forecasting -------------------------------------------------------------

# Holt-Winters Forecasting or Triple Exponential Smoothing ----------------
# https://en.wikipedia.org/wiki/Exponential_smoothing#Triple_exponential_smoothing
# α is the data smoothing factor, 0 < α < 1, 
# β is the trend smoothing factor, 0 < β < 1, 
# and γ is the seasonal change smoothing factor, 0 < γ < 1.

# alpha = .3
fit_es1 <- HoltWinters(ipi_train,alpha = .3, beta = F, gamma = F)
plot(fit_es1)
fit_es1$SSE

fit_es <- HoltWinters(ipi_train, beta = F, gamma = F)
plot(fit_es)
fit_es$SSE
round(fit_es$alpha,4)


#----------------------------------------------------------------------------
# Exercise 4. Fit a Holt-Winters leaving all parameter in blank. 
# Get the proposed α, β and γ.
# Check the final sum of squared errors achieved in optimizing
#----------------------------------------------------------------------------
fit_hw <- HoltWinters(ipi_train)
round(fit_hw$alpha, 2)
round(fit_hw$beta, 2)
round(fit_hw$gamma, 2)
plot(fit_hw)
fit_hw$SSE


# Predictive performance of the model
pred_es <- predict(fit_es, n.ahead = 12, prediction.interval = T, level = .95)
round(head(pred_es),0)

pred_hw <- predict(fit_hw, n.ahead = 12, prediction.interval = T, level = .95)
round(head(pred_hw),0)

# Get RMSE on a test dataset: ipi_test

round(rmse(predicted = pred_es[, "fit"], actual  = ipi_test), 2)
round(rmse(predicted = pred_hw[, "fit"], actual  = ipi_test), 2)

plot(fit_es,predicted.values = pred_es)
plot(fit_hw,predicted.values = pred_hw)

# Forecasting out of sample: future prediction
fit_out_sample <- HoltWinters(ipi.ts)
forecast_out_sample <- forecast(fit_out_sample, h=12)
round(print(forecast_out_sample),2)
plot(forecast_out_sample)

# Chech prediction for Dec.2017:
# https://goo.gl/b3dGiB




# Identifying possible breakpoints in a time series -----------------------
require(strucchange)
breakpoints(ipi.ts ~ 1)
plot(ipi.ts, main="IPI breakpoints", ylab="IPI", col="darkblue", lwd=1.5)
# Plot the line at the optimal breakpoint
lines(breakpoints(ipi.ts ~ 1), col="darkgreen")
# Plot a 90% confidence interval
lines(confint(breakpoints(ipi.ts ~ 1), level=0.90), col="darkgreen")
# Add breakpoint location text
text(2009, 125, "April 2011", cex=0.75, col="darkgreen", pos=4, font=3)
text(2014, 125, "March 2014", cex=0.75, col="darkgreen", pos=4, font=3)

