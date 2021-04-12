# Time series using Tomorrowmakers organic dataset
library(ggplot2)
library(forecast)
library(colortools)
library(readxl)
TM <- read_excel("TM_organic traffic_Jan 2016-Feb 2021.xlsx", 
                 col_types = c("date", "numeric"))
View(TM)

# convert to time series vector
tsv <- ts(TM$Users,start=c(2016,1),frequency = 12)
options(scipen = 999)
format(tsv,big.mark = ",")

# Plots
plot(tsv,xlab="Year",ylab="users",main="Monthly organic users")
lines(lowess(tsv),col="red")

plot(aggregate(tsv,FUN=mean),main="Avg.monthly Organic users per year",
     xlab="Year",ylab="users")
grid()
boxplot(tsv~cycle(tsv))

# seasonal plots

forecast::ggseasonplot(tsv, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("user") +
  ggtitle("Seasonal plot: Organic traffic")

ggsubseriesplot(tsv) +
  ylab("users") +
  ggtitle("Seasonal subseries plot: organic traffic")


plot(decompose(tsv))

# Augmented Dicky fuller test
tseries::adf.test(diff(log(tsv)), alternative="stationary", k=0)

# ACF
acf(log(tsv))
acf(diff(log(tsv)))
pacf(diff(log(tsv)))

# ARIMA model

fit <- arima(log(tsv), c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1),period = 12))
pred <- predict(fit, n.ahead = 5*12)
ts.plot(tsv,2.718^pred$pred, log = "y", lty = c(1,3))
