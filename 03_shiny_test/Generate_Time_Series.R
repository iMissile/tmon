library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(shiny)
library(stringr)

GenerateTimeSeries <- function(base, resid, func){
  TransForm <- function(x, transform, a = 0, b = 100){
    # x <- log((x-a) / (b-x))
    x <- transform(x)
    x <- x + resid/5
    x <- (b-a) * exp(x) / (1+exp(x)) + a
  }
  base <- TransForm(base, transform = func)
  #result <- base + resid
  return(base)
}

x <- seq(0, 20*pi, length.out = 1000)
timestamp <- seq(ymd("2015-12-01"), by = "1 hour", length.out = 1000)

time_series <- data_frame(
  timestamp = timestamp,
  ts1 = GenerateTimeSeries(base = -cos(x), resid=rnorm(1000), func = function(x) x^2),
  ts2 = GenerateTimeSeries(base = (-cos(x+2))^2, resid=rnorm(1000), func = function(x) log(x)),
  ts3 = GenerateTimeSeries(base = abs(tan(x)), resid=rexp(1000), func = function(x) sqrt(x)),
  ts4 = GenerateTimeSeries(base = sin(0.25*x), resid=rnorm(1000), func = function(x) x^3),
  ts5 = GenerateTimeSeries(base = cos(0.1*x-3), resid=sin(x) + rnorm(1000), func = function(x) sin(x)),
  ts6 = GenerateTimeSeries(base = sin(2*x-4), resid=rnorm(1000), func = function(x) x),
  ts7 = GenerateTimeSeries(base = abs(-cos(x)), resid=rexp(1000), func = function(x) log(x)),
  ts8 = GenerateTimeSeries(base = cos(0.3*x)^2, resid=sin(x) +rnorm(1000), func = function(x) sqrt(x+3)),
  ts9 = GenerateTimeSeries(base = cos(sin(x)), resid=rnorm(1000), func = function(x) exp(x)),
  ts10 = GenerateTimeSeries(base = cos(x)+sqrt(x), resid=rnorm(1000), func = function(x) abs(x))
)

Smoother <- function(y){
  return(lowess(y, f=1/3)$y)
}

baselines <- time_series %>% mutate_each(funs(Smoother), -timestamp)


ggtime <- gather(time_series, key = "type", value = "value", starts_with("ts"))
ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line()
