library(dplyr)
library(magrittr)
library(forecast)
library(lubridate)
library(ggplot2)

# Forecasting within limits http://robjhyndman.com/hyndsight/forecasting-within-limits/
LogTransform <- function(y, a = 0, b = 100){
  result <- log((y-a) / (b-y))
  return(result)
}

BackTransform <- function(y, a = 0, b = 100){
  result <- (b-a) * exp(y) / (1+exp(y)) + a
  return(result)
}

BackForecastTransform <- function(fc, a = 0, b = 100){
  fc$mean <- (b-a) * exp(fc$mean) / (1+exp(fc$mean)) + a
  fc$lower <- (b-a) * exp(fc$lower) / (1+exp(fc$lower)) + a
  fc$upper <- (b-a) * exp(fc$upper) / (1+exp(fc$upper)) + a
  fc$x <- (b-a) * exp(fc$x) / (1+exp(fc$x)) + a
  fc$fitted <- (b-a) * exp(fc$fitted) / (1+exp(fc$fitted)) + a
  return(fc)
}

m <- 672
subdata_ts <- ts(subdata$value, frequency = m)

# Arima-Fourier дл€ исходного р€да
# http://robjhyndman.com/hyndsight/longseasonality/
FourierArimaFc <- function(y, h=2*m){
  y <- ts(y, frequency = m)
  ## auto.arima работает долго, подбирает (2,0,1)
  # fit <- auto.arima(y, seasonal=FALSE, xreg=fourier(y, K=10))
  fit <- Arima(y, order=c(2,0,1), xreg=fourier(y, K=14))
  fc <- forecast(fit, h=2*m, xreg=fourierf(y, K=14, h=2*m))
  #plot(fc)
  #sd_error <- sd(fc$fitted - y)
  return(fc)
}


# Arima-Fourier дл€ преобразованного р€да (0-100)
FourierArimaLimFc <- function(y, h=2*m){
  y_log <- ts(LogTransform(y), frequency = m)
  fit <- Arima(y_log, order=c(2,0,1), xreg=fourier(y_log, K=14))
  fc <- forecast(fit, h=2*m, xreg=fourierf(y_log, K=14, h))
  fc <- BackForecastTransform(fc)
  #plot(fc)
  #sd_error <- sd(fc$fitted - y)
  #hist(fc$fitted - y, breaks=50)
}

# модель TBATS
TBatsModel <- function(y, h=2*m){
  y <- LogTransform(y)
  y <- msts(y, seasonal.periods=c(96,672))
  fit.tbats <- tbats(y)
  fc <- forecast(fit.tbats, h=h)
  fc <- BackForecastTransform(fc)
  return(fc)
}

# STL модель (сезонна€ декомпозици€)
STLModel <- function(y, h=2*m){
  y <- LogTransform(y)
  y <- ts(y, frequency = m)
  fit.stl <- stlm(y, method = "ets")
  fc <- forecast(fit.stl, h=h)
  fc <- BackForecastTransform(fc)
  return(fc)
}

TestSample <- function(y, method = STLModel, testsize = 672){
  trainsize <- length(y) - testsize
  fc <- method(y[1:trainsize], h = testsize)
  
  plot(fc)
  lines(y)
  sd_error <- sd(fc$mean - tail(y, testsize))
  print(paste("Testsample SD Error:", sd_error))
}

# fc <- STLModel(subdata_ts)
# plot(fc)
# hist(fc$fitted - subdata_ts, breaks=50)

TestSample(subdata_ts, method=STLModel)


#### Ќебольшой тест регрессии с LogTransform
if(FALSE){
  subdata2=subdata
  subdata2$nwday=as.factor(subdata2$nwday)
  subdata2$hgroup=as.factor(subdata2$hgroup)
  
  subdata2$week=week(subdata2$timestamp)
  subdata2$logvalue <- LogTransform(subdata2$value)
  
  testsize <- 672
  trainsize <- nrow(subdata2) - testsize
  
  model_test <- lm(logvalue ~ nwday*week + hgroup*week,
                   data = subdata2[(1:trainsize),])
  pr <- predict(model_test, newdata = subdata2)
  pr <- BackTransform(pr)
  
  base <- head(pr, trainsize)
  base_pr <- tail(pr, testsize)
  timest <- head(subdata2$timestamp, trainsize)
  timest_pr <- tail(subdata2$timestamp, testsize)
  value_pr <- tail(subdata2$value, testsize)
  
  SS <- sum((value_pr - base_pr)^2)
  SM <- sum(abs(value_pr - base_pr))
  
  df1<- data_frame(value = base, group = "baseline",
                   timestamp = timest)
  df2<- data_frame(value = base_pr, group = "predict",
                   timestamp = timest_pr)
  df3<- data_frame(value = subdata2$value, group = "value",
                   timestamp=subdata2$timestamp)
  df <- bind_rows(df1, df2, df3)
  p<-ggplot(df, aes(x=timestamp, y=value, group=group, colour=group)) + geom_line()
  p
}