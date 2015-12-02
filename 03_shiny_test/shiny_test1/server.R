library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(corrplot)
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
  ts9 = GenerateTimeSeries(base = cos(x)+sqrt(x), resid=rnorm(1000), func = function(x) abs(x))
)

Smoother <- function(y){
  return(lowess(y, f=0.1)$y)
}

time_series %<>% mutate_each(
  funs(baseline = Smoother, resid = Smoother(.) - (.)), -timestamp)

resid <- time_series %>% select(contains("resid"))

CorrMat <- cor(resid)

#ggtime <- gather(time_series, key = "Time Series", value = "value", starts_with("ts"))

## GGplot correlogram
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$CorrPlot <- renderPlot({
    # http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(CorrMat, method="color", col=col(200),  
             type="upper", order="hclust", 
             addCoef.col = "black", # Add coefficient of correlation
             tl.col="black", tl.srt=45, #Text label color and rotation
             # Combine with significance
             #p.mat = p.mat,
             sig.level = 0.01, insig = "blank", 
             # hide correlation coefficient on the principal diagonal
             diag=FALSE
    )
  })
  
  output$text1 <- renderText({ 
    paste("Click Info", input$corr_click$x, input$corr_click$y)
  })
  
  output$TSPlot <- renderPlot({
    input_choice <- input$InputName
    data <- time_series %>% select(timestamp, starts_with(input_choice))
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(!str_detect(type, "resid"))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle("Временной ряд с базовой линией")
  })
  
  output$RPlot <- renderPlot({
    input_choice <- input$InputName
    data <- time_series %>% select(timestamp, starts_with(input_choice)) 
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(str_detect(type, "resid"))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle("Остатки")
  })
})