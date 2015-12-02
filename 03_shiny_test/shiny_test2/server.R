library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(reshape2)
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

## GGplot correlogram
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

CorrMat <- cor(resid)
CorrMat %<>% #reorder_cormat() %>% 
  round(2) %>% get_upper_tri() %>% melt(na.rm = TRUE)

#ggtime <- gather(time_series, key = "Time Series", value = "value", starts_with("ts"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  vals <- reactiveValues(
    CorrMatClick = CorrMat %>% filter(FALSE)
  )
  
  output$CorrPlot <- renderPlot({
    # ggplot2 : Quick correlation matrix heatmap
    # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
    dd <- vals$CorrMatClick
    ggplot(CorrMat, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      geom_tile(data = dd, color = "green", size = 2) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Коэффициент\nкорреляции") +
      theme_bw() + ggtitle("Корреляция отклонений от базовой линии") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
      # Определение координат по клику в Shiny не работает с coord_fixed!!! (неверные координаты)
      # coord_fixed() + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))
  })
  
  observeEvent(input$corr_click, {
    vals$CorrMatClick <- nearPoints(CorrMat, input$corr_click, threshold = 500, maxpoints = 1)
  })
  
  output$text1 <- renderPrint({ 
    # paste("Click Info", round(input$corr_click$x), round(input$corr_click$y))
    vals$CorrMatClick
    str_extract(vals$CorrMatClick$Var1, "ts\\d")
  })
  
  output$ChoiceText <- renderPrint({
    if (length(vals$CorrMatClick$Var1)==0){
      cat(paste("Коэффициент корреляции не выбран"))
    } else {
      cat("Выбран коэффициент корреляции между рядами",
            vals$CorrMatClick$Var1, "и", vals$CorrMatClick$Var2)
    }
  })
  
  output$TS1Plot <- renderPlot({
    ts_choice <- str_extract(vals$CorrMatClick$Var1, "ts\\d")
    if (length(ts_choice)==0) return(NULL)
    data <- time_series %>% select(timestamp, starts_with(ts_choice))
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(!str_detect(type, "resid"))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle(paste("Временной ряд", ts_choice, "с базовой линией")) + theme_bw()
  })
  
  output$TS2Plot <- renderPlot({
    ts_choice <- str_extract(vals$CorrMatClick$Var2, "ts\\d")
    if (length(ts_choice)==0) return(NULL)
    data <- time_series %>% select(timestamp, starts_with(ts_choice))
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(!str_detect(type, "resid"))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle(paste("Временной ряд", ts_choice, "с базовой линией")) + theme_bw()
  })

  output$R1Plot <- renderPlot({
    ts_choice <- str_extract(vals$CorrMatClick$Var1, "ts\\d")
    if (length(ts_choice)==0) return(NULL)
    data <- time_series %>% select(timestamp, starts_with(ts_choice))
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(str_detect(type, "resid"))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle(paste("График отклонений ряда", ts_choice, "от базовой линии")) + theme_bw()
  })
  
  output$R2Plot <- renderPlot({
    ts_choice <- str_extract(vals$CorrMatClick$Var2, "ts\\d")
    if (length(ts_choice)==0) return(NULL)
    data <- time_series %>% select(timestamp, starts_with(ts_choice))
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(str_detect(type, "resid"))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle(paste("График отклонений ряда", ts_choice, "от базовой линии")) + theme_bw()
  })
})