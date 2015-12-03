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

lagged_series <- data_frame(
  
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
  cormat[lower.tri(cormat)] <- NA
  # diag(cormat) <- NA
  return(cormat)
}

CorrMat <- cor(resid)
CorrMat %<>% reorder_cormat() %>% 
  round(2) %>% get_upper_tri() %>% melt(na.rm = TRUE)

#ggtime <- gather(time_series, key = "Time Series", value = "value", starts_with("ts"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
    vals$ts_choice1 <- str_extract(vals$CorrMatClick$Var1, "ts\\d")
    vals$ts_choice2 <- str_extract(vals$CorrMatClick$Var2, "ts\\d")
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
  
  output$CCFPlot <- renderPlot({
    if (length(vals$ts_choice1)==0) return(NULL)
    ts1 <- time_series %>% select(get(vals$ts_choice1))
    ts2 <- time_series %>% select(get(vals$ts_choice2))
    crossCorr <- ccf(ts1, ts2, lag.max = 16, type = "correlation")
    # browser()
    cross_data <- data_frame(Lag = factor(crossCorr$lag[,1,1]), CCF = crossCorr$acf[,1,1])
    ggplot(cross_data, aes(x = Lag, y = CCF)) +
      geom_bar(stat="identity", position = "identity", fill = "red") +
      scale_y_continuous(limits=c(-1, 1)) +
      theme_bw() + xlab("Временной лаг") + ylab("Коэффициент кросс-корреляции") +
      ggtitle("График кросс-корреляции")
  })
  
  output$TSPlot <- renderPlot({
    if (length(vals$ts_choice1)==0) return(NULL)
    data <- time_series %>% select(timestamp, starts_with(vals$ts_choice1),
                                   starts_with(vals$ts_choice2))
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(!str_detect(type, "resid")) %>%
      mutate(tstype = str_extract(type, "ts\\d"))
    
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle(paste("Временной ряд", vals$ts_choice1, "с базовой линией")) + theme_bw() +
      facet_grid(tstype ~ .) + theme(legend.position = "bottom") +
      labs(color = "Временной ряд")
  })

  output$RPlot <- renderPlot({
    if (length(vals$ts_choice1)==0) return(NULL)
    data <- time_series %>% select(timestamp, starts_with(vals$ts_choice1),
                                   starts_with(vals$ts_choice2))
    ggtime <- gather(data, key = "type", value = "value", starts_with("ts")) %>%
      filter(str_detect(type, "resid"))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle(paste("График отклонений выбранных рядов от базовой линии")) + theme_bw() +
      theme(legend.position = "bottom") + labs(color = "Временной ряд")
  })
})