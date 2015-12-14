library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(reshape2)
library(stringr)
library(xts)
library(dygraphs)
library(TTR)
library(RColorBrewer)

source("dygraph_rus.R")

MaxLag <- 30
movWindow <- 100

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
  ts8 = GenerateTimeSeries(base = cos(0.3*x)^2, resid=sin(x) + rnorm(1000), func = function(x) sqrt(x+3)),
  ts9 = GenerateTimeSeries(base = cos(x)+sqrt(x), resid=rnorm(1000), func = function(x) abs(x))
)

Smoother <- function(y){
  return(lowess(y, f=0.1)$y)
}

baselines <- time_series %>% mutate_each(funs(Smoother), -timestamp)

resid <- select(baselines, -timestamp) - select(time_series, -timestamp)

OptimumLag <- function(ts1, ts2){
  crossCorr <- ccf(ts1, ts2, lag.max = MaxLag, plot = FALSE)
  ccorr <- crossCorr$acf[,,1]
  lag <- crossCorr$lag[,,1]
  # находим номер максимального по модулю коэффициента для положительного лага
  index <- which.max(ifelse(lag >= 0, abs(ccorr), 0))
  maxCorr <- ccorr[index]
  optLag <- lag[index]
  # рассчитываем порог как максимум из критического значения и 0.5
  critVal <- max(1.96 / sqrt(length(ts1)), 0.5)
  # Время отклика не может зависеть от будущих значений метрики
  if (optLag < 0) return(0)
  # Нет смысл применять лаг, если корреляция незначительна
  if (abs(maxCorr) < critVal) return(0)
  return(optLag)
}

# переменная с временем отклика
resp_time <- "ts1"
# переменные с метриками, от которых может зависеть время отклика
metrics <- paste0("ts", 2:9)
optLags <- c(ts1 = 0)

with(resid, {
  for (t in metrics){
    optLags[t] <<- OptimumLag(get(resp_time), get(t))
  }
})

lagged_resid <- resid
for (t in metrics){
  lagged_resid[t] <- lag(lagged_resid[[t]], n=optLags[t])
}

# time_series %<>% mutate_each(
#   funs(baseline = Smoother, resid = Smoother(.) - (.)), -timestamp)


#resid <- time_series %>% select(contains("resid"))

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
CorrMat %<>% #reorder_cormat() %>% 
  round(2) %>% get_upper_tri() %>% melt(na.rm = TRUE)

names(lagged_resid) <- paste0(names(lagged_resid), "_lag_", optLags[names(lagged_resid)])
laggedCorrMat <- cor(lagged_resid, use = "pairwise.complete.obs")
laggedCorrMat %<>% #reorder_cormat() %>% 
  round(2) %>% get_upper_tri() %>% melt(na.rm = TRUE)

runCorData <- data_frame(timestamp = time_series$timestamp)
with(lagged_resid, {
  t1 <- names(lagged_resid) %>% str_subset(resp_time)
  for (t in metrics){
    t2 <- names(lagged_resid) %>% str_subset(t)
    runCorData[t] <<- runCor(get(t1), get(t2), n = movWindow)
  }
})

theme_set(theme_bw(base_size = 16))



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  vals <- reactiveValues(
    CorrMatClick = CorrMat %>% filter(FALSE),
    laggedCorrMatClick = CorrMat %>% filter(FALSE),
    ts_choice1 = resp_time,
    ts_choice2 = metrics[1],
    l_ts_choice1 = resp_time,
    l_ts_choice2 = metrics[1]
  )
  
  CorrPlot <- function(CorrMat, dd){
    # ggplot2 : Quick correlation matrix heatmap
    # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
    ggplot(CorrMat, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      #geom_point(color = "black") +
      geom_tile(data = dd, color = "green", size = 2, fill="green", alpha=0.3) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Коэффициент\nкорреляции") +
      ggtitle("Корреляция отклонений от базовой линии") +
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
  }
  
  output$CorrPlot <- renderPlot({
    dd <- vals$CorrMatClick
    CorrPlot(CorrMat, dd)
  }, width = 500, height = 500)
  
  output$laggedCorrPlot <- renderPlot({
    dd <- vals$laggedCorrMatClick
    CorrPlot(laggedCorrMat, dd)
  }, width = 500, height = 500)
  
  observeEvent(input$corr_click, {
    vals$CorrMatClick <- nearPoints(CorrMat, input$corr_click, threshold = 100, maxpoints = 1)
    vals$ts_choice1 <- str_extract(vals$CorrMatClick$Var1, "ts\\d")
    vals$ts_choice2 <- str_extract(vals$CorrMatClick$Var2, "ts\\d")
  })
  
  observeEvent(input$lagged_corr_click, {
    vals$laggedCorrMatClick <- nearPoints(laggedCorrMat, input$lagged_corr_click, threshold = 100, maxpoints = 1)
    vals$l_ts_choice1 <- str_extract(vals$laggedCorrMatClick$Var1, "ts\\d")
    vals$l_ts_choice2 <- str_extract(vals$laggedCorrMatClick$Var2, "ts\\d")
  })
  
  
  output$text1 <- renderPrint({ 
    print(optLags)
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
    ts1 <- resid %>% select(get(vals$l_ts_choice1))
    ts2 <- resid %>% select(get(vals$l_ts_choice2))
    currentLag <- optLags[vals$l_ts_choice2] - optLags[vals$l_ts_choice1]
    crossCorr <- ccf(ts1, ts2, lag.max = MaxLag, type = "correlation")
    # browser()
    col <- rep(0, 2*MaxLag + 1)
    col[crossCorr$lag[,1,1] == currentLag] <- 1
    cross_data <- data_frame(Lag = crossCorr$lag[,1,1], CCF = crossCorr$acf[,1,1], col=factor(col))
    ggplot(cross_data, aes(x = Lag, y = CCF, fill=col)) +
      geom_bar(stat="identity", position = "identity") +
      scale_y_continuous(limits=c(-1, 1)) +
      xlab("Временной лаг") + ylab("Коэффициент кросс-корреляции") +
      ggtitle("График кросс-корреляции") +
      theme(legend.position="none")
  })
  
  output$rollCorr <- renderDygraph({
    data <- xts(runCorData, order.by = runCorData$timestamp)
    data$timestamp <- NULL
    crit <- 1.96 / sqrt(movWindow)
    dygraph(data, group="ts", ylab=enc2utf8("Скользящая корреляция на ts1")) %>%
      dyAxis("x", axisLabelFormatter = axisLabelFormatter) %>%
      dyRangeSelector(height = 20) %>% dyLegend(width = 400) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3) %>%
      dyOptions(colors = colorRampPalette(brewer.pal(11,"RdYlGn"))(length(metrics))) %>%
      dyLimit(crit, label=enc2utf8("крит"), color="red") %>%
      dyLimit(-crit, label=enc2utf8("крит"), color="red") %>%
      dyShading(-crit, crit, color = "#FFE6E6", axis = "y")
  })
  
  output$dygraphTS1 <- renderDygraph({
    ts1_val <- time_series %>% select(matches(vals$ts_choice1))
    ts1_base <- baselines %>% select(matches(vals$ts_choice1))
    names(ts1_base) <- "Базовая линия"
    ts1 <- xts(cbind(ts1_val, ts1_base), order.by = time_series$timestamp)
    # рисуем интерактивный график
    # TODO добавить доверительные интервалы
    dygraph(ts1, group="ts") %>%
      dyAxis("x", axisLabelFormatter = axisLabelFormatter)
  })
  
  output$dygraphTS2 <- renderDygraph({
    ts2_val <- time_series %>% select(matches(vals$ts_choice2))
    ts2_base <- baselines %>% select(matches(vals$ts_choice2))
    names(ts2_base) <- "Базовая линия"
    ts2 <- xts(cbind(ts2_val, ts2_base), order.by = time_series$timestamp)
    # рисуем интерактивный график
    dygraph(ts2, group="ts") %>% dyRangeSelector(height = 20) %>%
      dyAxis("x", axisLabelFormatter = axisLabelFormatter)
  })

  output$dygraphRPlot <- renderDygraph({
    data <- resid %>% select(matches(vals$ts_choice1),
                             matches(vals$ts_choice2))
    ts <- xts(data, order.by = time_series$timestamp)
    dygraph(ts, group="ts") %>% dyRangeSelector(height = 20) %>%
      dyAxis("x", axisLabelFormatter = axisLabelFormatter) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)
  })
  
  output$lag_dygraphRPlot <- renderDygraph({
    data <- lagged_resid %>% select(matches(vals$l_ts_choice1),
                             matches(vals$l_ts_choice2))
    ts <- xts(data, order.by = time_series$timestamp)
    dygraph(ts, group="ts") %>% dyRangeSelector(height = 20) %>%
      dyAxis("x", axisLabelFormatter = axisLabelFormatter) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)
  })
  
  output$ts_table = renderDataTable({
    time_series
  }, options = list(pageLength = 5, searching=FALSE))
  output$resid_table = renderDataTable({
    resid
  }, options = list(pageLength = 5, searching=FALSE))
  output$base_table = renderDataTable({
    baselines
  }, options = list(pageLength = 5, searching=FALSE))
})
