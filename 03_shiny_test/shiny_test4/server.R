library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(reshape2)
library(stringr)

MaxLag <- 60

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
  # ������� ����� ������������� �� ������ ������������ ��� �������������� ����
  index <- which.max(ifelse(lag > 0, abs(ccorr), 0))
  maxCorr <- ccorr[index]
  optLag <- lag[index]
  # ������������ ����� ��� �������� �� ������������ �������� � 0.5
  critVal <- max(1.96 / sqrt(length(ts1)), 0.5)
  # ����� ������� �� ����� �������� �� ������� �������� �������
  if (optLag < 0) return(0)
  # ��� ����� ��������� ���, ���� ���������� �������������
  if (abs(maxCorr) < critVal) return(0)
  return(optLag)
}

# ���������� � �������� �������
resp_time <- "ts1"
# ���������� � ���������, �� ������� ����� �������� ����� �������
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

theme_set(theme_bw(base_size = 16))



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  vals <- reactiveValues(
    CorrMatClick = CorrMat %>% filter(FALSE),
    laggedCorrMatClick = CorrMat %>% filter(FALSE)
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
                           name="�����������\n����������") +
      ggtitle("���������� ���������� �� ������� �����") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
      # ����������� ��������� �� ����� � Shiny �� �������� � coord_fixed!!! (�������� ����������)
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
      cat(paste("����������� ���������� �� ������"))
    } else {
      cat("������ ����������� ���������� ����� ������",
            vals$CorrMatClick$Var1, "�", vals$CorrMatClick$Var2)
    }
  })
  
  output$CCFPlot <- renderPlot({
    if (length(vals$l_ts_choice1)==0) return(NULL)
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
      xlab("��������� ���") + ylab("����������� �����-����������") +
      ggtitle("������ �����-����������") +
      theme(legend.position="none")
  })
  
  output$TSPlot <- renderPlot({
    if (length(vals$ts_choice1)==0) return(NULL)
    data <- time_series %>% select(timestamp, starts_with(vals$ts_choice1),
                                   starts_with(vals$ts_choice2))
    ggtime <- gather(data, key = "type", value = "value",
                     starts_with(vals$ts_choice1), starts_with(vals$ts_choice2)) %>%
      filter(!str_detect(type, "resid")) %>%
      mutate(tstype = str_extract(type, "ts\\d"))
    
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle("��������� ��������� ���� � �������� �������") +
      facet_grid(tstype ~ .) + theme(legend.position = "bottom") +
      labs(color = "��������� ���")
  })

  output$RPlot <- renderPlot({
    if (length(vals$ts_choice1)==0) return(NULL)
    data <- resid %>% select(starts_with(vals$ts_choice1),
                             starts_with(vals$ts_choice2))
    # ---> TODO ��������, ������ �� �������� mutate
    # data %<>% mutate(timestamp = time_series$timestamp)
    data$timestamp <- time_series$timestamp
    ggtime <- gather(data, key = "type", value = "value",
                     starts_with(vals$ts_choice1), starts_with(vals$ts_choice2))
    # draw plot
    ggplot(ggtime, aes(x=timestamp, y=value, color=type)) + geom_line() +
      ggtitle(paste("������ ���������� ��������� ����� �� ������� �����")) +
      theme(legend.position = "bottom") + labs(color = "��������� ���")
  })
})