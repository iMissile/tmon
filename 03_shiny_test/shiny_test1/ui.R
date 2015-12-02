library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Тест Shiny на временных рядах"),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("CorrPlot", click = "corr_click"),
    selectInput("InputName", "Выберите ряд:", choices = paste0("ts", 1:9)),
    textOutput("text1"),
    plotOutput("TSPlot"),
    plotOutput("RPlot")
  )
  
))