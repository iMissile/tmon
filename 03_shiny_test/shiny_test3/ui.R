library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Тест Shiny на временных рядах"),
  

  mainPanel(width = 12,
    plotOutput("CorrPlot", click = "corr_click"),
    # textOutput("text1"),
    verbatimTextOutput("ChoiceText"),
    plotOutput("TSPlot"),
    plotOutput("RPlot"),
    plotOutput("CCFPlot")
  )

))