library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Тест Shiny на временных рядах"),
  

  mainPanel(
    plotOutput("CorrPlot", click = "corr_click"),
    # textOutput("text1"),
    verbatimTextOutput("ChoiceText"),
    plotOutput("TS1Plot"),
    plotOutput("TS2Plot"),
    plotOutput("R1Plot"),
    plotOutput("R2Plot")
  )

))