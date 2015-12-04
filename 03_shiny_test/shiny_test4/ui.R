library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Тест Shiny на временных рядах"),
  

  mainPanel(width = 12,
    tabsetPanel(
      tabPanel("Исходные временные ряды",
        plotOutput("CorrPlot", click = "corr_click"),
        verbatimTextOutput("ChoiceText"),
        plotOutput("TSPlot"),
        plotOutput("RPlot")),
      tabPanel("Определение оптимальных лагов",
        plotOutput("laggedCorrPlot"),
        verbatimTextOutput("text1"),
        plotOutput("CCFPlot")
      )
    )
  )

))