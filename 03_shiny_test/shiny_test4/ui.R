library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("���� Shiny �� ��������� �����"),
  

  mainPanel(width = 12,
    tabsetPanel(
      tabPanel("�������� ��������� ����",
        plotOutput("CorrPlot", click = "corr_click", height = "500px"),
        verbatimTextOutput("ChoiceText"),
        plotOutput("TSPlot"),
        plotOutput("RPlot")),
      tabPanel("����������� ����������� �����",
        plotOutput("laggedCorrPlot", click = "lagged_corr_click", height = "500px"),
        verbatimTextOutput("text1"),
        plotOutput("CCFPlot")
      )
    )
  )

))