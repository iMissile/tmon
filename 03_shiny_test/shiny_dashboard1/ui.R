library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Тест ShinyDashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Аналитика", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Данные", tabName = "rawdata", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # Наш dashboard
      tabItem(tabName = "dashboard",
        fluidRow(
          tabBox(title = "Коррелограмма", width = 12,
             tabPanel("Исходные ряды",
                plotOutput("CorrPlot", click = "corr_click", height = "500px"),
                verbatimTextOutput("ChoiceText")
             ),
             tabPanel("Оптимальные лаги",
               plotOutput("laggedCorrPlot", click = "lagged_corr_click", height = "500px"),
               verbatimTextOutput("text1"),
               plotOutput("CCFPlot")
             )
          )
        ),
        fluidRow(
          box(
            title = "График выбранных временных рядов",
            status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            dygraphOutput("dygraphTS1", height = "150px"),
            dygraphOutput("dygraphTS2", height = "150px")
          )
        ),
        fluidRow(
          box(
            title = "График отклонений выбранных рядов от базовой линии",
            status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            dygraphOutput("dygraphRPlot", height = "250px")
          )
        )
      ),
      # Таблица с исходными данными для анализа
      tabItem(tabName = "rawdata",
        tabBox(title = "Исходные данные для анализа", width = 12,
          tabPanel("Исходные ряды",
            dataTableOutput("ts_table")
          ),
          tabPanel("Базовые линии",
            dataTableOutput("base_table")
          ),
          tabPanel("Отклонения от базовых линий",
            dataTableOutput("resid_table")
          )
        )
      )
    )
  )
)

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Тест Shiny на временных рядах"),
#   
# 
#   mainPanel(width = 12,
#     tabsetPanel(
#       tabPanel("Исходные временные ряды",
#         plotOutput("CorrPlot", click = "corr_click", height = "500px"),
#         verbatimTextOutput("ChoiceText"),
#         plotOutput("TSPlot"),
#         plotOutput("RPlot")),
#       tabPanel("Определение оптимальных лагов",
#         plotOutput("laggedCorrPlot", click = "lagged_corr_click", height = "500px"),
#         verbatimTextOutput("text1"),
#         plotOutput("CCFPlot")
#       )
#     )
#   )
# 
# ))