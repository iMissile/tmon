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
    fluidRow(tabItems(
      # Наш dashboard
      tabItem(tabName = "dashboard",
        tabBox(title = "Коррелограмма", width = 12,
          tabPanel("Исходные ряды",
             plotOutput("CorrPlot", click = "corr_click", height = "500px"),
             verbatimTextOutput("ChoiceText"),
             fluidRow(box(
               title = "График выбранных временных рядов",
               status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width = 12,
               dygraphOutput("dygraphTS1", height = "150px"),
               dygraphOutput("dygraphTS2", height = "160px")
             )),
             fluidRow(box(
               title = "График отклонений выбранных рядов от базовой линии",
               status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width = 12,
               dygraphOutput("dygraphRPlot", height = "260px")
             ))
          ),
          tabPanel("Оптимальные лаги",
            plotOutput("laggedCorrPlot", click = "lagged_corr_click", height = "500px"),
            verbatimTextOutput("text1"),
            plotOutput("CCFPlot"),
            fluidRow(box(
              title = "График отклонений выбранных рядов от базовой линии (с лагами)",
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              dygraphOutput("lag_dygraphRPlot", height = "260px")
            ))
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
    ))
  )
)
