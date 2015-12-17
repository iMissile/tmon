library(shinydashboard)
library(dygraphs)

dashboardPage(
  dashboardHeader(title = "Тест ShinyDashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Аналитика", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Данные", tabName = "rawdata", icon = icon("th"))
    ),
    dateRangeInput("DateRange",
                   label = "Введите диапазон дат для анализа",
                   separator = " - ", format = "dd/mm/yyyy",
                   startview = "year", language = "ru", weekstart = 1
    )
  ),
  dashboardBody(
    fluidRow(tabItems(
      # Наш dashboard
      tabItem(tabName = "dashboard",
        tabBox(title = "Коррелограмма", width = 12,
          tabPanel("Исходные ряды",
            p("Корреляционная матрица времени отклика с другими метриками
              без применения временных лагов. При выборе любой ячейки матрицы будет
              показана более подробная информация о выбранных временных рядах."),
            plotOutput("CorrPlot", click = "corr_click", height = "500px"),
            verbatimTextOutput("ChoiceText"),
            fluidRow(box(
              title = "График выбранных временных рядов",
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              p("Базовая линия в данном примере рассчитывается при помощи
              комбинации регрессионных моделей среднедневного уровня и мультипликативной
              сезонной компоненты, моделируемой регрессионным методом по 15-минутным интервалам."),
              dygraphOutput("dygraphTS1", height = "150px"),
              dygraphOutput("dygraphTS2", height = "160px")
            )),
            fluidRow(box(
              title = "Скользящая корреляция остатков",
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              textOutput("textRoll"),
              dygraphOutput("rollCorr", height = "450px")
            )),
            fluidRow(box(
              title = "График отклонений выбранных рядов от базовой линии",
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              dygraphOutput("dygraphRPlot", height = "260px")
            ))
          ),
          tabPanel("Оптимальные лаги",
            p("Корреляционная матрица времени отклика с другими метриками
              с автоподбором оптимальных временных лагов (по максимальному абсолютному значению
              коэффициента кросс-корреляции). При выборе любой ячейки матрицы будет
              показана более подробная информация о выбранных временных рядах."),
            plotOutput("laggedCorrPlot", click = "lagged_corr_click", height = "500px"),
            p("При помощи метода автоподбора оптимальных лагов были получены
              следующие значения:"),
            verbatimTextOutput("text1"),
            p("График коэффициентов кросс-корреляции выбранных метрик с выделенным
              значением кросс-корреляции при текущих применённых лагах. Красным
              цветом выделена область со статистически незначимыми коэффициентами."),
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
