library(shinydashboard)
library(dygraphs)

dashboardPage(
  dashboardHeader(title = "���� ShinyDashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("���������", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("������", tabName = "rawdata", icon = icon("th"))
    )
  ),
  dashboardBody(
    fluidRow(tabItems(
      # ��� dashboard
      tabItem(tabName = "dashboard",
        tabBox(title = "�������������", width = 12,
          tabPanel("�������� ����",
             plotOutput("CorrPlot", click = "corr_click", height = "500px"),
             verbatimTextOutput("ChoiceText"),
             fluidRow(box(
               title = "������ ��������� ��������� �����",
               status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width = 12,
               dygraphOutput("dygraphTS1", height = "150px"),
               dygraphOutput("dygraphTS2", height = "160px")
             )),
             fluidRow(box(
               title = "������ ���������� ��������� ����� �� ������� �����",
               status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width = 12,
               dygraphOutput("dygraphRPlot", height = "260px")
             ))
          ),
          tabPanel("����������� ����",
            plotOutput("laggedCorrPlot", click = "lagged_corr_click", height = "500px"),
            verbatimTextOutput("text1"),
            plotOutput("CCFPlot"),
            fluidRow(box(
              title = "���������� ���������� ��������",
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              dygraphOutput("rollCorr", height = "450px")
            )),
            fluidRow(box(
              title = "������ ���������� ��������� ����� �� ������� ����� (� ������)",
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              dygraphOutput("lag_dygraphRPlot", height = "260px")
            ))
          )
        )
      ),
      # ������� � ��������� ������� ��� �������
      tabItem(tabName = "rawdata",
        tabBox(title = "�������� ������ ��� �������", width = 12,
          tabPanel("�������� ����",
            dataTableOutput("ts_table")
          ),
          tabPanel("������� �����",
            dataTableOutput("base_table")
          ),
          tabPanel("���������� �� ������� �����",
            dataTableOutput("resid_table")
          )
        )
      )
    ))
  )
)
