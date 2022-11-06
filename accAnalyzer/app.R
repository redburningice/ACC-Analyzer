pacman::p_load(shinydashboard, googlesheets4, shinythemes, ggplot2, highcharter, shiny, DT, tidyr)
source("GoogleSheetsAnalyzer.R")

ui <- dashboardPage(
  title = "NoP - Not only Plots",
  skin = "purple",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Imports", tabName = "imports", selected = F),
      menuItem("Google Sheets Analyzer",
        tabName = "google-sheets-analyzer",
        menuSubItem("Tyres", tabName = "subtab-tyres"),
        menuSubItem("Drive Time", tabName = "subtab-drivetime"),
        menuSubItem("Fuel", tabName = "subtab-fuel"),
        menuSubItem("Experimental", tabName = "experimental", selected = T)
      ),
      menuItem("Motec Lap History Analyzer",
        tabName = "motec-lap-analyzer",
        menuSubItem("Lap Time Overview", tabName = "motec-lap-laptime"),
        menuSubItem("Understeer Analysis", tabName = "motec-lap-understeer")
      ),
      menuItem("Motec Section History Analyzer",
        tabName = "motec-section-analyzer",
        menuSubItem("Braking Analysis", tabName = "motec-section-braking"),
        menuSubItem("Detailled Section Analysis", tabName = "motec-section-braking-detailled"),
        menuSubItem("Understeer Analysis", tabName = "motec-section-understeer"),
        menuSubItem("Error Corrected Laptime", tabName = "motec-section-error-correction")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "imports",
        fluidRow(
          column(
            width = 12,
            textInput(inputId = "googleSheetsUrl", label = "Enter the Google Sheets URL:", value = "https://docs.google.com/spreadsheets/d/1ysa7HFW10e7g-7rtw3qsXWrMpoMSRQ34lW_PsJAvAbw/edit#gid=30962297"),
          )
        )
      ),
      tabItem(
        tabName = "subtab-tyres"
      ),
      tabItem(
        tabName = "experimental",
        p("This is experimental"),
        plotOutput("plotGoogle"),
        br(), br(),
        fluidRow(
            box(DT::dataTableOutput("tableGoogle"), width = 12)
        )
        
        
      )
    )
  )
)

server <- function(input, output) {
  sheetData <- reactive(read_googlesheet(input$googleSheetsUrl))

  output$plotGoogle <- renderPlot(google_plot(sheetData()))
  output$textGoogle <- renderText({
    "Hello Fabs"
  })
  data(iris)
  output$tableGoogle <- DT::renderDataTable(sheetData(), options = list(scrollX = TRUE))
}

shinyApp(ui, server)
