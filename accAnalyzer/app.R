pacman::p_load(shinydashboard, bslib, googlesheets4, shinythemes, ggplot2, highcharter, shiny, DT, tidyr, dplyr)
source("GoogleSheetsAnalyzer.R")
defaultUrl <- "https://docs.google.com/spreadsheets/d/1XqUbDBPDTwRxQYsmTz2R0g07LAD0iQtyB50LQYl-vHU/edit#gid=1840826699"

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
        menuSubItem("Lap Times", tabName = "subtab-laptimes", selected = T),
        menuSubItem("Tyres", tabName = "subtab-tyres"),
        menuSubItem("Drive Time", tabName = "subtab-drivetime"),
        menuSubItem("Fuel", tabName = "subtab-fuel"),
        menuSubItem("Experimental", tabName = "experimental", selected = F)
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
            textInput(inputId = "googleSheetsUrl", label = "Enter the Google Sheets URL:", value = defaultUrl),
          )
        )
      ),
      tabItem(
        tabName = "subtab-laptimes",
        fluidRow(
          box(
            width = 12,
            h2("Stint Overview"),
            plotOutput("subtab_laptimes_boxplot"),
            #sliderInput("laptime_range", label = "Upper Range", min = 0.5, max = 1, step = 0.01, value = 0.8)
          )
        ),
        br(),
        fluidRow(
          box(DT::dataTableOutput("tableGoogle"), width = 12)
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
          # box(DT::dataTableOutput("tableGoogle"), width = 12)
        ),
        p("Fabs was here"),
        br(), br(), br(), br(), br(), br(),
        p("All the time you have to leave a few paragraphs of space - Fernando Alonso - book author")
      )
    )
  )
)

server <- function(input, output) {
  lap_data <- reactive(read_googlesheet(input$googleSheetsUrl, "lap_data"))
  stint_overview <- reactive(read_googlesheet(input$googleSheetsUrl, "Stint overview"))
  # lap_data_X_stint_overview <-

  output$plotGoogle <- renderPlot(google_plot(lap_data()))
  data(iris)
  output$tableGoogle <- DT::renderDataTable(lap_data(), options = list(scrollX = TRUE))
  output$subtab_laptimes_boxplot <- renderPlot(laptimes_stintoverview(lap_data()))
  output$subtab_laptimes_table <- DT::renderDataTable(stint_overview(), options = list(scrollX = TRUE))
}

shinyApp(ui, server)
