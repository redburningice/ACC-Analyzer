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
      menuItem("Imports", tabName = "imports"),
      menuItem("Google Sheets Analyzer",
        tabName = "google-sheets-analyzer",
        menuSubItem("Lap Times", tabName = "subtab-laptimes", selected = F),
        menuSubItem("Tyres and Brakes", tabName = "subtab-tyres", selected = T),
        menuSubItem("Drive Time", tabName = "subtab-drivetime"),
        menuSubItem("Fuel", tabName = "subtab-fuel"),
        menuSubItem("Weather", tabName = "subtab-weather"),
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
          tabBox(
            id = "tabset1", width = 12,
            tabPanel("Boxplot", plotOutput("subtab_laptimes_boxplot")),
            tabPanel("Line Chart", plotOutput("subtab_laptimes_linechart"))
          )
        ),
        br(),
        fluidRow(
          box(DT::dataTableOutput("tableGoogle"), width = 12)
        )
      ),
      tabItem(
        tabName = "subtab-tyres",
        fluidRow(
          tabBox(
            id = "tabset2", width = 12,
            tabPanel(
              "Pressure",
              sliderInput("subtab_tyres_avg_pres_range", label = "Pressure Range [PSI]", min = 20, max = 40, value = c(27, 28.5), step = 0.5, width = "50%"),
              plotOutput("subtab_tyres_avg_pres_boxplot"),
              plotOutput("subtab_tyres_avg_pres_linechart")
            ),
            tabPanel(
              "Temperatures",
              sliderInput("subtab_tyres_avg_temp_range", label = "Temperature Range [°C]", min = 40, max = 120, value = c(80, 100), step = 5, width = "50%"),
              plotOutput("subtab_tyres_avg_temp_boxplot"),
              plotOutput("subtab_tyres_avg_temp_linechart")
            ),
            tabPanel(
              "Brake Temps",
              sliderInput("subtab_tyres_avg_braketemp_range", label = "Temperature Range [°C]", min = 100, max = 600, value = c(150, 300), step = 50, width = "50%"),
              plotOutput("subtab_tyres_avg_braketemp_boxplot"),
              plotOutput("subtab_tyres_avg_braketemp_linechart"),
              plotOutput("subtab_tyres_avg_brakewear_linechart")
            ),
            tabPanel(
              "Brake Wear",
              sliderInput("subtab_brakewear_range", label = "Temperature Range [°C]", min = 100, max = 600, value = c(150, 300), step = 50, width = "50%"),
              plotOutput("subtab_brakewear_boxplot"),
              plotOutput("subtab_brakewear_linechart")
            )
          )
        )
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

  output$plotGoogle <- renderPlot(google_plot(lap_data()))

  # Laptimes
  output$tableGoogle <- DT::renderDataTable(lap_data(), options = list(scrollX = TRUE))
  output$subtab_laptimes_boxplot <- renderPlot(stint_overview_boxplot(lap_data()))
  output$subtab_laptimes_table <- DT::renderDataTable(stint_overview(), options = list(scrollX = TRUE))
  output$subtab_laptimes_linechart <- renderPlot(stint_overview_linechart(lap_data()))

  # Tyres and Brakes
  output$subtab_tyres_avg_pres_boxplot <- renderPlot(tyres_boxplot(lap_data(), input$subtab_tyres_avg_pres_range, "pressure"))
  output$subtab_tyres_avg_pres_linechart <- renderPlot(tyres_linechart(lap_data(), input$subtab_tyres_avg_pres_range, "pressure"))

  output$subtab_tyres_avg_temp_boxplot <- renderPlot(tyres_boxplot(lap_data(), input$subtab_tyres_avg_temp_range, "temperature"))
  output$subtab_tyres_avg_temp_linechart <- renderPlot(tyres_linechart(lap_data(), input$subtab_tyres_avg_temp_range, "temperature"))

  output$subtab_tyres_avg_braketemp_boxplot <- renderPlot(tyres_boxplot(lap_data(), input$subtab_tyres_avg_braketemp_range, "braketemps"))
  output$subtab_tyres_avg_braketemp_linechart <- renderPlot(tyres_linechart(lap_data(), input$subtab_tyres_avg_braketemp_range, "braketemps"))

  output$subtab_brakewear_boxplot <- renderPlot(tyres_boxplot(lap_data(), NA, "brakewear"))
  output$subtab_brakewear_linechart <- renderPlot(tyres_linechart(lap_data(), NA, "brakewear"))
}

shinyApp(ui, server)
