pacman::p_load(shinydashboard, bslib, googlesheets4, shinythemes, ggplot2, highcharter, shiny, DT, tidyr, dplyr)
source("GoogleSheetsAnalyzer.R")
source("GenericPlots.R")
defaultUrl <- "https://docs.google.com/spreadsheets/d/1Ey2Zzh1tQSUmUNthjTydjWiRf1EtNA2dyV273UKY6Jo/edit#gid=1474571985"

ui <- dashboardPage(
  title = "NoP - Not only Plots",
  skin = "purple",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      textInput(inputId = "googleSheetsUrl", label = "Enter the Google Sheets URL:", value = defaultUrl),
      textInput(inputId = "googleSheetsStratSheet", label = "Enter the Name of the current Stratsheet:", value = "Strat plan (map1)"),
      
      menuSubItem("Lap Times", tabName = "subtab-laptimes", selected = T),
      menuSubItem("Tyres and Brakes", tabName = "subtab-tyres"),
      menuSubItem("Fuel", tabName = "subtab_fuel"),
      menuSubItem("Pitstops [not implemented]", tabName = "subtab_pitstops"),
      menuSubItem("Drive Time [not implemented]", tabName = "subtab_drivetime"),
      menuSubItem("Weather", tabName = "subtab_weather")
      # menuItem("Motec Lap History Analyzer [not implemented]",
      #   tabName = "motec-lap-analyzer",
      #   menuSubItem("Lap Time Overview", tabName = "motec-lap-laptime"),
      #   menuSubItem("Understeer Analysis", tabName = "motec-lap-understeer")
      # ),
      # menuItem("Motec Section History Analyzer [not implemented]",
      #   tabName = "motec-section-analyzer",
      #   menuSubItem("Braking Analysis", tabName = "motec-section-braking"),
      #   menuSubItem("Detailled Section Analysis", tabName = "motec-section-braking-detailled"),
      #   menuSubItem("Understeer Analysis", tabName = "motec-section-understeer"),
      #   menuSubItem("Error Corrected Laptime", tabName = "motec-section-error-correction")
      # )
    )
  ),
  dashboardBody(
    tabItems(
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
              fluidRow(
                column(width = 2, checkboxGroupInput("subtab_tyres_avg_pres_overwrite", label = "Overwrite Pressure Range", choices = list("Dry Tyres" = 1, "Wet Tyres" = 2))),
                column(width = 6, sliderInput("subtab_tyres_avg_pres_range", label = "Pressure Range [PSI]", min = 20, max = 40, value = c(27, 28.5), step = 0.5, width = "100%"))
              ),
              plotOutput("subtab_tyres_avg_pres_boxplot"),
              plotOutput("subtab_tyres_avg_pres_linechart")
            ),
            tabPanel(
              "Temperatures",
              fluidRow(
                  column(width = 2, checkboxGroupInput("subtab_tyres_avg_temp_overwrite", label = "Overwrite Temperature Range", choices = list("Dry Tyres" = 1, "Wet Tyres" = 2))),
                  column(width = 6, sliderInput("subtab_tyres_avg_temp_range", label = "Temperature Range [??C]", min = 20, max = 120, value = c(80, 100), step = 5, width = "100%")) 
              ),
              plotOutput("subtab_tyres_avg_temp_boxplot"),
              plotOutput("subtab_tyres_avg_temp_linechart")
            ),
            tabPanel(
              "Brake Temps",
              sliderInput("subtab_tyres_avg_braketemp_range", label = "Temperature Range [??C]", min = 100, max = 600, value = c(200, 450), step = 50, width = "50%"),
              plotOutput("subtab_tyres_avg_braketemp_boxplot"),
              plotOutput("subtab_tyres_avg_braketemp_linechart")
            ),
            tabPanel(
              "Brake Wear",
              plotOutput("subtab_brakewear_boxplot"),
              plotOutput("subtab_brakewear_linechart")
            )
          )
        )
      ),
      tabItem(
        tabName = "subtab_fuel",
        plotOutput("subtab_fuel_boxplot"),
        plotOutput("subtab_fuel_linechart"),
        fluidRow(
          column(
            width = 6,
            checkboxInput("subtab_fuel_overwrite_fueltarget", label = "Overwrite Fuel Target?", value = FALSE),
            textInput("subtab_fuel_fueltarget", label = "Fuel Target [L]")
          )
        )
      ),
      tabItem(
        tabName = "subtab_weather",
        h2("Temperature History"),
        fluidRow(plotOutput("subtab_weather_temperature")),
        br(),
        h2("Weather History"),
        fluidRow(plotOutput("subtab_weather_history")),
        br(),
        h2("Track State History"),
        fluidRow(plotOutput("subtab_trackstate_history")),
        br(), br(),
        fluidRow(DT::dataTableOutput("subtab_weather_table"))
      )
    )
  )
)

server <- function(input, output) {
  lap_data <- reactive(read_googlesheet(input$googleSheetsUrl, "lap_data"))
  stint_overview <- reactive(read_googlesheet(input$googleSheetsUrl, "Stint overview"))
  event_info <- reactive(read_googlesheet(input$googleSheetsUrl, "Event info"))
  weather_data <- reactive(read_googlesheet(input$googleSheetsUrl, "weather_data"))

  laptimes_sorted_filtered <- reactive(lap_data() %>% dplyr::filter(`Out lap?` == "No", `Lap` != 1, `In lap?` == "No") %>% pull(`Lap time`) %>% sort())
  laptimes_range <- reactive(c(laptimes_sorted_filtered()[1], laptimes_sorted_filtered()[length(laptimes_sorted_filtered()) * 0.98]))

  # Laptimes
  output$tableGoogle <- DT::renderDataTable(lap_data(), options = list(scrollX = TRUE))
  output$subtab_laptimes_boxplot <- renderPlot(boxplot(lap_data(), x = "Stint", y = "Lap time", hasLabel = TRUE, yRange = laptimes_range(), yLabel = "Lap Time [s]"))
  output$subtab_laptimes_table <- DT::renderDataTable(stint_overview(), options = list(scrollX = TRUE))
  #output$subtab_laptimes_linechart <- renderPlot(linegraph_facet(lap_data(), x = "Stintlap", y = "Lap time", variable = "Stint", nColumns = 1, colorVariable = "Driver", stripPos = "right", yRange = laptimes_range(), yLabel = "Lap Time [s]", hasStintSeperator = FALSE))
  output$subtab_laptimes_linechart <- renderPlot(linegraph(lap_data(), x = "Lap", y = "Lap time", colorVariable = "Driver", yRange = laptimes_range(), yLabel = "Lap Time [s]", hasStintSeperator = TRUE))
  
  # Tyres and Brakes
  range1 <- reactive(
    if (is.null(input$subtab_tyres_avg_pres_overwrite)) {
      input$subtab_tyres_avg_pres_range
    } else if ((1 %in% input$subtab_tyres_avg_pres_overwrite) & (2 %in% input$subtab_tyres_avg_pres_overwrite)) {
      c(26.5, 32.0)
    } else if (input$subtab_tyres_avg_pres_overwrite == c(1)) {
      c(26.5, 28.5)
    } else if (input$subtab_tyres_avg_pres_overwrite == c(2)) {
      c(29.0, 32.0)
    }
  )
  output$subtab_tyres_avg_pres_boxplot <- renderPlot(boxplot_facet(lap_data(), x = "Stint", y = NULL, variable = "Avg tyre pressure", yLabel = "AVG Tyre Pressure [PSI]", hasLabel = TRUE, nColumns = 2, yRange = range1()))
  output$subtab_tyres_avg_pres_linechart <- renderPlot(linegraph_facet(lap_data(), x = "Lap", y = NULL, variable = "Avg tyre pressure", yLabel = "AVG Tyre Pressure [PSI]", nColumns = 2, yRange = range1(), hasStintSeperator = TRUE, colorVariable = "Driver"))

  range2 <- reactive(
      if (is.null(input$subtab_tyres_avg_temp_overwrite)) {
          input$subtab_tyres_avg_temp_range
      } else if ((1 %in% input$subtab_tyres_avg_temp_overwrite) & (2 %in% input$subtab_tyres_avg_pres_overwrite)) {
          c(20, 110)
      } else if (input$subtab_tyres_avg_temp_overwrite == c(1)) {
          c(70, 110)
      } else if (input$subtab_tyres_avg_temp_overwrite == c(2)) {
          c(20, 100)
      }
  )
  output$subtab_tyres_avg_temp_boxplot <- renderPlot(boxplot_facet(lap_data(), x = "Stint", y = NULL, variable = "Avg tyre temp", hasLabel = TRUE, yLabel = "AVG Tyre Temperature [??C]", nColumns = 2, yRange = range2()))
  output$subtab_tyres_avg_temp_linechart <- renderPlot(linegraph_facet(lap_data(), x = "Lap", y = NULL, variable = "Avg tyre temp", nColumns = 2, yLabel = "AVG Tyre Temperature [??C]", yRange = range2(), hasStintSeperator = TRUE, colorVariable = "Driver"))

  output$subtab_tyres_avg_braketemp_boxplot <- renderPlot(boxplot_facet(lap_data(), x = "Stint", y = NULL, variable = "Brake avg temp", yLabel = "AVG Brake Temperature [??C]", hasLabel = TRUE, nColumns = 2, yRange = input$subtab_tyres_avg_braketemp_range))
  output$subtab_tyres_avg_braketemp_linechart <- renderPlot(linegraph_facet(lap_data(), x = "Lap", y = NULL, variable = "Brake avg temp", yLabel = "AVG Brake Temperature [??C]", nColumns = 2, yRange = input$subtab_tyres_avg_braketemp_range, hasStintSeperator = TRUE, colorVariable = "Driver"))

  output$subtab_brakewear_boxplot <- renderPlot(brakewear_boxplot(lap_data(), x = "Stint", y = NULL, variable = "Brake pad level", yLabel = "Brake Pad Wear per Lap [mm]", hasLabel = TRUE, nColumns = 2))
  output$subtab_brakewear_linechart <- renderPlot(linegraph_facet(lap_data(), x = "Lap", y = NULL, variable = "Brake pad level", yLabel = "Brake Pad Level [mm]", nColumns = 2, hasStintSeperator = TRUE, colorVariable = "Driver"))

  # Fuel
  targetFuelConsumption <- reactive(
    if (input$subtab_fuel_overwrite_fueltarget) {
      as.numeric(input$subtab_fuel_fueltarget)
    } else {
      unlist(read_googlesheet_range(input$googleSheetsUrl, input$googleSheetsStratSheet, range = "A2:A3")[1, 1])
    }
  )

  output$subtab_fuel_boxplot <- renderPlot(fuel_boxplot(lap_data(), x = "Stint", y = "Fuel consumption avg", yLabel = "Fuel Consumption [l/lap]", hasLabel = TRUE))
  output$subtab_fuel_linechart <- renderPlot(fuel_linegraph(lap_data(), x = "Lap", y = "Fuel consumption avg", yLabel = "Fuel Consumption [l/lap]", colorVariable = "Driver", hasStintSeperator = TRUE, targetFuelConsumption = targetFuelConsumption()))

  # Weather
  output$subtab_weather_temperature <- renderPlot(temperature_linegraph(weather_data(), x = "Race Time", y = NULL, yLabel = "Temperature [??C]"))
  output$subtab_weather_history <- renderPlot(weather_graph(weather_data(), x = "Race Time", y = NULL, yLabel = "Weather State"))
  output$subtab_trackstate_history <- renderPlot(trackstate_graph(weather_data(), x = "Race Time", y = NULL, yLabel = "Track State"))
  output$subtab_weather_table <- DT::renderDataTable(weather_data(), options = list(scrollX = TRUE))

  # Pitstops

  # Drive Time
}

shinyApp(ui, server)
