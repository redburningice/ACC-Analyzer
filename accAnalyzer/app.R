pacman::p_load(shinydashboard, googlesheets4, shinythemes, ggplot2)
data(mtcars)
head(mtcars)
source("GoogleSheetsAnalyzer.R")

ui <- dashboardPage(
  title = "NoP - Not only Plots",
  skin = "purple",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Imports", tabName = "imports", selected = TRUE),
      menuItem("Google Sheets Analyzer",tabName = "google-sheets-analyzer",
        menuSubItem("Tyres", tabName = "subtab-tyres"),
        menuSubItem("Drive Time", tabName = "subtab-drivetime"),
        menuSubItem("Fuel", tabName = "subtab-fuel"),
        menuSubItem("Experimental", tabName = "experimental")
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
              column(width = 12,
                     textInput(inputId = "googleSheetsUrl", label = "Enter the Google Sheets URL:", value = "https://docs.google.com/spreadsheets/d/1LuRYr2v4HAY2H49sGIhCBR87E5w23ItsTEg_pbXkaoM/edit#gid=1427649818"),
              )
          )
        ),
        tabItem(
          tabName = "experimental",
          plotOutput("plot1"),
          plotOutput("plotGoogle")
        ))
  )
  
)

server <- function(input, output) {
    data <- reactive(
        read_googlesheet(input$googleSheetsUrl)
    )
    
    output$submittedGoogleSheetsUrl <- renderText(input$googleSheetsUrl)
    output$plotGoogle <- renderPlot(data$`Track Temp`)
}

shinyApp(ui, server)
