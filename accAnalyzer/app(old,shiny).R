pacman::p_load(shiny, shinydashboard, googlesheets4, shinythemes, ggplot2)


ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("url", label = "Enter the googlesheet url"),
      actionButton("submitUrl", label = "Submit"),
      br(),br(),
      verticalTabsetPanel(
          verticalTabPanel(title = "Tyres"),
          verticalTabPanel("Drive Time"),
          verticalTabPanel("Fuel")
      )
    ),
    mainPanel(
        tabsetPanel(
            type = "tabs",
            tabPanel("Google Sheets Analyzer", plotOutput("google-sheets-analyzer")),
            tabPanel("Motec Lap Analyzer", plotOutput("motec-lap-analyzer")),
            tabPanel("Motec Section Analyzer", plotOutput("motec-section-analyzer"))
        ),
      h2("Boxplot"),
      plotOutput("plot1"),
      br(), br(), br(),
      h2("Summary"),
      dataTableOutput("table1")
    )
  )
)

server <- function(input, output) {
  output$table1 <- renderDataTable({
    summary(mtcars)
  })
  output$plot1 <- renderPlot({
    ggplot(mtcars, aes(mpg)) +
      geom_boxplot()
  })
}

shinyApp(ui = ui, server = server)
