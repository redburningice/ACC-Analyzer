pacman::p_load(shinydashboard, googlesheets4, shinythemes, ggplot2)

ui <- dashboardPage(
    title = "NoP - Not only Plots",
    skin = "purple",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
        sidebarMenu(id = "sidebar",
            menuItem("Google Sheets Analyzer", tabName = "google-sheets-analyzer", newTab = TRUE),
            menuSubItem("Tyres", tabName = "subtab-tyres"),
            menuItem("Source code", icon = icon("file-code-o"), 
                     href = "https://github.com/rstudio/shinydashboard/"),
            menuItem("Drive Time", tabName = "subtab-drive-timer"),
            
            menuItem("Fuel", tabName = "subtab-fuel")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "subtab-tyres",
                    fluidRow(
                        box(plotOutput("plot1", height = 250))
                    )),
            tabItem(tabName = "subtab-drive-timer",
                    h2("Tyre Widgets"),fluidRow(
                        box(plotOutput("plot1")),
                        
                        box(
                            "Box content here", br(), "More box content",
                            sliderInput("slider", "Slider input:", 1, 100, 50),
                            textInput("text", "Text input:")
                        )
                    )),
            tabItem(tabName = "subtab-fuel",
                    h2("Fuel Widgets"))
        )
    )
    
)

server <- function(input, output) {
    
}

shinyApp(ui,server)