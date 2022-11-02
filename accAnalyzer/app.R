pacman::p_load(shinydashboard, googlesheets4, shinythemes, ggplot2)

ui <- dashboardPage(
    title = "NoP - Not only Plots",
    skin = "purple",
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Tyres", tabName = "subtab-tyres"),
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
                    h2("Tyre Widgets")),
            tabItem(tabName = "subtab-fuel",
                    h2("Fuel Widgets"))
        )
    )
)

server <- function(input, output) {
    
}

shinyApp(ui,server)