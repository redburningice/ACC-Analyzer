pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table)

source("accAnalyzer/GoogleSheetsAnalyzer.R")

data <- read_googlesheet("https://docs.google.com/spreadsheets/d/1XqUbDBPDTwRxQYsmTz2R0g07LAD0iQtyB50LQYl-vHU/edit#gid=1046975186", "lap_data")

parameter_plot <- function(dataa, column, hasGeomPoint) {
    ggplot(dataa, aes(.data[[column]], x = as.factor(`Stint`)))+
        geom_boxplot()+
        geom_point(data = NULL)+
        theme_bw()
}

parameter_plot(data, "Position overall", FALSE)
parameter_plot(data, "Position overall", TRUE)


source("accAnalyzer/GenericPlots.R")
# boxplot(data, "Stint", "Position overall", hasLabel = FALSE, decimalPlaces = 1)
# boxplot(data, "Stint", "Position overall", hasLabel = TRUE, decimalPlaces = 3)
# boxplot(data, "Stint", "Position overall", hasLabel = FALSE, decimalPlaces = 1, yRange = c(5,17))

source("accAnalyzer/GenericPlots.R")
boxplot_facet2x2(data, "Stint", "Max tyre temp", hasLabel = TRUE)

