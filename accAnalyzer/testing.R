pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table)

source("accAnalyzer/GoogleSheetsAnalyzer.R")

data <- read_googlesheet("https://docs.google.com/spreadsheets/d/1XqUbDBPDTwRxQYsmTz2R0g07LAD0iQtyB50LQYl-vHU/edit#gid=1046975186", "lap_data")

source("accAnalyzer/GenericPlots.R")
general_plot(data, x = "Stint", "Lap time", yRange = c(50, 100))

boxplot_facet(data, "Stint", "Brake avg temp", yRange = NULL, freeYAxis = TRUE, hasLabel = FALSE, decimalPlaces = 2, nColumns = 2)

boxplot(data, "Stint", "Brake avg temp FL")


source("accAnalyzer/GenericPlots.R")
linegraph(data, "Lap", "Brake avg temp RR", colorVariable = "Stint")
