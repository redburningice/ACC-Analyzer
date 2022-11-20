pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table)

source("accAnalyzer/GoogleSheetsAnalyzer.R")

data <- read_googlesheet("https://docs.google.com/spreadsheets/d/1XqUbDBPDTwRxQYsmTz2R0g07LAD0iQtyB50LQYl-vHU/edit#gid=1046975186", "lap_data")

source("accAnalyzer/GenericPlots.R")


linegraph_facet(data, x = "Lap", y = "Lap time", variable = "Stint")
linegraph_facet(data, x = "Lap", y = NULL, variable = "Stint")

if("Position overall" %in% colnames(data)) print("INININ") else print("NOTNONOTNOTNOT")
