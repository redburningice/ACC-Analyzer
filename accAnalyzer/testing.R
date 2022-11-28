pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table)

source("accAnalyzer/GoogleSheetsAnalyzer.R")

data <- read_googlesheet("https://docs.google.com/spreadsheets/d/1XqUbDBPDTwRxQYsmTz2R0g07LAD0iQtyB50LQYl-vHU/edit#gid=1046975186", "lap_data")
laptimes_sorted_filtered <- data %>% dplyr::filter(`Out lap?` == "No", `Lap` != 1, `In lap?` == "No") %>% pull(`Lap time`) %>% sort()
laptimes_range <- c(laptimes_sorted_filtered[1],laptimes_sorted_filtered[length(laptimes_sorted_filtered*0.98)])


source("accAnalyzer/GenericPlots.R")


linegraph_facet(data, x = "Lap", y = "Lap time", variable = "Stint", freeYAxis = TRUE)
linegraph_facet(data, x = "Lap", y = NULL, variable = "Stint", colorVariable = "Driver", nColumns = 1, stripPos = "right")

linegraph_facet(data, x = "Stintlap", y = "Lap time", variable = "Stint", nColumns = 1, colorVariable = "Driver", stripPos = "left", yRange = laptimes_range)

if("Position overall" %in% colnames(data)) print("INININ") else print("NOTNONOTNOTNOT")

