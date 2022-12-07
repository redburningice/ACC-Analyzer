pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table, stringr)

temperature_linegraph <- function(data, x, y, yRange = NULL, yLabel) {
    plot <- plot(data, x, y, yRange = NULL, yLabel = yLabel)
    
    plot + 
        geom_line(aes(y = `Air temp`, colour = "Air temp"), size = 1, show.legend = TRUE)+
        geom_line(aes(y = `Track temp`, colour = "Track temp"), size = 1, show.legend = TRUE)+
        scale_color_manual(values=c("blue", "red"))+
        labs(y = yLabel, colour = "Temperature")+
        scale_x_continuous(breaks = seq(0,24, by = 1), sec.axis = sec_axis(~ . + data$hour[1], name = "Ingame Time", breaks = seq(0,24, by = 1)))
}

weather_graph <- function(data, x, y, yRange = NULL, yLabel) {
    plot <- plot(data, x, y, yRange = NULL, yLabel = yLabel)
    
    plot + 
        geom_point(aes(y = as.factor(`Current weather`), colour = "Weather State"), size = 1, show.legend = TRUE)+
        scale_color_manual(values=c("blue"))+
        labs(y = yLabel, colour = yLabel)+
        scale_x_continuous(breaks = seq(0,24, by = 1), sec.axis = sec_axis(~ . + data$hour[1], name = "Ingame Time", breaks = seq(0,24, by = 1)))+
        scale_y_discrete(limits = c("Heavy rain", "Medium rain", "Light rain", "Drizzle", "Clear"))
}

trackstate_graph <- function(data, x, y, yRange = NULL, yLabel) {
    plot <- plot(data, x, y, yRange = NULL, yLabel = yLabel)
    
    plot + 
        geom_point(aes(y = as.factor(`Track state`), colour = "Track State"), size = 1, show.legend = TRUE)+
        scale_color_manual(values=c("blue"))+
        labs(y = yLabel, colour = yLabel)+
        scale_x_continuous(breaks = seq(0,24, by = 1), sec.axis = sec_axis(~ . + data$hour[1], name = "Ingame Time", breaks = seq(0,24, by = 1)))+
        scale_y_discrete(limits = c("Wet", "Damp", "Green", "Fast", "Optimum"))
}

brakewear_boxplot <- function(data, x, y, variable, yLabel, hasLabel, nColumns) {
    data <- facet_pivot(data, variable = variable)
    
    # labelling facets
    brakewear_per_lap <- c(
        `Brake pad level FL` = "FL",
        `Brake pad level FR` = "FR",
        `Brake pad level RL` = "RL",
        `Brake pad level RR` = "RR"
    )
    
    plot(data, x = x, y = variable, yLabel = yLabel)+
        facet_wrap(vars(`VariableName`), ncol = nColumns, strip.position = "top", labeller = as_labeller(brakewear_per_lap))+
        stat_summary(
            aes(label = round(..y.., 4), fill = `Driver`), 
            fun = brakewear_fun, 
            geom = "label")
    
    # variable = variable, , hasLabel = hasLabel, nColumns = nColumns
}

brakewear_fun <- function(y) {
    mean(diff(y, differences = 1)*-1)
}

fuel_boxplot <- function(data, x, y, yRange = NULL, hasLabel = FALSE, decimalPlaces = 2, yLabel = NULL) {
    data <- data %>% dplyr::filter(`Out lap?` == "No", `Lap` != 1, `In lap?` == "No")
    boxplot(data, x, y, yRange, hasLabel, decimalPlaces, yLabel)
}

fuel_linegraph <- function(data, x, y, yRange = NULL, hasStintSeperator = FALSE, colorVariable = NULL, yLabel = NULL, targetFuelConsumption = 3.8) {
    data <- data %>% dplyr::filter(`Out lap?` == "No", `Lap` != 1, `In lap?` == "No")
    linegraph(data, x, y, yRange, hasStintSeperator, colorVariable, yLabel)+
        geom_hline(aes(yintercept = targetFuelConsumption, colour = "Target Fuel Consumption"))+
        geom_label(aes(y = targetFuelConsumption, x = 0, label = targetFuelConsumption, colour = "Target Fuel Consumption"))
}
