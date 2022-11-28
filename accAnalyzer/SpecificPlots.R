pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table, stringr)

weather_linegraph <- function(data, x, y, yRange = NULL, yLabel) {
    plot <- plot(data, x, y, yRange = NULL, yLabel = yLabel)
    
    plot + 
        geom_line(aes(y = `Air temp`, colour = "Air temp"), size = 1, show.legend = TRUE)+
        geom_line(aes(y = `Track temp`, colour = "Track temp"), size = 1, show.legend = TRUE)+
        scale_color_manual(values=c("blue", "red"))+
        labs(y = "Temperature [°C]", colour = "Temperature")
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
