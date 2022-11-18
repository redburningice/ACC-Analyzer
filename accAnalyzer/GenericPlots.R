pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table, stringr)

boxplot <- function(data, x, y, yRange = NULL, hasLabel = FALSE, decimalPlaces = 2) {
    
    # convert columns
    if (x == "Stint") {
        data$`Stint` <- as.factor(data$`Stint`)
    }
    
    # main plot
    plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = `Driver`))+
        geom_boxplot()+
        theme_bw()+
        labs(x = str_to_title(x), y = str_to_title(y))
    
    # add elements to the plot, if parameter is true
    if (hasLabel == TRUE) {
        plot <- plot+ stat_summary(
            aes(label = round(..y.., decimalPlaces)), 
            geom = "label", 
            fun = median, 
            fill = "white"
        )
    }
    
    if (!is.null(yRange)) {
        plot <- plot + coord_cartesian(ylim = yRange)
    }
    
    return(plot)
}

boxplot_facet <- function(data, x, variable, yRange = NULL, freeYAxis, hasLabel = FALSE, decimalPlaces = 2, individualsList) {
    first_column <- paste(variable, individualsList[1], sep = " ")
    last_column <- paste(variable, individualsList[length(individualsList)], sep = " ")
    
    data <- data %>% tidyr::pivot_longer(cols = all_of(first_column):all_of(last_column), names_to = "Tyre", values_to = variable)
    
    boxplot(data, x, y = variable, yRange = yRange, hasLabel = hasLabel, decimalPlaces = decimalPlaces)+
        facet_wrap(vars(`Tyre`))
}


