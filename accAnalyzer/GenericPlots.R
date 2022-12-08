pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table, stringr)

source("SpecificPlots.R")

plot <- function(data, x, y, yRange = NULL, yLabel = NULL) {
    #data <- data %>% dplyr::filter(`Out lap?` == "No", `In lap?` == "No")
    
    # convert columns
    if (x == "Stint") data$`Stint` <- as.factor(data$`Stint`)
    
    # main plot
    if (!is.null(y)) {
        plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]]))+
            theme_bw()+
            labs(x = str_to_title(x), y = yLabel)
    } else {
        plot <- ggplot(data, aes(x = .data[[x]]))+
            theme_bw()+
            labs(x = str_to_title(x))
    }
    
    # add elements to the plot, if parameter is true
    if (!is.null(yRange)) plot <- plot + coord_cartesian(ylim = yRange)
    
    return(plot)
}

boxplot <- function(data, x, y, yRange = NULL, hasLabel = FALSE, decimalPlaces = 2, yLabel = NULL) {
    
    plot <- plot(data, x, y, yRange, yLabel) + geom_boxplot(aes(fill = `Driver`))
    
    # add elements to the plot, if parameter is true
    if (hasLabel == TRUE) {
        plot <- plot+ stat_summary(
            aes(label = round(..y.., decimalPlaces)), 
            geom = "label", 
            fun = median, 
            fill = "white"
        )
    }
    
    return(plot)
}

linegraph <- function(data, x, y, yRange = NULL, hasStintSeperator = FALSE, colorVariable = NULL, yLabel = NULL) {
    plot <- plot(data, x, y, yRange, yLabel) + geom_path(aes(group = 1, colour = if (!is.null(colorVariable)) .data[[colorVariable]]))+
        labs(colour=colorVariable)
    
    # add elements to the plot, if parameter is true
    if (hasStintSeperator == TRUE) {
        
        # old way of getting the stint seperator line
        # pitlaps <- data %>% dplyr::filter(`Out lap?` == "Yes") %>% pull(`Lap`)
        pitlaps <- vector(mode="numeric", length=max(max(data$`Stint`))-1)
        for (stintNr in 1:max(data$`Stint`)) {
            for (i in 1:nrow(data)) {
                if (data$`Stint`[i] == stintNr) {
                    pitlaps[stintNr] <- data$`Lap`[i] 
                    break
                }
                
            }
        }
        pitlaps <- pitlaps[-1]
        plot <- plot + geom_vline(xintercept = pitlaps) + labs(colour = colorVariable)
    }
    
    return(plot)
}


# Facet Functions

facet_pivot <- function(data, variable) {
    data %>% tidyr::pivot_longer(cols = starts_with(variable), names_to = "VariableName", values_to = variable)
}

facet <- function(data, variable, freeYAxis = FALSE, nColumns = 2, stripPos = "top") {
    if (freeYAxis == TRUE) scaleOption <- "free_y" else scaleOption <- "fixed"
    
    if ("VariableName" %in% colnames(data)) {
        facet_wrap(vars(`VariableName`), scales = scaleOption, ncol = nColumns, strip.position = stripPos)
    } else {
        facet_wrap(vars(.data[[variable]]), scales = scaleOption, ncol = nColumns, strip.position = stripPos)
    }
    
}

boxplot_facet <- function(data, x, y = NULL, variable, yRange = NULL, freeYAxis = FALSE, hasLabel = FALSE, decimalPlaces = 2, nColumns = 2, stripPos = "top", yLabel = NULL) {
    if (is.null(y)) {
        data <- facet_pivot(data, variable)
        y <- variable
    }
    
    boxplot(data, x, y, yRange = yRange, hasLabel = hasLabel, decimalPlaces = decimalPlaces, yLabel = yLabel)+
        facet(data, variable, freeYAxis, nColumns, stripPos)
}

linegraph_facet <- function(data, x, y, variable, yRange = NULL, hasStintSeperator = FALSE, colorVariable = NULL, freeYAxis = FALSE, nColumns = 2, stripPos = "top", yLabel = NULL) {
    if (is.null(y)) {
        data <- facet_pivot(data, variable)
        y <- variable
    }
    
    linegraph(data, x, y, yRange, hasStintSeperator, colorVariable, yLabel = yLabel)+
        facet(data, variable, freeYAxis, nColumns, stripPos = stripPos)
}





