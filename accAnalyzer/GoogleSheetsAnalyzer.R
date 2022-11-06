# What this Script is for:
# Generate data from a Google Sheet file.

# Press CTRL+Shift+Enter to execute the whole script 

#import standard packages
pacman::p_load(googlesheets4, ggplot2, tidyr)

read_googlesheet <- function(url) {
    gs4_deauth()
    data <- googlesheets4::read_sheet(url, sheet = "lap_data")
    
    # Data formatting
    data$`Sector 1` <- data$`Sector 1` / 1000
    data$`Sector 2` <- data$`Sector 2` / 1000
    data$`Sector 3` <- data$`Sector 3` / 1000
    data$`Lap time` <- data$`Lap time` / 1000
    
    for(i in 1:ncol(data)) {
        if(is.numeric(data[,i])) {
            data[,i] <- format(round(data[,i], 3), nsmall = 3)
        }
    }
    
    # data$`Fuel level (end of lap)` <- format(round(data$`Fuel level (end of lap)`, 3), nsmall = 3)
    
    return(data)
}

google_plot <- function(data) {
    ggplot(data, aes(x = `Lap time`,fill = `Driver`))+
        geom_boxplot()
}

google_text <- function(url) {
    gs4_deauth()
    data <- googlesheets4::read_sheet(url, sheet = "lap_data")
    
}


