# What this Script is for:
# Generate data from a Google Sheet file.

# Press CTRL+Shift+Enter to execute the whole script 

#import standard packages
pacman::p_load(googlesheets4, ggplot2)

read_googlesheet <- function(url) {
    gs4_deauth()
    data <- googlesheets4::read_sheet(url)
}