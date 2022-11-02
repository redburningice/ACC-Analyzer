# What this Script is for:
# Generate data from a Google Sheet file.

# Press CTRL+Shift+Enter to execute the whole script 

# clear global environment
rm(list = ls())

#import standard packages
pacman::p_load(googlesheets4, ggplot2)

# Import Google Sheet - Googlesheets4 implementation
    # disables access via token
    gs4_deauth()
data <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1LuRYr2v4HAY2H49sGIhCBR87E5w23ItsTEg_pbXkaoM/edit#gid=1427649818', sheet = "lap_data")

ggplot(data, aes(data$`Max tyre pressure FL`))+
    geom_boxplot()+
    coord_flip()
