# What this Script is for:
# Generate data from a Google Sheet file.

# Press CTRL+Shift+Enter to execute the whole script 

# clear global environment
rm(list = ls())

#import standard packages
pacman::p_load(Hmisc, plyr, pacman, ggpubr, gridExtra, tidyquant, magrittr, dplyr, GGally, ggplot2, ggthemes, ggvis, 
               httr, lubridate, plotly, rio, markdown, shiny, stringr, tidyr, tidyverse, zoo, googlesheets4)

# Import Google Sheet - Googlesheets4 implementation
 # disables access via token
gs4_deauth()
sheets4 <- read_sheet('https://docs.google.com/spreadsheets/d/1LuRYr2v4HAY2H49sGIhCBR87E5w23ItsTEg_pbXkaoM/edit#gid=1427649818', sheet = "lap_data")
