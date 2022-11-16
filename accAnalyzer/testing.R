pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table)

source("accAnalyzer/GoogleSheetsAnalyzer.R")
data <- read_googlesheet("https://docs.google.com/spreadsheets/d/1XqUbDBPDTwRxQYsmTz2R0g07LAD0iQtyB50LQYl-vHU/edit#gid=1046975186", "lap_data")

tyres_boxplot <- function(data, range, variable) {
    switch(variable,
           "braketemps" = {
               data %>% tidyr::pivot_longer(cols = `Brake avg temp FL`:`Brake avg temp RR`, names_to = "Tyre", values_to = "value") %>%
                   ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
                   geom_boxplot()+
                   coord_cartesian(
                       ylim = range
                   )+
                   labs(x = "Stint", y = "Average Brake Temperature [°C]")+
                   stat_summary(
                       aes(label = round(..y.., 2)), 
                       fun = brakewear_fun, 
                       geom = "label", 
                       fill = "white")+
                   facet_wrap(vars(`Tyre`))+
                   theme_bw()            
           },
           "brakewear" = {
               brakewear_per_lap <- diff(data$`Brake pad level FL`, differences = 1)*-1
               data %>% tidyr::pivot_longer(cols = `Brake pad level FL`:`Brake pad level RR`, names_to = "Tyre", values_to = "value") %>%
                   ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
                   labs(x = "Stint", y = "Brake Pad Life [mm]")+
                   stat_summary(
                       aes(label = round(..y.., 4)), 
                       fun = brakewear_fun, 
                       geom = "label")+
                   facet_wrap(vars(`Tyre`))+
                   theme_bw()            
           }
    ) 
}

brakewear_fun <- function(y) {
    mean(diff(y, differences = 1)*-1)
}

plot <- tyres_boxplot(data, c(0,0.05), "brakewear")
plot

brakewear_fun(data$`Brake pad level FL`)
