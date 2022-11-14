pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table)

source("accAnalyzer/GoogleSheetsAnalyzer.R")
data <- read_googlesheet("https://docs.google.com/spreadsheets/d/1XqUbDBPDTwRxQYsmTz2R0g07LAD0iQtyB50LQYl-vHU/edit#gid=1046975186", "lap_data")


# tyres_boxplot <- function(data, range, variable) {
#     switch(
#         variable,
#         "pressure" = {
#             data %>% pivot_longer(cols = `Avg tyre pressure FL`:`Avg tyre pressure RR`, names_to = "Tyre", values_to = "value") %>%
#                 ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
#                 geom_boxplot()+
#                 coord_cartesian(
#                     ylim = range
#                 )+
#                 labs(x = "Stint", y = "Average Tyre Pressure")+
#                 stat_summary(aes(label = round(..y.., 2)), fun = median, geom = "label", fill = "white")+
#                 facet_wrap(vars(`Tyre`))+
#                 theme_bw()
#         },
#         "temperature" = {
#             data %>% pivot_longer(cols = `Avg tyre temp FL`:`Avg tyre temp RR`, names_to = "Tyre", values_to = "value") %>%
#                 ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
#                 geom_boxplot()+
#                 coord_cartesian(
#                     ylim = range
#                 )+
#                 labs(x = "Stint", y = "Average Tyre Temperature")+
#                 stat_summary(aes(label = round(..y.., 2)), fun = median, geom = "label", fill = "white")+
#                 facet_wrap(vars(`Tyre`))+
#                 theme_bw()            
#         },
#         "braketemps" = {
#             data %>% pivot_longer(cols = `Brake avg temp FL`:`Brake avg temp RR`, names_to = "Tyre", values_to = "value") %>%
#                 ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
#                 geom_boxplot()+
#                 coord_cartesian(
#                     ylim = range
#                 )+
#                 labs(x = "Stint", y = "Average Brake Temperature")+
#                 stat_summary(aes(label = round(..y.., 2)), fun = median, geom = "label", fill = "white")+
#                 facet_wrap(vars(`Tyre`))+
#                 theme_bw()            
#         }
#     ) 
# }

tyres_boxplot(data, c(27.0, 28.0), "pressure")
tyres_boxplot(data, c(70, 110), "temperature")
tyres_boxplot(data, c(150, 300), "braketemps")



