# What this Script is for:
# Generate Lap History Plots from a Motec exported csv file.
# The Motec data is exported from the "Lap History" tab (in the Manic_Driver [Data] 
# group) within the ACC_Kwitsch_Fabian workbook

# Press CTRL+Shift+Enter to execute the whole script 

# clear global environment
rm(list = ls())

#import standard packages
pacman::p_load(Hmisc, plyr, pacman, ggpubr, gridExtra, tidyquant, magrittr, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, markdown, shiny, stringr, tidyr, tidyverse, zoo)

# Import CSV
# motec_data <- import("C:\Users\fabi1\Downloads\Channel Report - Laps.csv", header = FALSE)
motec_data <- import(file.choose(), header = FALSE)

motec_data_t <- motec_data %>% 
  t() %>%
  as.data.frame()

# name rows in column 1
motec_data_t$'1'[c(1:6)] <- c("Driver/Setup","Date", "Car", "Track", "Lap #", "Lap Time")

# merging first two columns
motec_data_t$'1' <-replace_na(motec_data_t$'1', "")
motec_data_t$'2' <-replace_na(motec_data_t$'2', "")
motec_data_t$'1' <- paste(motec_data_t$'1', motec_data_t$'2')

# transpose
motec_data_n <- t(motec_data_t)
motec_data_n <- as.data.frame(motec_data_n)

# take first row as header
names(motec_data_n) <- motec_data_n[1,]
motec_data_n <- motec_data_n[-1,]
motec_data_n <- motec_data_n[-1,]

# remove 'Lap ' from Lap # column
str_remove(c(motec_data_n$`Lap # `), ".+[Lap ]")
motec_data_n$'Lap # ' <- str_remove(c(motec_data_n$`Lap # `), ".+[Lap ]")

# change datatype of some columns
motec_data_n[,1] <- as.factor(motec_data_n[,1])
motec_data_n[,5] <- as.numeric(motec_data_n[,5])
motec_data_n[,6] <- as.numeric(motec_data_n[,6])
motec_data_n[,7] <- as.numeric(motec_data_n[,7])
motec_data_n[,8] <- as.numeric(motec_data_n[,8])
motec_data_n[,9] <- as.numeric(motec_data_n[,9])
motec_data_n[,10] <- as.numeric(motec_data_n[,10])
motec_data_n[,11] <- as.numeric(motec_data_n[,11])
motec_data_n[,12] %<>% as.numeric()
motec_data_n[,13] %<>% as.numeric()
motec_data_n[,14] %<>% as.numeric()
motec_data_n[,15] %<>% as.numeric()
motec_data_n[,16] %<>% as.numeric()
motec_data_n[,17] %<>% as.numeric()
motec_data_n[,17] %<>% as.numeric()
motec_data_n[,18] %<>% as.numeric()
motec_data_n[,19] %<>% as.numeric()
motec_data_n[,20] %<>% as.numeric()



# Lap Time vs Driver/Setup
ggplot(motec_data_n, aes(`Driver/Setup `, `Lap Time `, fill = `Driver/Setup `)) +
  # geom_text(aes(label = `Lap Time `), stat = )+
  geom_boxplot() +
  stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+
  labs(title = )
  theme_bw()

# Lap Times over stint: linear regression
ggplot(motec_data_n, aes(`Lap # `, `Lap Time `, color = `Driver/Setup `))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  #geom_text(se = FALSE)+
  labs(x = "Lap Number", y="Lap Time [s]", title = "Lap Times over a full stint")+
  theme_bw()

# Lap Times over stint: Moving average
ggplot(motec_data_n, aes(`Lap # `, `Lap Time `, color = `Driver/Setup `))+
  geom_point()+
  geom_ma(ma_fun = SMA, n = 4, linetype = 1, size = 1.5)+
  labs(x = "Lap Number", y="Lap Time [s]", title = "Lap Times over a full stint")+
  theme_bw()

# Understeer over stint
ggplot(motec_data_n, aes(`Lap # `, `Understeer Angle [°] Avg`, color = `Driver/Setup `))+
  geom_point()+
  geom_smooth(se = FALSE, method = lm)+
  labs(x = "Lap Number", y="Undesteer Angle [?]")+
  #ylim(NA, NA)+
  theme_bw()

# Understeer vs laptime
ggplot(motec_data_n, aes(`Understeer Angle [°] Avg`, `Lap Time ` , color = `Driver/Setup `)) +
  geom_point()+
  geom_smooth(se = FALSE, method = lm)+
  labs(x = "Understeer Angle [?]", y="Lap Time [s]")+
  #xlim(1, 1.3)+
  facet_grid(~`Driver/Setup `)+
  theme_bw()

# Steerangle over stint
ggplot(motec_data_n, aes(`Lap # `, `Steerangle abs [°] Avg`, color = `Driver/Setup `))+
  geom_point()+
  geom_smooth(se = FALSE, method = lm)+
  labs(x = "Lap Number", y="Steerangle abs [?] Avg", title = "Average Steering Angle over a full stint")+
  ylim(15, 35)+
  theme_bw()

# Tyre Temps FL over stint
ggplot(motec_data_n, aes(`Lap # `, `TYRE_TAIR_LF [°C] Avg`, color = `Driver/Setup `))+
  geom_point()+
  geom_smooth(se = FALSE, method = lm)+
  ylim(75, 100)+
  labs(x = "Lap Number", y="Steerangle abs [?] Avg")+
  #ylim(NA, NA)+
  theme_bw()

# Tyre Temps FR over stint
ggplot(motec_data_n, aes(`Lap # `, `TYRE_TAIR_RF [°C] Avg`, color = `Driver/Setup `))+
  geom_point()+
  geom_smooth(se = FALSE, method = lm)+
  ylim(75, 100)+
  labs(x = "Lap Number", y="Steerangle abs [?] Avg")+
  #ylim(NA, NA)+
  theme_bw()


# TODO
# - in the graph show the number of datapoints per driver
