# What this Script is for:
# Generate Section History Plots from a Motec exported csv file.
# The Motec data is exported from the "Section History" tab (in the Manic_Driver [Data] 
# group) within the ACC_Kwitsch_Fabian workbook

# Press CTRL+Shift+Enter to execute the whole script 

# clear global environment
rm(list = ls())

#import standard packages
pacman::p_load(Hmisc, plyr, pacman, ggpubr, gridExtra, tidyquant, magrittr, dplyr, GGally, ggplot2, ggthemes, ggvis, 
               httr, lubridate, plotly, rio, markdown, shiny, stringr, tidyr, tidyverse, zoo, ggforce)

# Import CSV
# motec_data <- import("D:/Downloads/Channel Report3 - Laps.csv", header = FALSE)
motec_data <- import(file.choose(), header = FALSE)

motec_data_t <- motec_data %>% 
  t() %>%
  as.data.frame()

# name rows in column 1
motec_data_t$'1'[c(1:7)] <- c("Driver/Setup","Date", "Car", "Track", "Lap #", "Lap Time", "Section Name")

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
motec_data_n[,7] <- as.factor(motec_data_n[,7])
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



# Section Times
ggplot(motec_data_n, aes(`Driver/Setup `, `Calc Outing Time [s] Range`, fill = `Driver/Setup `))+
  geom_boxplot()+
  stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+  
  facet_wrap(~`Section Name `, scales = "free")+
  labs(y="Section Time [s]", title = "Section Times per Driver", x = "")+
  theme_bw()

# Inverse Braking Point (higher number = later braking point)
ggplot(motec_data_n, aes(`Driver/Setup `, `Brake Point [m] Range`, fill = `Driver/Setup `))+
  geom_boxplot()+
  stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+  
  facet_wrap(~`Section Name `, scales = "free")+
  labs(y="Inverse Braking Point [m]", title = "Inverse Braking Points", x = "")+
  coord_flip()+
  theme_bw()

# Braking Point vs Section Time
section_name <- "09 Bit"
section <- motec_data_n$`Section Name ` == section_name
ggplot(motec_data_n[section,], aes(`Brake Point [m] Range`, `Calc Outing Time [s] Range`, colour = `Driver/Setup `))+
  geom_point()+
  #stat_density_2d_filled(alpha = 0.6)+
  geom_mark_ellipse(size = 2, expand = 0)+
  #geom_smooth(se = F)+
  geom_text(aes(label = `Lap # `), nudge_y = 0.01)+
  labs(x="Inverse Braking Point [m]", y = "Section Time [s]", title = section_name, x = "")+
  theme_bw()

# Understeer Angle avg
ggplot(motec_data_n, aes(`Driver/Setup `, `Understeer Angle [°] Avg`, fill = `Driver/Setup `))+
  geom_boxplot()+
  stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+  
  facet_wrap(~`Section Name `, scales = "free")+
  labs(y="Understeer Angle [?]", title = "Average Understeer Angle", x = "")+
  theme_bw()

# Understeer Angle min (oversteer)
ggplot(motec_data_n, aes(`Driver/Setup `, `Understeer Angle [°] Min`, fill = `Driver/Setup `))+
  geom_boxplot()+
  stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+  
  facet_wrap(~`Section Name `, scales = "free")+
  labs(y="Understeer Angle [?]", title = "Min Understeer Angle (oversteer)", x = "")+
  theme_bw()

# Understeer Angle max (understeer)
ggplot(motec_data_n, aes(`Driver/Setup `, `Understeer Angle [°] Max`, fill = `Driver/Setup `))+
  geom_boxplot()+
  stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+  
  facet_wrap(~`Section Name `, scales = "free")+
  labs(y="Understeer Angle [?]", title = "Max Understeer Angle (understeer)", x = "")+
  theme_bw()

# Max Steering Angle
ggplot(motec_data_n, aes(`Driver/Setup `, `Steerangle abs [°] Max`, fill = `Driver/Setup `))+
  geom_boxplot()+
  stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+  
  facet_wrap(~`Section Name `, scales = "free")+
  labs(y="Max Steering Angle", title = "Max Steering Angle", x = "")+
  theme_bw()

# Error-Corrected-Laptime over a stint

  # create temp table for error-correction logic
  temp <- motec_data_n
  sectionNr <- max(as.numeric(unique(temp$`Section Name `)))
  temp$`lastLapSection` = temp$`Calc Outing Time [s] Range`
  temp <- temp %>% dplyr::relocate(lastLapSection, .after = `Calc Outing Time [s] Range`)
  temp$lastLapSection <- dplyr::lag(temp$lastLapSection, n = sectionNr)
  temp$bestSection = pmin(temp$lastLapSection, temp$`Calc Outing Time [s] Range`)
  temp$bestSection[is.na(temp$bestSection)] <- temp$`Calc Outing Time [s] Range`

e <- temp %>% dplyr::group_by(`Lap # `) %>% 
  dplyr::summarise(laptime = sum(`Calc Outing Time [s] Range`), errorCorrectedLaptime = sum(bestSection))

ggplot(e, aes(`Lap # `, `laptime`))+
  geom_point()+
  geom_ma(data = e, ma_fun = SMA, n = 5, linetype = 1, size = 1.5)+
  geom_ma(data = e, mapping = aes(`Lap # `, `errorCorrectedLaptime`), ma_fun = SMA, n = 5, linetype = 1, color = "green", size = 1)+
  labs(x = "Lap Number", y="Lap Time [s]", title = "Error-corrected Times over a full stint", 
       subtitle = "only counts the fastest section of two consecutive laps, which can help to 
       understand tyre wear impact a little bit better")+
  theme_bw()


# TODO
# - in the graph show the number of datapoints per driver
