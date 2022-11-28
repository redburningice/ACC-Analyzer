# this script is deprecated

# clear global environment
rm(list = ls())

#import standard packages
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, markdown, shiny, stringr, tidyr, tidyverse)

# Import CSV
motec_data <- import("D:/Downloads/Channel Report2 - Laps.csv", header = FALSE)

# motec_data2 <- import(file.choose())

motec_data_t <- motec_data %>% 
  t() %>%
  as.data.frame()

# name rows in column 1
motec_data_t$'1'[c(1:7)] <- c("Driver/Setup","Date", "Car", "Track", "Lap #", "Lap Time", "Section Name")



# making column names valid
# colnames(motec_data_t) <- make.names(colnames(motec_data_t), unique=TRUE)

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
motec_data_n[,5] <- as.numeric(motec_data_n[,5])
motec_data_n[,6] <- as.numeric(motec_data_n[,6])
motec_data_n[,8] <- as.numeric(motec_data_n[,8])
motec_data_n[,9] <- as.numeric(motec_data_n[,9])
motec_data_n[,10] <- as.numeric(motec_data_n[,10])
motec_data_n[,11] <- as.numeric(motec_data_n[,11])
motec_data_n[,1] <- as.factor(motec_data_n[,1])
motec_data_n[,7] <- as.factor(motec_data_n[,7])

par(bg = 'gray')

plot(motec_data_n$`Driver/Setup `[motec_data_n$`Section Name ` == "Spoon"], 
     motec_data_n$`Calc Outing Time [s] Range`[motec_data_n$`Section Name ` == "Spoon"],
     'ylim = c(19.8,20.5)')


ggplot(motec_data_n, aes(`Driver/Setup `, `Calc Outing Time [s] Range`, fill = `Driver/Setup `))+
  geom_boxplot()+
  facet_wrap(~`Section Name `, scales = "free")+
  labs(y="Section Time [s]", title = "Section Times by Nearly on Pace drivers in a full stint practice session", x = "")+
  theme_bw()


