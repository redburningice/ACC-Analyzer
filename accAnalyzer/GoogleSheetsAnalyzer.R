# What this Script is for:
# Generate data from a Google Sheet file.

# import standard packages
pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble, data.table)

read_googlesheet <- function(url, sheetname) {
  gs4_deauth()


  # Sheet = lap_data
  if (sheetname == "lap_data") {
    data <- googlesheets4::read_sheet(url, sheet = sheetname)
    # Data formatting
    data$`Sector 1` <- data$`Sector 1` / 1000
    data$`Sector 2` <- data$`Sector 2` / 1000
    data$`Sector 3` <- data$`Sector 3` / 1000
    data$`Lap time` <- data$`Lap time` / 1000

    for (i in 1:ncol(data)) {
      if (is.numeric(data[, i])) {
        data[, i] <- format(round(data[, i], 3), nsmall = 3)
      }
    }

    data <- add_column(data, Stint = NA, .before = "Lap")
    data <- add_column(data, Stintlap = NA, .after = "Stint")

    for (i in 1:nrow(data)) {
        
        #add Stint and Stintlap columns
      if (i == 1) {
        data$Stint[i] <- 1
        data$Stintlap[i] <- 1
        next
      }
      next_stint <- 0
      if (data$`Out lap?`[i] == "Yes") {
        next_stint <- 1
        data$`Stintlap`[i] <- 1
      } else {
          data$Stintlap[i] <- data$Stintlap[i - 1] + 1
      }
      
      data$Stint[i] <- data$Stint[i - 1] + next_stint
    }
  }

  # Sheet = Stint Overview
  if (sheetname == "Stint overview") {
    data <- googlesheets4::read_sheet(url, sheet = sheetname, skip = 1)
    data <- na.omit(data)
    data <- data[-c(5, 7:12, 15:30)]

    data <- data %>% rename(
      "Outlap" = "Outlap...2",
      "Inlap" = "Inlap...3"
    )
  }


  return(data)
}

google_plot <- function(data) {
  ggplot(data, aes(x = `Lap time`, fill = `Driver`)) +
    geom_boxplot() +
    scale_x_discrete()
}

stint_overview_boxplot <- function(data) {
    laptimes_sorted_filtered <- data %>% dplyr::filter(`Out lap?` == "No", `Lap` != 1, `In lap?` == "No") %>% pull(`Lap time`) %>% sort()
    ggplot(data, aes(as.factor(`Stint`), `Lap time`, fill = `Driver`)) +
        geom_boxplot(show.legend = TRUE) +
        stat_summary(aes(label = round(..y.., 2)), fun = median, geom = "label", fill = "white") +
        coord_cartesian(
            ylim = c(
                min(data$`Lap time`),
                laptimes_sorted_filtered[length(laptimes_sorted_filtered)*0.98]
            )
        )+
        labs(x = "Stint")+
        theme_bw()
}

stint_overview_linechart <- function(data) {
    laptimes_sorted_filtered <- data %>% dplyr::filter(`Out lap?` == "No", `Lap` != 1, `In lap?` == "No") %>% pull(`Lap time`) %>% sort()
    
    data %>%
        dplyr::filter(`Out lap?` == "No", `Lap` != 1, `In lap?` == "No") %>%
        ggplot(aes(`Stintlap`, `Lap time`, colour = `Driver`))+
        geom_line(size = 1)+
        coord_cartesian(
            ylim = c(
                min(data$`Lap time`),
                laptimes_sorted_filtered[length(laptimes_sorted_filtered)*0.98]
            )
        )+
        facet_grid(rows = vars(`Stint`), shrink = TRUE)+
        #stat_summary(aes(label = "a", x = x, y = ..y..), geom = "label", fill = "white")+
        labs(x = "Stint Lap")+
        theme_bw()
}

tyres_boxplot <- function(data, range, variable) {
    switch(
        variable,
        "pressure" = {
            data %>% tidyr::pivot_longer(cols = `Avg tyre pressure FL`:`Avg tyre pressure RR`, names_to = "Tyre", values_to = "value") %>%
                ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
                geom_boxplot()+
                coord_cartesian(
                    ylim = range
                )+
                labs(x = "Stint", y = "Average Tyre Pressure [PSI]")+
                stat_summary(aes(label = round(..y.., 2)), fun = median, geom = "label", fill = "white")+
                facet_wrap(vars(`Tyre`))+
                theme_bw()
        },
        "temperature" = {
            data %>% tidyr::pivot_longer(cols = `Avg tyre temp FL`:`Avg tyre temp RR`, names_to = "Tyre", values_to = "value") %>%
                ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
                geom_boxplot()+
                coord_cartesian(
                    ylim = range
                )+
                labs(x = "Stint", y = "Average Tyre Temperature [°C]")+
                stat_summary(aes(label = round(..y.., 2)), fun = median, geom = "label", fill = "white")+
                facet_wrap(vars(`Tyre`))+
                theme_bw()            
        },
        "braketemps" = {
            data %>% tidyr::pivot_longer(cols = `Brake avg temp FL`:`Brake avg temp RR`, names_to = "Tyre", values_to = "value") %>%
                ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
                geom_boxplot()+
                coord_cartesian(
                    ylim = range
                )+
                labs(x = "Stint", y = "Average Brake Temperature [°C]")+
                stat_summary(aes(label = round(..y.., 2)), fun = median, geom = "label", fill = "white")+
                facet_wrap(vars(`Tyre`))+
                theme_bw()            
        },
        "brakewear" = {
            brakewear_per_lap <- c(
                `Brake pad level FL` = "FL",
                `Brake pad level FL` = "FR",
                `Brake pad level FL` = "RL",
                `Brake pad level FL` = "RR"
            )
            data %>% tidyr::pivot_longer(cols = `Brake pad level FL`:`Brake pad level RR`, names_to = "Tyre", values_to = "value") %>%
                ggplot(aes(as.factor(`Stint`), `value`, fill = `Driver`))+
                labs(x = "Stint", y = "Brake Pad Wear per Lap [mm]")+
                stat_summary(
                    aes(label = round(..y.., 4)), 
                    fun = brakewear_fun, 
                    geom = "label")+
                facet_wrap(vars(`Tyre`))+
                facet_wrap(vars(`Tyre`), labeller = as_labeller(brakewear_per_lap))+
                theme_bw()            
        }
    ) 
}

tyres_linechart <- function(data, range, variable) {
    pitlaps <- data %>% dplyr::filter(`In lap?` == "Yes") %>% pull(`Lap`)
    switch(
        variable,
        "pressure" = {
            data %>% tidyr::pivot_longer(cols = `Avg tyre pressure FL`:`Avg tyre pressure RR`, names_to = "Tyre", values_to = "value") %>%
                ggplot(aes(x = `Lap`,y = `value`, colour = `Driver`))+
                geom_path(aes(group = 1))+
                coord_cartesian(
                    ylim = range
                )+
                labs(x = "Laps", y = "Average Tyre Pressure [PSI]")+
                geom_vline(xintercept = pitlaps)+
                facet_wrap(vars(`Tyre`))+
                theme_bw()
        },
        "temperature" = {
            data %>% tidyr::pivot_longer(cols = `Avg tyre temp FL`:`Avg tyre temp RR`, names_to = "Tyre", values_to = "value") %>%
                ggplot(aes(x = `Lap`,y = `value`, colour = `Driver`))+
                geom_path(aes(group = 1))+
                coord_cartesian(
                    ylim = range
                )+
                labs(x = "Laps", y = "Average Tyre Temp [°C]")+
                geom_vline(xintercept = pitlaps)+
                facet_wrap(vars(`Tyre`))+
                theme_bw()           
        },
        "braketemps" = {
            data %>% tidyr::pivot_longer(cols = `Brake avg temp FL`:`Brake avg temp RR`, names_to = "Tyre", values_to = "value") %>%
                ggplot(aes(x = `Lap`,y = `value`, colour = `Driver`))+
                geom_path(aes(group = 1))+
                coord_cartesian(
                    ylim = range
                )+
                labs(x = "Laps", y = "Average Brake Temperature [°C]")+
                geom_vline(xintercept = pitlaps)+
                facet_wrap(vars(`Tyre`))+
                theme_bw()       
        },
        "brakewear" = {
            data <- data %>% tidyr::pivot_longer(cols = `Brake pad level FL`:`Brake pad level RR`, names_to = "Tyre", values_to = "value")
            data %>% ggplot(aes(x = `Lap`,y = `value`, colour = `Driver`))+
                geom_path(aes(group = 1))+
                labs(x = "Laps", y = "Brake Pad Level [mm]")+
                geom_vline(xintercept = pitlaps)+
                facet_wrap(vars(`Tyre`))+
                theme_bw()      
        }
    ) 
}

brakewear_fun <- function(y) {
    mean(diff(y, differences = 1)*-1)
}

weather_linechart <- function(data) {
    pitlaps <- data %>% dplyr::filter(`In lap?` == "Yes") %>% pull(`Lap`)
    ggplot(data, aes(x = `Lap`))+
        geom_line(aes(y = `Air temp`), colour = "blue", size = 1)+
        geom_line(aes(y = `Track temp`), colour = "red", size = 1)+
        geom_vline(xintercept = pitlaps)+
        labs(x = "Laps", y = "Temperature [°C]")+
        theme_bw()
}

fuel_boxplot <- function(data) {
    data %>% dplyr::filter(`Out lap?` == "No", `In lap?` == "No") %>%
        ggplot(aes(x = as.factor(`Stint`), y = `Fuel consumption avg`, fill = `Driver`))+
        geom_boxplot()+
        labs(x = "Stint", y = "Avg Fuel Consumption per Lap [l/lap]")+
        stat_summary(
            aes(label = round(..y.., 2)), 
            fun = median, 
            geom = "label", fill = "white")+
        theme_bw()
}

fuel_linechart <- function(data) {
    pitlaps <- data %>% dplyr::filter(`In lap?` == "Yes") %>% pull(`Lap`)
    data <- data %>% dplyr::filter(`Out lap?` == "No", `In lap?` == "No")
    ggplot(data, aes(x = `Lap`))+
        geom_path(aes(y = `Fuel consumption avg`, group = 1, colour = `Driver`), size = 1)+
        geom_vline(xintercept = pitlaps)+
        labs(x = "Laps", y = "Avg Fuel Consumption [l]")+
        theme_bw()
}


