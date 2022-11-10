# What this Script is for:
# Generate data from a Google Sheet file.

# Press CTRL+Shift+Enter to execute the whole script

# import standard packages
pacman::p_load(googlesheets4, ggplot2, tidyr, dplyr, tibble)

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

    for (i in 1:nrow(data)) {
      if (i == 1) {
        data$Stint[i] <- 1
        next
      }

      next_stint <- 0
      if (data$`Out lap?`[i] == "Yes") {
        next_stint <- 1
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

# laptimes_stintoverview <- function(data, range) {
#     ggplot(data, aes(`Driver`, `Lap time`, fill = `Driver`)) +
#         geom_boxplot(varwidth = TRUE) +
#         stat_summary(aes(label=round(..y..,2)), fun=median, geom="label", fill = "white")+
#         #scale_y_discrete()+
#         theme_bw()
#
# }

laptimes_stintoverview <- function(data) {
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
        labs(title = "Stint Overview", x = "Stint")+
        theme_bw()
}
