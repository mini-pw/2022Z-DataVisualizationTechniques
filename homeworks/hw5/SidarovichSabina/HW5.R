library(readr)
library(dplyr)
library(plotly)
library(DescTools)

#adding the column with the zodiac sign for every laureate
complete <- read_csv("complete.csv")
complete["zodiac"] <- Zodiac(complete$birth_date)

#creationg the plot
astro_plot <- complete %>%
  plot_ly(
    x = ~zodiac, 
    color = ~gender,
    colors = c("pink", "dodgerblue3"),
    frame = ~category,
    hoverinfo = "y",
    type = "histogram"
  ) %>%
  layout(
    title = "Zodiac signs of Nobel Prize Laureats",
    xaxis = list(title = "Zodiac sign"),
    yaxis = list(title = "Number of Laureats",
                 range = list(0,35)),
    barmode="stack",
    legend=list(title=list(text='<b> Sex </b>'))) %>%
  animation_opts(
          easing = "elastic"
  ) %>% 
  animation_button(visible = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "Category:")) %>%
  config(displayModeBar = FALSE)
astro_plot