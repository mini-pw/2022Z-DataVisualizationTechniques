library(plotly)
library(dplyr)

df_raw <- read.csv2("data/data.csv", sep = ",", header = T)

draw_box_prizes_over_continents <- function(continent, decade_){
  
  df <- df_raw %>% 
    filter(birth_continent %in% continent) %>%
    mutate(
      decade = awardYear - awardYear %% 10
    ) %>% 
    filter(decade == decade_)
  
  t1 = list(
    size = 25
  )
  
  palette = list(
    Africa = "black",
    Asia = "#cccc00",
    Europe = "blue",
    "North America" = "red",
    Australia = "darkgreen",
    "South America" = "orange")
  
  print(unlist(palette[continent]))
  my_colors <- unlist(palette[continent])
  
  m <- list(
    l = 75,
    r = 75,
    b = 100,
    t = 100,
    pad = 4
  )
  
  continents_name <- paste(continent, collapse = ", ")

  plot_ly(
    data = df,
    x = ~category,
    y = ~prizeAmount,
    color = ~birth_continent,
    colors = my_colors,
    type = "box"
  ) %>% layout(
    boxmode = "group",
    autosize = T,
    margin = m,
    xaxis = list(title = "Categories"),
    yaxis = list(title = "Prize amount in dollars"),
    title = list(text = paste("Prize distribution in\n", continents_name), font = t1)
  ) %>% 
    config(displayModeBar = FALSE)
}
