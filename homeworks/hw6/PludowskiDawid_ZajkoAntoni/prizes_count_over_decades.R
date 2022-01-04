library(plotly)
library(dplyr)
library(tidyverse)


df <- read.csv('data/data.csv')

df <- df[!is.na(df$gender) & df$gender != '', ]

draw_prizes_count_over_decades <- function(decade_){
  
  t1 = list(
    size = 25
  )
  
  m <- list(
    l = 75,
    r = 75,
    b = 0,
    t = 100,
    pad = 4
  )

  most_nobles <- df %>% 
    mutate(
      decade = df$awardYear - df$awardYear %% 10
    ) %>% group_by(decade, birth_continent, category) %>% 
    summarise(count = n()) %>% 
    filter(count == max(count)) %>% 
    filter(row_number() == 1) %>% 
    select(-c('count')) %>% 
    filter(decade == decade_)

  year_count <- df %>% 
    mutate(
      decade = df$awardYear - df$awardYear %% 10
    ) %>% 
    group_by(decade, birth_continent) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    complete(decade, birth_continent, fill = list(n = 0)) %>% 
    left_join(most_nobles, by = c('decade', 'birth_continent')) %>% 
    group_by(decade, birth_continent) %>% 
    filter(decade == decade_)


  plot_ly(data = data.frame(year_count),
          text = paste0("Most popular category: ", year_count$category),
          hoverinfo = 'text') %>% 
    add_bars(
      x = ~birth_continent,
      y = ~n,
      color = ~birth_continent,
      colors = c("black", "#cccc00", "blue", "red", "darkgreen", "orange"),
      opacity = 0.7)  %>%
    layout(xaxis = list(title = list(text = "Birth continent",
                                     standoff = 30L)),
           margin = m,
           title = list(
             text = paste("Noble prizes by birth continent of receiver in", decade_),
             font = t1),
           yaxis = list(title = 'Count', range = c(0,65)),
           showlegend = FALSE) %>% 
    config(displayModeBar = FALSE)

}
