library(plotly)
library(dplyr)
library(tidyverse)


df <- read.csv('data/data.csv')

df <- df[!is.na(df$gender) & df$gender != '', ]

most_nobles <- df %>% 
  mutate(
    decade = df$awardYear - df$awardYear %% 10
  ) %>% group_by(decade, birth_continent, category) %>% 
  summarise(count = n()) %>% 
  filter(count == max(count)) %>% 
  filter(row_number() == 1)
  select(-c('count'))

year_count <- df %>% 
  mutate(
    decade = df$awardYear - df$awardYear %% 10
  ) %>% 
  group_by(decade, birth_continent) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  complete(decade, birth_continent, fill = list(n = 0)) %>% 
  left_join(most_nobles, by = c('decade', 'birth_continent')) %>% 
  group_by(decade, birth_continent)
  

plot_ly(data = data.frame(year_count),
        text = paste0("Most popular category: ", year_count$category),
        hoverinfo = 'text') %>% 
  add_bars(x = ~birth_continent, y = ~n, frame = ~decade)  %>%
  layout(xaxis = list(title = 'Birth continent'), yaxis = list(title = 'Count'),
         title = "Noble prizes by birth continent of receiver") %>% 
  animation_slider(currentvalue = list(prefix = "Decade: ", font = list(color="black")))
