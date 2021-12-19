library(dplyr)
library(ggplot2)
library(plotly)

data <- read.csv("complete.csv")

#####################################################

data <- data %>% 
  filter(birth_continent!="") %>% 
  mutate(sex = ifelse(gender=="male", 10, ifelse(gender=="female", 15, 0)))



  
fig = plot_ly(width = 800) %>% 
  add_trace(data = data,
            y = ~category,
            x = ~awardYear,
            color = ~gender,
            type = 'scatter',
            mode = 'markers',
            opacity = 1,
            size = ~sex,
            text = paste0(data$name," ",data$awardYear),
            frame = ~birth_continent,
            hoverinfo = 'text')


config(fig, responsive = FALSE) %>% 
  layout(yaxis = list(title = ''), 
         xaxis = list(title = 'Year'), 
         title = "Nobel prize winners across the years"
  ) %>% 
  animation_opts(transition = 1000,
                 frame = 2000, 
                 mode = 'immediate') %>%
  animation_slider(currentvalue = list(prefix = "Continent: ", font = list(color="black"))) 












