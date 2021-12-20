library(plotly)
library(dplyr)
library(tidyverse)
library(data.table) 
d <- read.csv("./HW5/complete.csv")

d %>% 
  select(awardYear, gender) %>%
  filter(awardYear >= 1980) %>%
  group_by(awardYear,gender) %>% 
  summarise(n = n()) %>%
  spread(gender, n) -> d1

plot_ly(
    data = d1,
    x = ~awardYear, 
    y = ~male, 
    #color = '#F75B95',
    type = 'bar',
    colors = "Set1",
    name = 'male',
    #hoverinfo = 'x+y'
    hovertemplate = paste('<br><b>Year</b>: %{x}','<br><b>n</b>: %{y}')
) %>% 
  add_trace(y = ~female,
            #color = '#01B0F0' ,
            name = 'female') %>% 
  add_trace(y = ~V1,
            #color = '#000000',
            name = 'other') %>% 
  layout(barmode = 'stack',
         title = "Number of Nobel prizes, from 1980-2019, by gender",
         xaxis = list(title = 'Number of prizes'),
         yaxis = list(title = 'Year'))-> fig
fig

 
library(plotly)

data.table::melt(d1, id.vars='awardYear') %>% View()
  plot_ly(x = ~awardYear, y = ~value, type = 'bar', 
          name = ~variable, color = ~variable) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

