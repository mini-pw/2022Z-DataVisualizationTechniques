library(plotly)
library(dplyr)

options(warn=-1)

nobel <- read.csv("nobel.csv") %>% 
  mutate(birth_year = as.numeric(format(as.Date(birth_date, format="%Y-%m-%d"),"%Y"))) %>% 
  filter(!is.na(birth_year)) %>% mutate(age = awardYear - birth_year) %>%
  mutate(decade = awardYear - (awardYear %% 10))

  fig <- plot_ly(
    data = nobel,
    x = ~category, 
    y = ~age,
    color = ~birth_continent,
    colors = c("Set1"),
    frame = ~decade,
    text = paste0("Name: ", nobel$knownName, " Country: ", nobel$birth_country, nobel$awardYear),
    hoverinfo = 'x+y+text',
    type = 'scatter',
    mode = "markers", 
    showlegend = TRUE) %>%
    layout(
      xaxis = list(title = 'Category'), 
      yaxis = list(title = "Nobelist's age"))
  
  fig %>% 
    animation_opts(5555, easing = "elastic") %>%
    animation_button(x = 0.05, y = 0.05) %>%
    animation_slider(currentvalue = list(prefix = "DECADE: ", font = list(color="firebrick")))
