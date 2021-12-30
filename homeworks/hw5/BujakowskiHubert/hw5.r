library(dplyr)
library(plotly)

df <- read.csv('complete.csv')
df <- df %>% select('awardYear', 'category', 'gender') %>% 
  filter(awardYear %in% 1990:2018) %>%
  filter(gender != "")

df <- df %>% group_by(gender) %>% 
  group_by(category, awardYear, gender) %>% 
  summarise(nn = n())

df <- df %>% mutate(n_male = ifelse(gender == 'male', nn, 0), 
         n_female = ifelse(gender == 'female', nn, 0)) %>% 
  select(-gender, -nn)

fig <- df %>%
  plot_ly(
  x = ~category,
  y = ~n_male,
  frame = ~awardYear,
  name = 'male',
  type = 'bar',
  marker = list(color = "#3376FF")
) %>% add_trace(
  y = ~n_female,
  name = 'female',
  marker = list(color = "#FF69B4")
) %>% layout(
  legend = list(title = list(text = "<b> Gender </b>")),
  barmode = 'group',
  title = "Number of Laureats in different categories in 1990-2018",
  yaxis = list(title = "Number of Laureats",
               range = list(0, 4)),
  xaxis = list(title = "Category")
) %>% 
  animation_button(visible = FALSE) %>%
  animation_slider(currentvalue = list(prefix = "Award Year:"))

fig
