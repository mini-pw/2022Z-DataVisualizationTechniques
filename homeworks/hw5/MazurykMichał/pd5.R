library('plotly')
library('dplyr')
library('forcats')

df = read.csv("complete.csv")

df1 <-df %>% 
  select(awardYear,category,givenName,birth_continent,birth_countryNow,gender)%>% 
  group_by(birth_countryNow,category) %>%
  summarise(n=n()) %>%
  filter(birth_countryNow %in% c('USA',"United Kingdom","Germany",'France','Poland')) %>%
  ungroup() %>% 
  mutate(birth_countryNow = fct_reorder(birth_countryNow, n, .desc = TRUE))
plot_ly(
  data = df1, 
  x = ~birth_countryNow, 
  y = ~n,
  color = I("gold"),
  frame = ~category,
  type = "bar") %>%
  layout(
    title = "The most awarded countries",
  xaxis = list(title = ""),
  yaxis = list(title = "Number of awards"),
  showlegend = F ) %>%
  config(displayModeBar = FALSE)%>% 
  animation_opts(800) %>%
  animation_button(x = 0.05, y = 0.1) %>%
  animation_slider(x = 0.001, y = 0.01,currentvalue = list(prefix = "Category: ", font = list(color="gold")))


  

