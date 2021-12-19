library(plotly)
library(dplyr)
library(tidyr)

nobel_data <- read.csv("complete.csv")

nobel_data$awardYear <- (nobel_data$awardYear%/%10)*10

top_countries <- 
  nobel_data %>% 
  select("birth_country") %>% 
  filter(birth_country != "") %>% 
  group_by(birth_country) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n >= 13)

df <- nobel_data %>% 
  select("awardYear", "birth_country") %>% 
  filter(birth_country %in% top_countries$birth_country) %>% 
  group_by(awardYear, birth_country) %>% 
  count()
  
colnames(df) <- c("awardDecade", "countryName", "numberOfAwards")

fig <- plot_ly(data = df, x = ~countryName, y = ~numberOfAwards, frame = ~awardDecade, type = 'bar')
fig %>% 
  animation_opts(1000, easing = "elastic", redraw = TRUE) %>%
  animation_slider(
    currentvalue = list(prefix = "Decade: ", font = list(color="black"))
  ) %>% 
  layout(title = list(text = 'Top 15 Noble prize winning countries', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'), 
         xaxis = list(title = 'Country name'), 
         yaxis = list(title = 'Number of awards won in a specific decade', fixedrange = TRUE))
