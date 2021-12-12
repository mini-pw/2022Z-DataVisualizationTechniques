library(plotly)
library(dplyr)

df <- read.csv(file = "complete.csv")

df1 <- df %>% select(c("category", "birth_continent"))

df2 <- df1 %>% 
      filter(birth_continent != "") %>% 
      group_by(birth_continent) %>% 
      summarise(n = n()) %>% 
      left_join(df1) %>% 
      group_by(birth_continent, category) %>% 
      summarise(n1 = n())

fig <- plot_ly(data = df2, x = ~birth_continent, y= ~n1, frame = ~category, type = "bar") %>% 
  layout(title = list(text = "Number of Nobel Prize laureates for each continent when awarded by category", y = 0.97),
         xaxis = list(title = "Birth continent"),
         yaxis = list(title = "Number of laureates"),
         showlegend = F) %>%
  animation_slider(len = 0.8, x = 0.5, xanchor = "center") %>%
  config(displayModeBar = FALSE)
fig
  