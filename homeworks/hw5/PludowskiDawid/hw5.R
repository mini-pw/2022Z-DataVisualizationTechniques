library(plotly)
library(dplyr)


df <- read.csv2("./../data/complete.csv", sep = ",", header = T)
df <- df %>% select(category, prizeAmount, birth_continent) %>% 
  filter(birth_continent != "")

t1 = list(
  size = 25
)

m <- list(
  l = 75,
  r = 75,
  b = 100,
  t = 100,
  pad = 4
)

fig <- plot_ly(
  data = df,
  x = ~category,
  y = ~prizeAmount,
  frame = ~birth_continent,
  color = ~birth_continent,
  colors = c("black", "#cccc00", "blue", "red", "darkgreen", "orange"),
  type = "box"
) %>% layout(
  autosize = T,
  margin = m,
  xaxis = list(title = "Categories"),
  yaxis = list(title = "Prize amount in dollars"),
  title = list(text = "Prize distribution", font = t1)
)  %>% 
  animation_opts(frame = 1000) %>% 
  animation_slider(currentvalue = list(
      prefix = "<b>Country of origin:</b> ",
      font = list(color = "#666666"))
    )

fig
