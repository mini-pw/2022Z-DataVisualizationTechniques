library(dplyr)
library(plotly)

df <- read.csv("data.csv")

df <- df %>% 
  select(category, birth_date, dateAwarded) %>% 
  filter(birth_date != "", dateAwarded != "") %>% 
  mutate(age = as.numeric(as.Date(as.character(dateAwarded), format="%Y-%m-%d") 
                          - as.Date(as.character(birth_date), format="%Y-%m-%d"))%/%365) %>% 
  filter(!is.na(age))

fig <- plot_ly() %>% 
  add_histogram(data = df, x = ~age, name = "Age", frame = ~category) %>% 
  layout(title = "Age distribution of Noble Prize laureates at the time of awarding by category",
         margin = list(l = 50, r = 50,
              b = 50, t = 50,
              pad = 20),
         showlegend = FALSE, 
         xaxis = list(title = 'Age when awarded'),
         yaxis = list(title = 'Number of awarded')) %>% 
  animation_button(label = "play") %>% 
  animation_slider(easing = "linear",
                   len = 0.8,
                   x = 0.1,
                   xnachor = "center",
                   transition = 1500,
                   currentvalue = list(x = 1, xanchor = "left", y = 0, yanchor = "bottom",
                                       font = list(color = "black", size = 12)
                   ))
fig <- config(fig, staticPlot = TRUE)
fig
