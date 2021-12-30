library(plotly)
library(dplyr)

df <- read.csv("complete.csv")

plot_ly(data = df, x = ~category, y = ~awardYear, type = 'scatter', frame = ~birth_country, mode = 'markers', hoverinfo = 'text', text = ~paste('</br> Name: ', name,
                                                                                                                                                '</br> Year: ', awardYear,
                                                                                                                                    '</br> Prize amount: ', prizeAmount)) %>%
  layout(title = "Kraje pochodzenia noblistow") %>%
  animation_opts(3000, redraw = FALSE) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Birth country: ", font = list(color="black")))

