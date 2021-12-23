library(plotly)
library(dplyr)

nobel <- read.csv("complete.csv")  %>% arrange(awardYear) 

nobel2 <- nobel %>% select(awardYear, prizeAmount)

lata1 <- nobel %>% select(awardYear) %>% unique() 
lata <- lata1 %>% inner_join(nobel2, by = "awardYear") %>% unique()


fig <- plot_ly(lata, x = ~awardYear)
fig <- fig %>% add_lines(y = ~prizeAmount)
fig <- fig %>% rangeslider(fig)  %>% layout(
  title = "Wysokość nagrody nobla na przestrzeni lat",
  xaxis = list(title = "Rok otrzymania nagrody"),
  yaxis = list(title = "Wysokość nagrody"),
  updatemenus = list(
    list(active = -1,
         type = 'buttons',
      buttons = list(
        list(method = "restyle",
             args = list("line.color", "blue"),
             label = "Niebieski"),
        list(method = "restyle",
             args = list("line.color", "red"),
             label = "Czerwony"),
        list(method = "restyle",
             args = list("line.color", "pink"),
             label = "Różowy")
      ))))
fig
