library(plotly)
library(dplyr)
library(tidyr)

mixedToFloat <- function(x){
  x <- sub(' ', '+', x, fixed=TRUE)
  return(unlist(lapply(x, function(x) eval(parse(text=x)))))
}

setwd("./IAD/sem 3/techniki wizualizacji danych/ZD5")
NobelPrize <- read.csv("complete.csv")
NobelPrize$portion <- mixedToFloat(NobelPrize$portion)
head(NobelPrize)
MyNobel <- NobelPrize %>% 
  select(category, portion, id, birth_continent) %>% 
  group_by(category, birth_continent) %>% 
  summarise(val = sum(portion)) %>% 
  filter(birth_continent != "")

all <- MyNobel %>% 
  group_by(birth_continent) %>% 
  summarise(val = sum(val))

grouped <- pivot_wider(MyNobel, names_from = category, values_from = val)
grouped[is.na(grouped)] <- 0

#grouped <- grouped %>% pivot_longer(!birth_continent, names_to = "category", values_to = "val")

updatemenus = list(
  list(
    buttons = list(
      list(method = list("restyle"),
           args = list("y", list(all$val)),
           label = "All"),
      
      list(method = "restyle",
           args = list("y", list(grouped$Chemistry)),
           label = "Chemistry"),
      list(method = "restyle",
           args = list("y", list(grouped$`Economic Sciences`)),
           label = "Economic Sciences"),
      list(method = "restyle",
           args = list("y", list(grouped$Literature)),
           label = "Literature"),
      list(method = "restyle",
           args = list("y", list(grouped$Peace)),
           label = "Peace"),
      list(method = "restyle",
           args = list("y", list(grouped$Physics)),
           label = "Physics"),
      list(method = "restyle",
           args = list("y", list(grouped$`Physiology or Medicine`)),
           label = "Physiology or Medicine"))))


fig <- plot_ly(data = all, x = ~birth_continent, y = ~val, type = 'bar') %>% 
  layout(
  title = "Nobel Prize winners by continent",
  yaxis = list(title ="Number of winners"),
  xaxis = list(title =""),
  updatemenus = updatemenus)
fig

