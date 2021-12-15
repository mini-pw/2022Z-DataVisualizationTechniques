library(plotly)
library(dplyr)

Nobels <- read.csv("complete.csv")


Nobels <- Nobels%>%
  filter(awardYear >= 2010)%>%
  group_by(category,sortOrder)%>%
  summarize(liczba = n())



steps <-list(
      list(method = "restyle",
           args = list("type", "bar"),
           label = "bar"),
      list(method = "restyle",
           args = list("type", "scatter"),
           label = "scatter")
      )

plot_ly(data = Nobels,
        frame = ~sortOrder,
        x = ~category, 
        y = ~liczba, 
        type = "bar"
)%>%layout(
  title = "Liczba nagórd nobla w poszczególnych kategoriach od 2010 roku",
  yaxis = list(range = c(0,11)),
  xaxis = list(title = "Kategoria"),
  showlegend = F,
  updatemenus = list(
    list( 
      x = 1, y = 1,
      buttons = steps

  )))  

