# TWD HW5 Jêdrzej Soko³owski
# Nr. albumu 313507

library(dplyr)
library(plotly)
library(ggplot2)

# Przygotowanie danych

noble_df <- read.csv("complete.csv")
noble_df <- select(noble_df, c(awardYear,category,fullName,prizeAmount))


#Pierwszy wykres

steps <- list(
  list(args = list(list("marker.color" = list("red"), "fillcolor" = list("red"), "line" =list("red"))),
       label = "Czerwony",
       method = "restyle",
       value = "1"
  ),
  list(args = list(list("marker.color" = list("blue"),"fillcolor" = list("blue"),"line" =list("blue"))),
       label = "Niebieski",
       method = "restyle",
       value = "2"
  ),
  list(args = list(list("marker.color" = list("green"),"fillcolor" = list("green"), "line" =list("green"))), 
       label = "Zielony", 
       method = "restyle", 
       value = "3"
  )
)


fig1 <- plot_ly(data = noble_df, 
               y = ~awardYear,
               type = "box",
               name = "Year")
fig1 <- fig1 %>% layout(
  title = "Rozk³ad wybranej zmiennej dotycz¹cej nagród Nobla",
  xaxis = list(title = "Rok"),
  yaxis = list(title ="Wartoœæ"),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list("type", "box"),
             label = "Boxplot"),
        list(method = "restyle",
             args = list("type", "histogram"),
             label = "Histogram"),
        list(method = "restyle",
             args = list("type", "violin"),
             label = "Violin")
      )),
    list(
      x = 1, y = 0.9,
      buttons = list(
        list(method = "restyle",
             args = list("y", list(noble_df$awardYear)),
             label = "Rok przyznania"),
        list(method = list("relayout"),
             args = list("y", list(noble_df$prizeAmount)),
             label = "Nagroda pieniê¿na")
      ))
  ),
  legend = list(
    x = 1, y = 0.5
  ),
  sliders = list(
    list(
      active = 1,
      currentvalue = list(prefix = "Kolor: "),
      pad = list(t = 60),
      steps = steps
    )
  ))

#Drugi wykres

noble_1 <- noble_df %>% group_by(awardYear) %>% summarise(Count = length(category))
noble_2 <- noble_df %>% group_by(awardYear, category) %>% summarise(Count = length(category))

fig2 <- plot_ly(data = noble_1, x = ~awardYear, y = ~Count, type = 'bar')

fig2 <- fig2 %>% layout(
  title = "Iloœæ przyznanych nagród Nobla na przestrzeni lat",
  yaxis = list(title ="Iloœæ nagród", range = c(0,17)),
  xaxis = list(title ="Rok"),
  updatemenus = list(
    list(
      xanchor = 'center',
      yanchor = "center",
      buttons = list(
        
        list(method = list("restyle"),
             args = list("y", list(noble_1$Count)),
             label = "All"),
        
        list(method = "restyle",
             args = list("y", list(noble_2 %>% filter(category == "Physics") %>% pull(Count))),
             label = "Physics"),
        list(method = list("restyle"),
             args = list("y", list(noble_2 %>% filter(category == "Chemistry") %>% pull(Count))),
             label = "Chemistry"),
        list(method = list("restyle"),
             args = list("y", list(noble_2 %>% filter(category == "Economic Sciences") %>% pull(Count))),
             label = "Economic Sciences"),
        list(method = list("restyle"),
             args = list("y", list(noble_2 %>% filter(category == "Peace") %>% pull(Count))),
             label = "Peace"),
        list(method = list("restyle"),
             args = list("y", list(noble_2 %>% filter(category == "Physiology or Medicine") %>% pull(Count))),
             label = "Physiology or Medicine"),
        list(method = list("restyle"),
             args = list("y", list(noble_2 %>% filter(category == "Literature") %>% pull(Count))),
             label = "Literature")
      
      )),
    list(
      y=0.88,
      xanchor = 'right',
         buttons=list(
           list(method = "restyle",
                args = list("type", "bar"),
                label = "S³upkowy"),
           list(method = "restyle",
                args = list("type", "line"), #"yaxis.range", c(0,17)),
                label = "Liniowy")
         )),
    list(
      y=0.76, 
      xanchor = 'right',
         type = "buttons",
         buttons=list(
           list(method="relayout",
                args = list("yaxis.range", c(0,17)),
                label = "Fix the scale")
         ))
  ),
  legend = list(
    x = 1, y = 0.5
  ),
  sliders = list(
    list(
      active = 1,
      currentvalue = list(prefix = "Kolor: "),
      pad = list(t = 60),
      steps = steps
    )
  ))


#animacja

anim <- plot_ly(data = noble_1, y = ~Count, x = ~awardYear, frame = ~awardYear, type = "bar") %>% 
  layout(title = "Iloœæ wszystkich nagród Nobla przyznanych w danym roku",
         yaxis = list(title =  "Iloœc przyznanych nagród"),
         xaxis = list(title = "Rok"))

anim <- anim %>% 
  animation_opts(frame = 100,
                 redraw = FALSE) %>%
  animation_button(x = 0.05, y = 0.08) %>%
  animation_slider(currentvalue = list(prefix = "Rok: ", font = list(color="black")))



