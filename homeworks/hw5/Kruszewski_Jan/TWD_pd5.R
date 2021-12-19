library(plotly)
library(dplyr)

dane <- read.csv(file = "C://Users//Admin//Downloads//complete.csv")
dane1 <- dane %>% group_by(gender, category) %>% count()
dane1 <-  dane1[-1,]
chem <- dane1 %>% filter(category=="Chemistry")
econ <- dane1 %>%  filter(category == "Economic Sciences")
dane2 <- chem %>% inner_join(econ, by = "gender")
liter <- dane1 %>% filter(category=="Literature")
dane2 <- dane2 %>% inner_join(liter, by = "gender")
peace <- dane1 %>% filter(category=="Peace")
dane2 <- dane2 %>% inner_join(peace, by = "gender")
phys <- dane1 %>% filter(category=="Physics")
dane2 <- dane2 %>% inner_join(phys, by = "gender")
medic <- dane1 %>% filter(category=="Physiology or Medicine")
dane2 <- dane2 %>% inner_join(medic, by = "gender")
dane2

fig2 <- plot_ly(dane2, x = ~gender) %>% add_bars(y = ~n.x, name = "A",visible = T) %>% 
  add_bars(y = ~n.y,  name = "B",visible = F)%>%
  add_bars(y = ~n.x.x, name = "C",visible = F) %>% 
  add_bars(y = ~n.y.y, name = "D",visible = F) %>% 
  add_bars(y = ~n.x.x.x, name = "E",visible = F) %>% 
  add_bars(y = ~n.y.y.y, name = "F",visible = F) %>% 
  layout(
  title = "Nobel prize in different fields for men and wemen",
  showlegend = FALSE,
  yaxis = list(title = "number of laureates"),
  
  updatemenus = list(
    list(
      y = 0.8, x = -0.3,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
             label = "Chemistry"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
             label = "Economic Sciences"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
             label = "Literatur"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE,TRUE, FALSE, FALSE)),
             label = "Peace"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
             label = "Physics"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
             label = "Physiology or Medicine")
        
        ))
    
  )
)
fig2
