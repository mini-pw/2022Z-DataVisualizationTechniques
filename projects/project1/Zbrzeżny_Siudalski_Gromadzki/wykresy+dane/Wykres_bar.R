library(ggplot2)
library(dplyr)
library(ggalt)

tab <- read.csv("dane/modern-renewable-prod.csv")

tabpom <- tab %>% 
  filter(Entity=="India" | Entity=="Brazil" | Entity=="China" | Entity=="United States" | Entity=="Canada") %>% 
  filter(Year==2020)


Entity <- c(rep("Chiny",4),rep("Stany Zjednoczone",4),rep("Brazylia",4),rep("Kanada",4),rep("Indie",4))
rodzaj <- rep(c("Wodna", "Wiatrowa", "S³oneczna", "Inne"),5)
value <- c(1355.2000,466.50000,261.100000,129.73974,285.7896,336.53333
           ,132.630484,72.98492,391.1987,56.67775,7.593308,54.74157,382.3535,33.60374,4.386610,10.17645,163.5140
           ,60.41475,58.729050,15.58387)
wykres <- data.frame(Entity,rodzaj,value)

ggplot(wykres,aes(x=reorder(Entity,-value),y=value,fill=forcats::fct_reorder(rodzaj,-value)))+
  geom_bar(position = "fill", stat = "identity")+
  labs(title="Podzia³ wyprodukowanej energii odnawialnej w zale¿noœci od Ÿród³a w 5 krajach o najwiekszej produkcji w 2020 roku",
       x="Pañstwa",
       y="Udzia³ w ca³oœci")+
  scale_fill_manual("",values=c("Wodna"="navyblue","Wiatrowa"="darkorchid3","S³oneczna"="gold1","Inne"="olivedrab"))+
  scale_y_continuous(labels=function(x) paste0(100*x,"%")) +
  theme_light()



