library(dplyr)
library(tidyr)
library(ggplot2)

#Data Frame

df <- read.csv("drivers-forest-loss-brazil-amazon.csv")

df2 <- df %>% 
  select(!c(Entity, Code)) %>% 
  pivot_longer(!Year, names_to = "Driver", values_to = "n") %>% 
  mutate(n = n/1000000)
#Plot

plot <- ggplot(df2,aes(x = Year, y = n, fill = Driver))+
  geom_area(position = "stack", alpha = 0.8)+
  labs(title = "Drivers of the deforestation in the Brazilian Amazon between 2001 and 2013",
       subtitle = "Forest area lost measured in milions of hectares")+
  scale_fill_manual(values = c("#A6385D","#FA6950","#455FD6","#3A3B59","#EEF2DC","#E835E8","#46B072","#AAAAAA","#B58E2A","#DEA19B","#6A7A5C"),
                    labels = c("Commercial crops", "Fire", "Dams flooding", "Mining", "Natural Disturbances", "Infrastructure","Pasture","Roads","Selective logging","Small scale\nclearing","Tree plantations"))+
  theme(plot.background = element_rect(fill = "#fff6db"),
        axis.line = element_blank(),
        panel.grid.major = element_line(colour = rgb(0,0,0,0.2), size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff"),
        plot.title = element_text(face = "bold", size = 15, colour = "#017f38"),
        plot.subtitle = element_text(size = 13, colour = "#017f38"),
        axis.title.x = element_text(colour = "#017f38"),
        axis.title.y = element_text(colour = "#017f38"),
        legend.title = element_text(colour = "#017f38"),
        legend.text = element_text(colour = "#017f38"),
        axis.text.x = element_text(colour = "#017f38"),
        axis.text.y = element_text(colour = "#017f38"))+
  scale_x_continuous(breaks = c(2001:2013))+
  ylab("Area")
plot