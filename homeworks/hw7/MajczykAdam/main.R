library(dplyr)
library(ggplot2)
library(ggstar)

drzewo <- read.csv("dane.csv")
pieniek <- read.csv("dane_pieniek.csv")
swiatla <- read.csv("dane_swiatla.csv")
gwiazda <- read.csv("dane_gwiazda.csv")
snieg <- read.csv("dane_snieg.csv")

  



drzewoP <- ggplot()+
  geom_col(data = drzewo, aes(x = n0, y = value), fill = "dark green", width = 1) +
  geom_col(data = drzewo, aes(x = n0, y = -value), fill = "dark green", width = 1) +
  coord_flip() +
  scale_x_continuous(trans = "identity")

drzewoPieniek <- 
  drzewoP + 
  geom_col(data = pieniek, aes(x = n0, y = value), fill = "brown", width = 1)+
  geom_col(data = pieniek, aes(x = n0, y = -value), fill = "brown", width = 1) +
  coord_flip() +
  scale_x_continuous(trans = "identity")

drzewoPieniekSwiatla <- 
  drzewoPieniek +
  geom_point(data = swiatla, aes(x = n0, y= value, color = kolor, group = kolor), size = 5)+
  scale_color_gradient2(low = "red", mid = "yellow", high = "blue", midpoint = 2)

drzewoPieniekSwiatla +
  geom_star(data = gwiazda, aes(x = n0, y = value), size = 10, fill = "#FFD700", color = "#FFD700")+
  geom_star(data = snieg, aes(x = n0, y = value), size = 3, fill = "white", color = "white", starshape = "hexagonal star")  +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "transparent"),
    axis.title.x = element_text(colour = "transparent"),
    axis.title.y = element_text(colour = "transparent"),
    axis.text.x = element_text(colour="transparent"),
    axis.text.y = element_text(colour="transparent"),
    axis.ticks = element_blank(),
    legend.position = "none"
  )


