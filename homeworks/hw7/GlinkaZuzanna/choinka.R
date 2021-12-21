library(ggplot2)
library(gganimate)
library(ggstar)
library(dplyr)
choinka <- data.frame(X = c(12, 0, 11, 2 ,10 ,3, 9, 4, 8, 5, 7, 6), Y = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8 ,9, 10))
gwiazdka <- data.frame(X = 6, Y = 10)
drzewko <- ggplot() + geom_path(data = choinka, aes(x = X, 
                                           y = Y, colour = Y), size = 10) + scale_color_gradient(
                                             low = "darkgreen", high = "white") + 
  labs(title = "Radosnych Świąt Bożego Narodzenia!!") + 
  xlab("") + ylab("") + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),strip.background = element_rect(colour=NA, fill=NA),panel.border = element_rect(fill = NA, color = NA),
        plot.title = element_text(family = "mono", face = "bold", color = "black", size = 20)) +
  geom_star(data = gwiazdka, aes(x = X, y = Y), size = 15, colour = "yellow", fill = "yellow") + 
  scale_fill_manual(values = "yellow")

p_line

anim_line <- drzewko + 
  transition_reveal(Y) + enter_fade() + 
  exit_shrink()
animate(anim_line, end_pause = 30)
anim_save("choinka.gif", anim_line)