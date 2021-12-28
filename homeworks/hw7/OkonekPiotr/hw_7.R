library(ggplot2)
library(gganimate)
library(ggimage)

d <- data.frame(x1 = c(1.5,3,2.2,2.3,1.6,2.3,2.7,3.2), 
                y1 = c(1.2,1.1,2,1.4,2.1,2.8,2.4,1.9),
                level=c(1:2)
                )



p <- ggplot() +
  geom_polygon(aes(x = c(1, 2, 2), y = c(1, 1, 2)), fill = "darkgreen") +
  geom_polygon(aes(x = c(2.5, 2.5, 3.5), y = c(1, 2, 1)), fill = "darkgreen") +
  geom_polygon(aes(x = c(1.2, 2, 2), y = c(2, 2, 2.8)), fill = "white") +
  geom_polygon(aes(x = c(2.5, 2.5, 3.3), y = c(2, 2.8, 2)), fill = "white") +
  geom_polygon(aes(x = c(1.7, 2.8, 2.25), y = c(2.8, 2.8, 3.4)), fill = "red") +
  geom_rect(aes(xmin=2,xmax=2.5,ymin=2,ymax=2.8), fill = "white") +
  geom_rect(aes(xmin=2,xmax=2.5,ymin=1,ymax=2), fill = "darkgreen") +
  geom_rect(aes(xmin=2.15,xmax=2.35,ymin=0.5,ymax=1), fill = "black") +
  scale_color_identity() +
  ylim(c(0.5, 4.2)) +
  transition_states(states = level,transition_length = 0.1,state_length =3 )+
  geom_image(data=d,aes(x1,y1), size = 0.2,image="bombka.png") +
  geom_image(aes(x=2.25,y=3.7),size=0.2,image="puchar.png")+
  theme_void() + theme(plot.background = element_rect(fill = "gray"))

animate(plot = p, duration = 3, fps = 20)
anim_save("choinka.gif")
