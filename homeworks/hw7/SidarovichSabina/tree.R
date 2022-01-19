library(ggplot2)
library(gganimate)
library(transformr)

polydata <- rbind(
  data.frame(x = c(4, 4, 6, 6, 4), y = c(0, 1, 1, 0, 0), 
             group = "level1", fill = "brown"),
  data.frame(x = c(0, 5, 10, 0), y = c(1, 7, 1, 1), 
             group = "level2", fill = "darkgreen"),
  data.frame(x = c(1, 5, 9, 1), y = c(3, 8, 3, 3), 
             group = "level3", fill = "green"),
  data.frame(x = c(2, 5, 8, 2), y = c(5, 9, 5, 5), 
             group = "level4", fill = "darkgreen"),
  data.frame(x = c(3, 5, 7, 3), y = c(7, 10, 7, 7), 
             group = "level5", fill = "green"))

shapedata <- rbind(
  data.frame(x = seq(0, 10, 2), y =1, 
             group = "level2", color = "yellow"),
  data.frame(x = seq(1, 9, 2), y = 3, 
             group = "level3", color = "red"),
  data.frame(x = seq(2, 8, 2), y = 5, 
             group = "level4", color = "darkblue"),
  data.frame(x = seq(3, 7, 2), y = 7, 
             group = "level5", color = "darkviolet"),
  data.frame(x = 5, y = 10, group = "level1", color = "yellow"))

ggplot() + 
  geom_polygon(data = polydata, alpha = 1,
               aes(x = x, y = y, group = group, 
                   fill = "black", colour = fill), size = 1.5) +
  scale_colour_identity() + scale_fill_identity() +
  geom_point(data = shapedata, aes(x, y, group = group, 
                                   color = color, size = 15),
             shape =c(rep(1, (length(shapedata$x)-1)), 8), stroke = 3) +
  theme_void() +
  theme(plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black')) +
  xlim(-1, 10.0001)+
  coord_fixed() +
  transition_layers(layer_length = 1, transition_length = 10,
                    keep_layers = TRUE, from_blank =  FALSE) +
  enter_grow() -> p
animate(p, 23)
anim_save("tree.gif")
