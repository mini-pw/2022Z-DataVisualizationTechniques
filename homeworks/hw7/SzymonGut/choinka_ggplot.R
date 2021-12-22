library(ggplot2)
library(gganimate)

drzewo = data.frame(x = c(-4, 4, 0,  -2, 2, 0,  -0.5, 0.5, 0,  -0.25, 0.25, 0,  -0.25, 0.25, 0,  -0.5, 0.5, -0.5,  -0.5, 0.5, 0.5), 
                    y = c(1, 1, 4,  3, 3, 5,  4.75, 4.75, 5.5,  5.4, 5.4, 5.833,   5.68867, 5.68867, 5.255667,  0.5, 0.5, 1,  1, 1, 0.5), 
                    group = rep(c('a', 'b', 'c', 'd', 'e', 'f', 'g'), each = 3))
drzewo_dol <- c(-3, 3, 0, -3, 1, 1, 5.5, 1)
dim(drzewo_dol) <- c(4, 2)
polygon <- sp::Polygon(drzewo_dol)
bombki <- sp::spsample(polygon, n = 60, type = 'random')
bombki <- as.data.frame(sp::coordinates(bombki))
kokardki <- data.frame(x1 = c(-2.6,-1.1,2.25,3.4), x2 = c(-2.4,-0.9,2.45,3.6),y1 = c(0,0,0,0),y2 = c(1,1.25,1,1.5))
prezenty <- data.frame(x1 = c(-3, -1.5, 2, 3),x2 = c(-2,-0.5,2.7,4), y1 = c(0,0,0,0),y2 = c(1,1.25,1,1.5))
poziome <- data.frame(x1 = c(-3,-1.5,2,3),x2=c(-2,-0.5,2.7,4),y1 = c(0.4,0.525,0.4,0.65),y2 = c(0.6,0.725,0.6,0.85))
snieg  <- data.frame(x = runif(10000, -4.5, 4.5), y = runif(10000, 0, 60), group = rep(c('a', 'b', 'c', 'd', 'e', 'f', 'g', "h", "i", "j"), each = 1000))
choinka <- ggplot() + 
  geom_polygon(data = drzewo, mapping = aes(x = x, y = y, group = group, fill = rep(c('g', 'g', 'g', 'y', 'y', 'z', 'z'), each = 3))) + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#1E3F20", "#F3DE8A", "#513A15")) +
  geom_point(data = bombki, mapping = aes(x = x, y = y, color = sample(1:8, length(x), replace = TRUE)), size = 4)  + 
  scale_color_gradientn(colours = rainbow(8)) +
  geom_rect(prezenty, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#ef3b2c" )+
  geom_rect(kokardki, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#fe9929" )+
  geom_rect(poziome, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#fe9929" )+
  theme(
    panel.background = element_rect(fill = "#74a9cf",
                                    colour = "#74a9cf",
                                    size = 0.5, linetype = "solid")) + 
  geom_point(snieg, mapping = aes(x = x, y = y), color = 'white') +
  coord_cartesian(ylim = c(0, 6), xlim = c(-4.5, 4.5))
anim <- choinka +
    transition_states(snieg$group,
                    transition_length = 2,
                    state_length = 1)
animate(anim)


anim_save("output.gif",anim)
