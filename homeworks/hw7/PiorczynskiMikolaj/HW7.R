library(ggplot2)
library(gganimate)
library(ggstar)

tree <- data.frame(
  x = c(-2.5, 0, 2.5, -2, 0, 2, -1, 0, 1),
  y = c(0, 6, 0, 3, 8, 3, 6.5, 10, 6.5),
  group = rep(c("bottom", "mid", "top"), each = 3)
) 
  
trunk <- data.frame(
  xmin = -0.5,
  xmax = 0.5,
  ymin = -2,
  ymax = 0
) 

baubles <- data.frame(
  x = c(1.9, 0, -1.2, 0.3, -0.4, -1.3, 0.7, -0.3, 0.4, -0.3, 0.2), 
  y = c(0.5, 0.7, 1.5, 2.5, 3.2, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5),
  color = sample(c("#d41111", "#ff00d9", "#1179d4", "#ffcc00", "#ffff00"), 11, replace = TRUE)
)

presents <- data.frame(
  xmin = c(-3, -1.75, 0.75),
  xmax = c(-2, -0.75, 2),
  ymin = c(-2, -2, -2),
  ymax = c(1, -0.5, -0.75)
)

bows_vertical <- data.frame(
  xmin = c(-2.6, -1.35, 1.275),
  xmax = c(-2.4, -1.15, 1.475),
  ymin = c(-2, -2, -2),
  ymax = c(1, -0.5, -0.75)
)

bows_horizontal <- data.frame(
  xmin = c(-3, -1.75, 0.75),
  xmax = c(-2, -0.75, 2),
  ymin = c(-0.1, -1.35, -1.275),
  ymax = c(0.1, -1.15, -1.475)
)

snow <- data.frame(
  x = runif(10000, -7, 7),
  y = runif(10000, -5, 15),
  state = rep(1:100, each = 100))


xmas_tree <- ggplot() +
  geom_polygon(data = tree, mapping = aes(x = x, y = y, group = group), fill = "#4a7700") + 
  geom_rect(data = trunk, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#4a3814") +
  geom_star(aes(x = 0, y = 10), size = 10, starshape = 1, color = "#ffd600", fill = "#ffd600") + 
  geom_point(data = baubles, aes(x = x, y = y, color = color), size = 5) +
  geom_rect(data = presents, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ff0000") + 
  geom_rect(data = bows_vertical, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ffd600") + 
  geom_rect(data = bows_horizontal, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ffd600") +  
  geom_point(data = snow, aes(x, y), size = 5, color = "white", pch = 42) +
  geom_text(aes(x = 0, y = 11.5, label = "Merry Christmas!"), color = "#ffd600", size = 18) + 
  coord_cartesian(xlim = c(-5, 5), ylim = c(-2, 12)) + 
  theme_void() + 
  theme(
    panel.background = element_rect("#2b5483"), 
    legend.position = "none"
  ) + 
  transition_states(snow$state, transition_length = 1, state_length = 1)

animate(xmas_tree)
anim_save("xmas_tree.gif", xmas_tree)
