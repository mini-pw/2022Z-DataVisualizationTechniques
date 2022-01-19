 
library(ggplot2)
library(dplyr)
library(gganimate)
tree <- data.frame(xmax = c( 3,1, 2, 0.5,1,0), y = c(1,5,5,8, 8, 10))
root <- data.frame(xmax = c(1,1), y = c(0,1))
star = data.frame(text = "⭐", x = 0, y = 10)
DISTANCE = 0.25
OFFSET_X = 0.1
OFFSET_Y = -0.05
FRAME_COUNT = 100
lights <- data.frame(x = c(seq(-2 + OFFSET_X, 2, DISTANCE), seq(-1.2 + OFFSET_X, 2, DISTANCE), seq(-1.2 + OFFSET_X, 2, DISTANCE),seq(-0.8 + OFFSET_X, 0.6666, DISTANCE)), 
                     y = c(seq(3 + OFFSET_Y, 1, -DISTANCE/2), seq(4.6 + OFFSET_Y, 3, -DISTANCE/2), seq(6.6 + OFFSET_Y, 5, -DISTANCE/2),seq(8.4 + OFFSET_Y, 7.6666, -DISTANCE/2))) %>% 
  tidyr::uncount(FRAME_COUNT) %>% 
  mutate(frame_nr = rep(1:FRAME_COUNT, length.out = length(x)), 
         alpha = rep(c(1:(FRAME_COUNT/4), (FRAME_COUNT/4):1, rep(0, times = 100), 1:(FRAME_COUNT/4), (FRAME_COUNT/4):1), length.out = length(x)))

fig <- tree %>% ggplot(aes(xmax = xmax, xmin = -xmax, y = y)) + 
  geom_ribbon(fill = "#00a000") + 
  geom_ribbon(data = root, fill = "brown") +
  geom_text(data = star, aes(x = x, y = y, label = text), inherit.aes = F, size = 10) +
  geom_point(data = lights, mapping = aes(x = x,y = y, alpha = alpha), inherit.aes = F,color = "red", size = 2, show.legend =  F)
  
 animated <- fig + transition_manual(frame_nr)

animate(animated, fps = 30)
