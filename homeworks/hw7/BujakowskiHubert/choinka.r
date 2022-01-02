library(ggplot2)
library(dplyr)
library(plotly)
library(ggpubr)

df <- data.frame(x = c(1,3,3, 3,3,5, 1.5,3,3, 3,3,4.5, 2.5,3,3, 3,3,3.5), 
                 y = c(1,1,1.5, 1,1.5,1, 1.4,1.4,2, 1.4,2,1.4, 1.9,1.9,2.3, 1.9,2.3,1.9),
                 t = c('a','a','a','b','b','b','c','c','c','d','d','d','e','e','e','f','f','f'))

bombki <- data.frame(x = c(4,2,3,3.5, 2.8, 2.6, 3.4, 3.15, 3.1, 2.4,2),
                     y = c(1.2, 1.5, 2, 1.6, 1.55, 1.15, 1.13, 1.25, 1.65, 1.6,1.15), 
                     kolorek = c('orange','red','purple','blue','yellow','lightred','darkred','silver','gold', 'lightblue','pink'))

df %>% ggplot() +
  geom_polygon(mapping = aes(x = x, y = y, group = t), fill = 'darkgreen') +
  geom_rect(mapping = aes(xmin = 2.7, xmax = 3.3, ymin = 0.7, ymax = 1), fill = 'brown') +
  geom_rect(mapping = aes(xmin = 3.5, xmax = 4, ymin = 0.7, ymax = 0.9), fill = 'red') +
  geom_rect(mapping = aes(xmin = 3.7, xmax = 3.8, ymin = 0.7, ymax = 0.9), fill = 'gold') +
  geom_rect(mapping = aes(xmin = 3.5, xmax = 4, ymin = 0.78, ymax = 0.82), fill = 'gold') +
  geom_point(mapping = aes(x = 3.75, y = 0.9), color = 'gold', fill = 'gold', size = 11, shape = 22) +
  geom_point(mapping = aes(x = 3.75, y = 0.9), color = 'gold', fill = 'gold', size = 11, shape = 23) +
  geom_point(data = bombki, mapping = aes(x = x, y = y, colour = kolorek), size = 5) +
  geom_point(aes(x = 3, y = 2.27), color = 'gold',fill = 'gold', shape = 24, size = 14 ) +
  geom_point(aes(x = 3, y = 2.27), color = 'gold', fill = 'gold', shape = 25, size = 14 ) +
  theme_bw() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = '', y = '') +
  theme(legend.position = 'none',
        panel.background = element_blank())


