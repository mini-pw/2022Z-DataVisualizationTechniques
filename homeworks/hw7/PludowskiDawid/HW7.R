library(ggplot2)
library(tidyverse)
library(ggstar)


y <- seq(from = 0, to = 50, by = 0.01)
x <- sin(y) / y^2

df <- data.frame(x = x, y = y)


ggplot(df) +
  geom_line( aes(x = x, y = y, color = y) ) +
  geom_star(
    aes(x = x, y = y),
    starshape = 2, color = "yellow"
    ) + 
  scale_color_gradient(low = "#66ff33", high = "#00cc00") +
  xlim(c(-0.01, 0.01)) + 
  theme_dark()


