library(ggplot2)
library(tidyverse)
library(ggstar)


y <- seq(from = 0, to = 50, by = 0.01)
x <- sin(y) / y^2

df <- data.frame(x = x, y = y)


ggplot(df) +
  geom_line( aes(x = x, y = y, color = y) , size = 1) +
  geom_point(aes(x, y), color = "#00e600") + 
  geom_star(
    aes(x = runif(x, -0.01, 0.01), y = runif(y, 0, 500)),
    starshape = 2, color = "yellow", fill = "yellow"
    ) + 
  scale_color_gradient(low = "#66ff33", high = "#00cc00") +
  xlim(c(-0.005, 0.005)) + 
  ylim(c(10, 50)) + 
  theme_dark() + 
  theme(legend.position='none')


