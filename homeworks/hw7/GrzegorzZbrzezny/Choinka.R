library(ggplot2)
library(dplyr)
library(plotly)
library(gganimate)
library(Rcpp)
library(gifski)
library(av)


data1 <- data.frame(x = c(-1, 1, 1, -1), y = c(0, 0, 1, 1))
data2 <- data.frame(x = c(-4.5, 4.5, 0), y = c(1, 1, 3.25))
data3 <- data.frame(x = c(-3.5, 3.5, 0), y = c(2, 2, 4))
data4 <- data.frame(x = c(-2.5, 2.5, 0), y = c(3, 3, 5))
xAxisValues <- c(runif(12, -4, 4), runif(10, -3, 3), runif(8, -2.75, 2.75), runif(8, -2.4, 2.4), runif(6, -1.6, 1.6), runif(5, -1.25, 1.25))
data5 <- data.frame(y = rep(c(1.25, 1.75, 2.25, 2.5, 3.25, 3.50), times = c(12, 10, 8, 8, 6, 5)), x = xAxisValues, group = c(sample(1:7, 49, replace= TRUE)))
data6 <- data.frame(x = 0, y = 5)


p <- ggplot() + 
  geom_polygon(data = data1, aes(x = x, y = y), fill = "#922D25") + 
  geom_polygon(data = data2, aes(x = x, y = y),fill = "#008736") +
  geom_polygon(data = data3, aes(x = x, y = y),fill = "#008736") +
  geom_polygon(data = data4, aes(x = x, y = y),fill = "#008736") + 
  geom_point(data = data6, aes(x = x, y = y), size = 15, shape = 8, color = "#FBFF00") +
  geom_point(data = data5, aes(x = x, y = y, color = as.factor(group)), size = 6) + scale_colour_manual(values = c("red", "blue", "green", "#FBFF00", "#FF6B00", "#FF008B", "#A400FF")) +
  theme_void() +
  theme(legend.position = "none", panel.background = element_rect(fill = 'black')) +
  transition_states(group)

animate(p)
anim_save("Choinka.gif")
