library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)

interval = 0.05
t = seq(5*pi, 50*pi, interval)

tree.data <- data.frame(
  x = c(-cos(100*t), sin(100*t), -sin(100*t), -cos(100*t)) * (t - 60 * pi)^3,
  y = c(t, t + interval / 4, t + interval / 2, t + 3*interval/4),
  frame = rep(c(1, 2, 3, 4), length(t))
)

rand_t <- sample(t[t < 45 * pi], 400)
bulbs.data <- data.frame(
  x = c(sin(100*rand_t)) * (rand_t - 60*pi)^3,
  y = rand_t,
  frame = rep(c(1, 2, 3, 4), 100)
)

col <- rep(c("#5E8D63", "#4E846B", "#416245", "#3E6940"), length(t))
col <- c(col, rep(c("#ffcc00", "#C22C4E", "#BCBBFF", "#FF9400"), each=100))

data <- rbind(tree.data, bulbs.data)
data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(colour = col) +
  theme(
    plot.background = element_rect(fill = "#002060", color = "#002060"),
    panel.background = element_rect(fill = "#002060"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    x = "",
    y = ""
  )-> p
p

anim <- p + transition_states(frame, transition_length = 0.5, state_length = 0)
anim
anim_save("christmasTree.gif", anim)

