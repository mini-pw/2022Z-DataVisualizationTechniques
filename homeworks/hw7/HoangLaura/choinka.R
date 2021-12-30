library(ggplot2)
library(gganimate)

area.data <- data.frame(
  x = c(0, 1, 49, 50, 89, 90, 130),
  y = c(0, 25, 5, 15, 2, 10, 0)
)

star.data <- data.frame( x = 130, y = 0 )

choinka <- ggplot() +
  geom_area(data = area.data, aes(x=x, y=y), fill = "#005E00") +
  geom_area(data = area.data, aes(x=x, y=-y), fill = "#66B200") +
  geom_point(data = star.data, aes(x=x, y=y), shape = 24, size = 20, fill = "#FFD64C", color = "#FFD64C") +
  geom_point(data = star.data, aes(x=x, y=y), shape = 25, size = 20, fill = "#FFD64C", color = "#FFD64C") +
  transition_reveal(x) +
  labs(title = "WESOŁYCH SWIĄT! :D") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family = "mono", face = "bold", hjust = 0.5, color = "#005E00", size = 20)
        ) +
  xlim(0, 135) +
  coord_flip()

# choinka
animate(choinka, nframes = 300, fps = 50, end_pause = 30)
