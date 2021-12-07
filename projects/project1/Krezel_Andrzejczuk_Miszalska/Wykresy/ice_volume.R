library(dplyr)
library(ggplot2)
library(forcats)
library(Cairo)

background.color = "transparent"
grid.color = "transparent"
highlight.color = "#353535"
current.year.color = "#353535"
# mean.color = rgb(68, 134, 166, maxColorValue = 255);


ice.data.daily <- read.csv("Data/ice-volume-daily.dat", sep='')

mean.ice.vol = ice.data.daily %>% 
  group_by(X.day) %>% 
  summarise(Mean.X.day = mean(Vol))

ggplot() +
  geom_line(
    data = filter(ice.data.daily, Year != 2021),
    aes(x = X.day, y = Vol, group = Year, colour = Year)) +
  geom_line(
    data = filter(ice.data.daily, Year == 2021),
    aes(x = X.day, y = Vol, group = Year, colour = Year),
    colour = current.year.color, size = 1.2
  ) +
  # Mean over the years, but I've decided not to include it
  #geom_line(
  #  data = mean.ice.vol,
  #  aes(x = X.day, y = Mean.X.day),
  #  colour = mean.color, size = 1.2
  #) +
  geom_point(aes(x = 273, y = 4.987), size = 3, colour = current.year.color) +
  scale_color_gradient(low = "#ffffff", high = "#00ffaa") +
  scale_x_continuous(breaks = c(seq(0, 360, 90)), expand = c(0, 0)) +
  labs(
    x = "",
    y = "") +
  theme(
    plot.background = element_rect(fill = background.color, color = background.color),
    panel.background = element_rect(fill = background.color),
    axis.line = element_line(colour = "#5555bb", size = 1.2),
    axis.ticks = element_line(colour = "#5555bb", size = 1),
    axis.ticks.length = unit(1, "mm"),
    axis.text = element_text(size = 10),
    panel.grid = element_line(color = grid.color),
    legend.background = element_rect(fill = background.color),
  ) +
  annotate(
    geom="text", 
    x=275, y=3, 
    label="2021",color=highlight.color, 
    size = 5, fontface = 2) +
  guides(colour = guide_colourbar(ticks = FALSE, frame.colour = "black", frame.linewidth = 1, reverse = TRUE))
  

ggsave(file="Render/ice_volume_daily.png", type="cairo-png", width = 12, height = 5)
