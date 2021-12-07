library(dplyr)
library(ggplot2)
library(forcats)
library(Cairo)
library(tidyr)


water.usage.raw <- read.csv2("Data/water_usage.csv", fileEncoding = "UTF-8-BOM")

## Remove uneccesarry rows
to.remove = c("Sugar crops", "Starchy roots", "Oil Crops")

water.usage <- water.usage.raw %>% filter(!Product %in% to.remove)

background.colour = "transparent"

# colours visible on the poster were added in InkScape
p <- water.usage %>% 
  ggplot(aes(x = Water, y = reorder(Product, Water), fill = Type)) +
  geom_col() +
  theme(
    panel.background = element_rect(fill = background.colour, colour = background.colour),
    plot.background = element_rect(fill = background.colour, colour = background.colour),
    legend.background = element_rect(fill = background.colour, colour = background.colour),
  )

p
