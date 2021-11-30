library(ggplot2)
library(dplyr)
require(maps)
library(RColorBrewer)

country <- map_data("world")

countries.area <- read.csv(file = "land-area-km.csv")

countries.area <- countries.area %>% 
  filter(Year == 2017)

countries.area

df <- read.table(file = "clipboard", sep = "\t", header = TRUE)
head(df)

df <- df %>% 
    left_join(countries.area, by = c("Code" = "Code")) %>% 
    mutate(Bee.Hives.per.sq.km = Bee.Number/Land.area..sq..km.)

p <- country %>% 
  left_join(df, by = c("region" = "country")) %>% 
  ggplot(aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = Bee.Hives.per.sq.km)) +
  scale_fill_gradient(low = "#f3c188", high = "#F08000", na.value = "grey")

p <- p +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
p


ggsave(p, filename = "tr_tst23333.png",  bg = "transparent", width = 12)

  
  
