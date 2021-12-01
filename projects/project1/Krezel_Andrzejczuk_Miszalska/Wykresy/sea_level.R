library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


tabela4 <- read.csv2("Data/sea_level.csv")

tab4 <- as.data.frame(apply(tabela4, 2, as.numeric))

wyk3 <- ggplot(tab4, aes(x = Data, y = Liczba)) +
  geom_point(aes(x = Data, y = Liczba), color = "#00ffaa")  +
  labs(title = "Global Mean Sea Level", x = "",
       y = "Change in mean sea level [mm]") +
  theme(
    panel.background = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015, 2020))

wyk3
