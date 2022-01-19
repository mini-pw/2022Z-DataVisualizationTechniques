library(dplyr)
library(ggplot2)

x <- runif(30000)
y <- seq(0,1, length.out = 30000)

df <- data.frame(x,y)

tree <- ggplot(df, aes(x,y)) +
  geom_point(color = case_when(
    (x - 0.5)^2 + (y - 0.55)^2 < 0.0005 ~ "yellow",
    (x - 0.53)^2 + (y - 0.35)^2 < 0.0005 ~ "red",
    (x - 0.35)^2 + (y - 0.18)^2 < 0.0005 ~ "purple",
    (x - 0.63)^2 + (y - 0.22)^2 < 0.0005 ~ "orange",
    (x - 0.58)^2 + (y - 0.09)^2 < 0.0005 ~ "red", 
    (x - 0.47)^2 + (y - 0.2)^2 < 0.0005 ~ "magenta4",
    x >= 0.47 & x <= 0.53 & y <= 0.05 ~"chocolate4",
    y > 0.05 & y <= 0.25 & x >= 0.2 & x <= 0.8 & 1.3*x - 0.2 > y & -1.3*x + 1.1 > y ~ "green",
    y > 0.25 & y <= 0.45 & x >= 0.3 & x <= 0.7 & 1.3*x - 0.15  > y & -1.3*x + 1.15 > y ~ "green",
    y > 0.45 & y <= 0.7 & x >= 0.4 & x <= 0.6 & 1.3*x -0.1  > y & -1.3*x + 1.2 > y~ "green",
    TRUE ~ "darkblue"
  )) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),
        axis.line = element_blank(),axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.ticks = element_blank(), 
        axis.title.x = element_blank(),axis.title.y = element_blank(),)
tree
