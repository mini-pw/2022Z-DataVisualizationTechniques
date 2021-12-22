library(ggplot2)
library(dplyr)

choinka <- function(x0, y0, width, height, id_s){
  x3 <- seq(x0-width/4, x0+width/4, length.out=3)
  x2 <- seq(x0-width/3, x0+width/3, length.out=3)
  x1 <- seq(x0-width/2, x0+width/2, length.out=3)
  y1 <- c(y0 - height/2, y0+height/6 , y0 - height/2)
  y2 <- c(y0-height/6, y0+height/3, y0-height/6)
  y3 <- c(y0 + height/6, y0+height/2, y0+height/6)
  data.frame(x=c(x1,x2,x3), y=c(y1,y2,y3), id=rep(c(id_s, id_s+0.33, id_s+0.66), each=3))
}

set.seed(2137)
x <- runif(1500, -10, 10)
y <- runif(1500, 0, 40)

z <- data.frame(x=x, y=y) %>% 
  filter(abs(x)+y/4 < 10)

root = data.frame(x=c(-1, -1, 1, 1), y=c(0, -6, -6, 0))

p <- ggplot() +
  geom_polygon(data=root, aes(x=x, y=y), fill="brown", color="black")

for(i in 1:dim(z)[1]){
  ch <- choinka(z[i, 1], z[i, 2], 12/7, 4, i)
  p <- p + geom_polygon(data=ch, aes(x=x, y=y, group=id), fill="green", color="black")
}

p +
  ylim(-6, 40) + 
  xlim(-20, 20) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  labs(title="Choincepcja",
       x="",
       y="")
