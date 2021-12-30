library(dplyr)
library(ggplot2)
library(gganimate)
library(ggstar)

x1 <- runif(20000,-5,5)
y1 <- (5-abs(x1))*runif(20000,0,1)
warstwa1 = data.frame(x = x1,y = y1)

x2 <- runif(4000,-4,4)
y2 <- (4-abs(x2))*runif(4000,0,1) + 1.5
warstwa2 = data.frame(x = x2,y = y2)

x3 <- runif(3000,-3,3)
y3 <- (3-abs(x3))*runif(3000,0,1) + 3
warstwa3 = data.frame(x = x3,y = y3)

x4 <- runif(2000,-2,2)
y4 <- (2-abs(x4))*runif(2000,0,1) + 4.5
warstwa4 = data.frame(x = x4,y = y4)

xl1 <- seq(-3.75,3.75,0.01)
yl1 <- 0.053*(xl1)^2+1/2
lancuch1 <-data.frame(x=xl1,y=yl1)

xl2 <- seq(-3,3,0.03)
yl2 <- 0.053*(xl2)^2+2.02
lancuch2 <-data.frame(x=xl2,y=yl2)

xl3 <- seq(-2.3,2.3,0.05)
yl3 <- 0.053*(xl3)^2+3.42
lancuch3 <-data.frame(x=xl3,y=yl3)

xl4 <- seq(-1.8,1.8,0.05)
yl4 <- 0.053*(xl4)^2+4.52
lancuch4 <-data.frame(x=xl4,y=yl4) 

xs <- runif(2000,-6,6)
ys <- runif(2000,0,7)
mode <- 1:10
snieg <- data.frame(x=xs,y=ys,mode=mode)


xb <- rep(c(0,-1.2, 1.2,-1.7,-0.2,1,-2,-0.5,0.7,2.7),2)
yb <- rep(c(5,4.2, 4, 3,2.4, 2.8,1.5,1,1.2,1.8),2)
bombki <- data.frame(x=xb,y=yb) 

pien = data.frame(x = runif(1000,-0.5,0.5),y = runif(100,-0.5,0))

p <- warstwa1 %>% ggplot(aes(x=x,y=y)) + geom_point(colour = "darkgreen", shape = 17) +
  geom_point(data = warstwa2, color = "darkgreen",shape = 17) +
  geom_point(data = warstwa3, color = "darkgreen", shape = 17) +
  geom_point(data = warstwa4, color = "darkgreen", shape = 17) +
  geom_star(data = data.frame(x = 0, y = 6.7), fill = "#FFD700", size = 15) + 
  geom_point(data = pien, color = "brown") +
  geom_point(data = lancuch1, color = "#FFD700") +
  geom_point(data = lancuch2, color = "#FFD700") +
  geom_point(data = lancuch3, color = "#FFD700") +
  geom_point(data = lancuch4, color = "#FFD700") +
  geom_point(data = bombki, color = "#CD0000", size = 7)+
  geom_point(data = snieg, aes(group = seq_along(mode)), color = "white", shape = 8) +
  theme_void() + 
  theme(plot.background = element_rect(fill="black"))

p

p + transition_states(mode, transition_length = 2, state_length = 1) + enter_fade() + exit_shrink()

anim_save("hw7.gif")
