library(gganimate)
library(dplyr)
library(ggplot2)
library(ggnewscale)

ramka <- NULL
for(i in 1:100){
  x <- rnorm(i*20, 0, i/4)
  y <- rep(100 - i, i*20)
  color <- runif(i*20, 0, 10)
  df <- data.frame(x = x, y = y, color = color)
  ramka <- rbind(ramka, df)
}

gwiazda <- NULL
x <- rnorm(200, 0, 5)
angle <- seq(0,2*pi,pi/6)
for(i in angle){
  x1 <- x*cos(i)
  y1 <- x*sin(i) + 100
  df1 <- data.frame(x = x1, y = y1)
  gwiazda <- rbind(gwiazda, df1)
}
color <- runif(dim(gwiazda)[1], 10, 20)
gwiazda <- cbind(gwiazda, data.frame(color = color))

y <- runif(400, 0, 100)
x <- runif(400, -100, 100)
bombki <- data.frame(x = x, y = y, color = runif(100, 20, 30))
bombki <- bombki %>% 
  filter(y<=x*2+100,y<=x*(-2)+100) %>% 
  head(50)

plot <- ggplot(data = NULL, aes(x,y)) +
  geom_point(data = ramka, aes(color = color)) + 
  scale_color_gradient(low = "white", high = "darkgreen") +
  theme_void() +
  new_scale_color() +
  geom_point(data = gwiazda, aes(color = color)) +
  scale_color_gradient(low = "white", high = "gold") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'black')) +
  new_scale_color() +
  geom_point(data = bombki, aes(color = color), size=10) +
  scale_color_gradient(low = "firebrick1", high = "darkred") +
  transition_reveal(color) + 
  shadow_mark()
  
animate(plot)


        