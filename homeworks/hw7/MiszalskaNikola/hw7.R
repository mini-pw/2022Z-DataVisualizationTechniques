library(ggplot2)
library(RColorBrewer)
library(dplyr)

x <- c(seq(0,10,0.2))

for(j in  0:2){
  
  for (i in seq(0.6, 2.4, 0.6)){
    y <- seq(0+j+i,10-j-i,0.2)
    x <- c(x,y)
}
}

for (i in seq(0.6, 1.8, 0.6)){
  y <- seq(3+i,7-i,0.2)
  x <- c(x,y)
}  

for (i in seq(0.6, 1.2, 0.6)){
  y <- seq(3.8+i,6.2-i,0.2)
  x <- c(x,y)
}  




h <- rep(0,51)
k <- 0.5
for(j in c(0,10,20)){
  for (i in seq(45-j, 27-j, -6)){
  y <- rep(k, i)
  h <- c(h,y)
  k <- k+0.5
}
} 

for (i in seq(15, 3, -6)){
  y <- rep(k, i)
  h <- c(h,y)
  k <- k+0.5
}
for (i in seq(7, 1, -6)){
  y <- rep(k, i)
  h <- c(h,y)
  k <- k+0.5
}


choinka <- data.frame(x,h)

library(gganimate)
library(Rcpp)
library(gifski)
library(av)

a <- c(2, 3,4.7,4,5.2,7.5,8,5.4,6,5,9)
b <- c(1,2,6,4,7,1,2.5, 2.5,5,0.5, 0.2)       
wspolrzedne <- data.frame(a,b)

snieg <- data.frame(x = runif(1200, 0, 10), y = runif(1200, 0, 10))

bombki <- c("tomato1", "red1","deepskyblue1", "darkmagenta","deeppink",
            "red1", "hotpink3", "deepskyblue1","goldenrod1","deeppink","orange")

gwiazda <- data.frame(5,9)

choinka <- ggplot(data = choinka, aes(x = x, y = h))+
  geom_point(size=5.3,color = "darkgreen")+
  transition_manual(h, cumulative = T)+
  geom_point(data = wspolrzedne, aes(x = a, y = b),color = bombki, size=11)+
  #transition_manual(a, cumulative = T)+
  geom_point(data = snieg,aes(x = x, y = y), shape = 8,size = 0.8, color = 'white')+
  geom_point(data = gwiazda, aes(x = X5, y = X9),
             shape = 24, size=15,color = "gold", fill = "gold")+
  geom_point(data = gwiazda, aes(x = X5, y = X9), 
             shape = 25, size=15,color = "gold", fill = "gold")+
  theme(panel.background = element_rect(fill = 'lemonchiffon'))


anim_save("choinka.gif", choinka)

 




