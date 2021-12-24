
library(dplyr)
library(ggplot2)
library(gganimate)
library(Rcpp)
library(gifski)
library(av)



igly <- data.frame()
seria = seq(0,2,length.out = 100)
j = 0
for(i in seria){
  x <-  seq(from = (-2 + i), to = (2-i), length.out = 100-j)
  y <- rep(i,100-j)
  j = j+1
  tmp <- as.data.frame(list("x" = x,"y" = y, "k" = sample.int(3,size = length(x),replace = T)))
  igly <- bind_rows(igly,tmp)
}

pien_df <-  expand.grid(seq(-0.3,0.3, length.out = 30),seq(-0.1,0,length.out = 10)) 
colnames(pien_df) <- c("x","y")


lan_x <- seq(-1.5,1.75,length.out = 19)
lan_y <- -lan_x/13 + 0.5-(1.5/13)
p1 <- as.data.frame(list("x" = lan_x , "y" = lan_y))

lan_x <- seq(-1,1.125,length.out = 14)
lan_y <- -lan_x/13 + 0.5-(1.5/13) + 0.5
p1 <- bind_rows(p1,as.data.frame(list("x" = lan_x , "y" = lan_y)))

lan_x <- seq(-0.5,0.625,length.out = 7)
lan_y <- -lan_x/13 + 0.5-(1.5/13) + 1
p1 <- bind_rows(p1,as.data.frame(list("x" = lan_x , "y" = lan_y)))

lan_x <- seq(-1.875,1.625,length.out = 22)
lan_y <- lan_x/13 + (1.5/13) + 0.125
p2 <- as.data.frame(list("x" = lan_x , "y" = lan_y))

lan_x <- seq(-1.325,1.125,length.out = 15)
lan_y <- lan_x/13 + (1.5/13) + 0.625
p2 <- bind_rows(p2,as.data.frame(list("x" = lan_x , "y" = lan_y)))

lan_x <- seq(-0.825,0.675,length.out = 10)
lan_y <- lan_x/13 + (1.5/13) + 1.125
p2 <- bind_rows(p2,as.data.frame(list("x" = lan_x , "y" = lan_y)))

lan_x <- seq(-0.375,0.325,length.out = 5)
lan_y <- lan_x/13 + (1.5/13) + 1.525
p2 <- bind_rows(p2,as.data.frame(list("x" = lan_x , "y" = lan_y)))

swiatla <- bind_rows(p1,p2) 
swiatla$z <-  as.character(rep(c(4,5,6),length.out = nrow(swiatla)))

plt <-   ggplot(data = swiatla,aes(x = x,y = y,color = z)) +
  scale_color_manual(values = c("red","blue","green")) + 
  geom_point(data=pien_df,aes(x=x,y=y),shape = 15,size = 3,color = "brown") +
  geom_point(data=igly,aes(x=x,y=y),shape = 17,size = 4,
             color = ifelse(igly$k == 1,"#789759",ifelse(igly$k == 2,"#678A45","#567D30"))) +
  geom_point(shape = 19,size = 5) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  geom_point(aes(x = 0,y=2),shape = "\u2736",size = 30,color="yellow") +
  transition_states(z , transition_length = 3,state_length = 1) +
  enter_fade() +
  exit_fade()
  
animate(plt, height = 1000, width = 600, res = 150)



