library(ggplot2)
library(gganimate)
library(Rcpp)
library(gifski)
library(av)



df <- data.frame("y" = rnorm(100000))
pien <- data.frame(x = c(-0.25, 0.25, 0.25, -0.25), y = c(-0.1, -0.1, 0, 0))

x1 <- runif(7, -1.5, 1.5)
pkt1 <- data.frame("x" = x1,
  "y" = runif(7, 0.02, exp(-(x1)^2/2)/(sqrt(2*pi))),
  rodzaj = "p1")

x2 <- runif(7, -1, 1)
pkt2 <- data.frame("x" = x2,
                   "y" = runif(7, 0.05, exp(-(x2)^2/2)/(sqrt(2*pi))),
                   rodzaj = "p2")

x3 <- runif(7, -1, 1)
pkt3 <- data.frame("x" = x3,
                   "y" = runif(7, 0.1, exp(-(x3)^2/2)/(sqrt(2*pi))),
                   rodzaj = "p3")

pkt <- rbind(pkt1, pkt2, pkt3)

p <- ggplot(df, aes(x= y)) +
  geom_density(fill="green", color="green") +
  scale_y_continuous(limits = c(-0.1, 0.4)) +
  scale_x_continuous(limits = c(-3, 3))+
  geom_polygon(data=pien, mapping=aes(x=x, y=y), fill='brown', color='black')+
  geom_point(data = pkt, aes(x = x, y= y, color = rodzaj, size=1)) + scale_colour_manual(values = c("#984ea3", "#ff7f00", "#e41a1c"))+
  theme_void()+
  theme(legend.position = "none")


anim_p <- p +
  transition_states(rodzaj,
                    transition_length = 0.5,
                    state_length = 0.5)

animate(anim_p)
anim_save("GaussTree.gif", anim_p)
