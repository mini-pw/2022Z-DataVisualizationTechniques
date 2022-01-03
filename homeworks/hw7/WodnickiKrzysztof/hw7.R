library(dplyr)
library(ggplot2)
library(gganimate)

# drzewko
gauss <- function(x, mean=0, sd=1) {
  ( 1/(sd*sqrt(2*pi)) ) * exp( -(x-mean)^2 / (2*sd^2) )
}

x <- seq(-5, 5, by=0.01)
bottom <- 0.7*gauss(x, sd=1.5)
middle <- gauss(x, sd=1)
top <- 0.5*gauss(x, sd=0.3)
tree <- data.frame(x = x, top = top, middle = middle, bottom = bottom)

dg <- "#386641"
g <- "#45DD22"

# płatki śniegu
nflake <- 250
nframe <- 150
flakes <- data.frame(x = c(), y = c(), group = c(), frame = c(), size = c(), alpha = c())

for (i in 1:nflake) {
  end <- abs(rnorm(1, 0, 0.01))
  y <- pmax(cumsum(c(runif(1, 0.1, 0.7), runif(nframe-1, -0.015, -0.005))), end)
  x <- c(runif(1, -5, 5), rnorm(nframe-1, 0, 0.02))
  x[y==end] <- 0
  x <- cumsum(x)
  
  group <- rep(i, nframe)
  frame <- 1:nframe
  size <- runif(1, 1, 2)
  size <- rep(size, nframe)
  alpha <- runif(1, 0.1, 0.7)
  alpha <- rep(alpha, nframe)
  
  new <- data.frame(x=x, y=y, group=group, frame=frame, size=size, alpha=alpha)
  flakes <- rbind(flakes, new)
}

# obrazek
p <- ggplot() +
  geom_polygon(data = tree, aes(x = x, y = top), color=dg, fill=g) +
  geom_polygon(data = tree, aes(x = x, y = middle), color=dg, fill=g) +
  geom_polygon(data = tree, aes(x = x, y = bottom), color=dg, fill=g) +
  geom_point(aes(x=0,y=0.5*gauss(0, sd=0.3)),
             color="yellow",
             shape="\u2605",
             size=8) +
  geom_point(data=flakes,
             aes(x=x, y=y, group=group, size=size, alpha=alpha),
             color="white",
             shape="*") +
  ggtitle("Wesołych Świąt!") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill="#3E517A"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(color="#D90429", size=18, face="bold", hjust=0.5)
  )

# animacja
pa <- p + transition_states(states = frame, transition_length = 3)
animate(pa, nframes=nframe)
