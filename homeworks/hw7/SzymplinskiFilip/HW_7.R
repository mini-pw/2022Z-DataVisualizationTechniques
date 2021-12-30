library(dplyr)
library(ggplot2)
library(plotly)

library(gganimate)
library(Rcpp)
library(gifski)
library(av)


alpha <- rep(seq(0, (2*pi)-(pi/12), pi/12), 5)
addent <- rep(c(0, 2*pi, 4*pi, 6*pi, 8*pi), times = 1, each = 24)
alpha2 <- alpha + addent
r <- 4*alpha2
x <- r*cos(alpha2)
y <- r*sin(alpha2)
z <- alpha2*(-8)

all <- 1:length(z)
red <- sample(all, 20, replace = FALSE)
all <- all[-red]
blue <- sample(all, 20, replace = FALSE)
all <- all[-blue]
purple <- sample(all, 20, replace = FALSE)

df <- data_frame(x = x, y = y, z = z, color = rep("green", length(z)))
df[red, "color"] <- "red"
df[blue, "color"] <- "blue"
df[purple, "color"] <- "purple"
df[1, "color"] <- "star"

df <- df %>% 
  mutate(size = case_when(color == "star" ~ 20,
                          color == "green" ~ 2.5,
                          color == "red" ~ 3,
                          color == "blue" ~ 2,
                          color == "purple" ~ 3)) %>% 
  mutate(frame = 1:length(df$z))

df$color <- factor(df$color, levels = c("star", "green", "red", "blue", "purple"))


plot2 <- plot_ly(
  data = df,
  x = ~x,
  y = ~y,
  z = ~z,
  color = ~color,
  colors = c("yellow", "green", "red", "blue", "purple"),
  symbol = ~color,
  symbols = c(23, 21, 5, 1, 5),
  size = ~size,
  sizes = c(50, 400),
  type = "scatter3d",
  mode = "markers"
) %>% 
  layout(title = "Wesołych Świąt!")

plot2