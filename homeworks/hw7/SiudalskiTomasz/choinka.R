library(plotly)
library(dplyr)
library(shiny)
library(rgl)
library(magick)

options(warn=-1)

#ig³y
igly <- NULL
z <- 5
for (i in seq(10, 1, by=-0.25)) {
  x <- seq(0, i, by = 0.01)
  y <- sqrt(i^2 - x^2)
  igly <- rbind(igly, cbind(x, y, z), cbind(-x, y, z), cbind(-x, -y, z), cbind(x, -y, z))
  z <- z+0.5
}
igly <- data.frame(igly)
igly$c <- "green"

#bombki
bombki <- NULL
z <- 5
n <- 20
for (i in seq(10, 1, by=-0.5)) {
  r <- i + 0.3
  x <- runif(n, -r, r)
  y <- sqrt(r^2 - x^2)
  m <- sample(c(-1,1), n, replace = TRUE)
  y <- y*m 
  c <- sample(c(0,1,2), n, replace = TRUE)
  bombki <- rbind(bombki, cbind(x, y, z, c))
  z <- z+1
  n <- n-1
}
bombki <- data.frame(bombki)

colors <- c("purple", "red", "blue")
bombki$c <- colors[ as.numeric( as.factor(bombki$c) ) ]

#pieñ
pien <- NULL
bok <- seq(-1.5, 1.5, by = 0.5)
for (i in seq(-2, 6, by=0.5)) {
  bok1 <- data.frame(x = bok, y = -1.5, z =i)
  bok2 <- data.frame(x = bok, y = 1.5, z =i)
  bok3 <- data.frame(x = -1.5, y = bok, z =i)
  bok4 <- data.frame(x = 1.5, y = bok, z =i)
  pien <- rbind(pien, bok1, bok2, bok3, bok4)
}
pien$c <- "brown"

#gwiazda
x <- c(0,0,0,0,0,0,cos(pi/6)*0.6,-cos(pi/6)*0.6,
       cos(pi/6)*0.6,-cos(pi/6)*0.6,
       cos(pi/6)*1.2,-cos(pi/6)*1.2,
       cos(pi/6)*1.2,-cos(pi/6)*1.2)
y <- 0
z <- c(23.3, 23.9, 24.5, 25.1, 25.7, 26.3, 
       25.1 - sin(pi/6)*0.6,
       25.1 - sin(pi/6)*0.6,
       25.1 + sin(pi/6)*0.6,
       25.1 + sin(pi/6)*0.6,
       25.1 - sin(pi/6)*1.2,
       25.1 - sin(pi/6)*1.2,
       25.1 + sin(pi/6)*1.2,
       25.1 + sin(pi/6)*1.2)
c <- "gold"
gwiazda <- data.frame(x,y,z,c)


#choinka
choinka <- rbind(igly, bombki, pien, gwiazda)

plot3d(choinka$x, choinka$y, choinka$z, col = choinka$c, type = "s", radius = .3,
       xlab = "", ylab = "", zlab = "",
       main = "", sub = "", ann = FALSE, axes = FALSE)

play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

movie3d(
  movie="Choinka3D", 
  spin3d( axis = c(0, 0, 1), rpm = 7),
  duration = 10, 
  dir = ".",
  type = "gif", 
  clean = TRUE
)











