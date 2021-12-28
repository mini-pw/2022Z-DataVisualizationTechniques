#Load packages
library(rgl)
library(magick)

#Prepare spiral data
t <- seq(0.1,10*pi,0.01)
x <- seq(10*pi,0.1,-0.01) * cos(t)
y <-  seq(10*pi,0.1,-0.01) * sin(t)
z <-  t

t2 <- seq(0.1,10*pi,0.01) + pi/3
x2 <- seq(10*pi,0.1,-0.01) * cos(t2) * 0.9
y2 <-  seq(10*pi,0.1,-0.01) * sin(t2) *0.9

t3 <- seq(0.1,10*pi,0.01) + 2*pi/3
x3 <- seq(10*pi,0.1,-0.01) * cos(t3) * 0.8
y3 <-  seq(10*pi,0.1,-0.01) * sin(t3) * 0.8

#Start 3D plot
plot3d(x = c(x,x2,x3), y = c(y,y2,y3), z = c(z,z,z),
       col = c(rep("darkgreen",length(t)),rep("green",length(t)),rep("lightgreen",length(t))),
       axes = FALSE,
       xlab=NULL, ylab=NULL, zlab=NULL,
       size = 5)

#Generate random 200 spheres
rand <- sample(length(t) * 3,200)
rgl.spheres(x = c(x,x2,x3)[rand], y = c(y,y2,y3)[rand], z = c(z,z,z)[rand], r = 1.5, color = rainbow(200),
            axes = FALSE,
            alpha = 0.8)

#Set background colour
rgl.bg(color = "black")
par3d(windowRect = c(20, 30, 800, 800))

#Save tree to png file
rgl.snapshot("XmasTree.png",fmt = "png")

#Play and make a gif
play3d(spin3d(axis = c(0, 0, 1), rpm = 20), duration = 15)
movie3d(
  movie="XmasTree", 
  spin3d(axis = c(0, 0, 1), rpm = 20),
  duration = 15, 
  dir = tempdir(),
  type = "gif",
  clean = TRUE
)
