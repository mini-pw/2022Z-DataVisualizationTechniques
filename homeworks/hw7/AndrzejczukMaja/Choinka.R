library(ggplot2)
par(bg = "black")
plot(1:10,1:10,xlim=c(-10,10),ylim=c(0,10),type="n",xlab="",ylab="",xaxt="n",yaxt="n")

#pieniek
rect(-0.5, 0, 0.5, 2,col="brown", border = "brown")
#choinka
polygon(c(-4,0,4), c(2, 5, 2),col="darkgreen",border="darkgreen")
polygon(c(-3,0,3),c(4, 7, 4),col="darkgreen",border="darkgreen")
polygon(c(-2,0,2),c(6, 8, 6),col="darkgreen",border="darkgreen")
#gwiazda z dwoch trojkatow
points(x = 0, y = 8, pch = 24, col = "gold", bg = "gold", cex = 4)
points(x = 0, y = 8, pch = 25, col = "gold", bg = "gold", cex = 4)
#bombki
points(x = c(-2.5,-1,-1.2,1,-0.8, 1,-0.8,1,0.3,2.2),
       y = c(2.5, 2, 4.8, 5, 6.5, 6.2, 3.1, 2.5, 4, 2.8), 
       col = c("red"), pch = 20, cex = 2)
#œnieg 
points(x = runif(150, -10, 10), y = runif(150, 0, 10), col = "white", pch = 8)
mtext("Weso³ych Œwi¹t!!", side=3, col = "white", cex = 2)
