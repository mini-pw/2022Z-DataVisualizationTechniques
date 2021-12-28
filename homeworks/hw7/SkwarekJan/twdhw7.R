library(dplyr)
library(ggplot2)
library(gganimate)
library(Rcpp)
library(gifski)
library(av)
library(maps)
library(geosphere)

lewy_rog <- c(-50,-30)
gora_choinki <- c(0, 70)
prawy_rog <- c(50,-30)
dol_choinki <- c(0, -42)
dol_pnia <- c(0, -70)
srodek_choinki <- c(0, 0)

data <- rbind(lewy_rog, gora_choinki, prawy_rog, dol_choinki, dol_pnia) %>% 
  as.data.frame()

colnames(data) <- c("long","lat")

map('world',
    col="#f2f2f2", fill=TRUE, bg="red", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)

points(x=data$long, y=data$lat, col="dark green", cex=3, pch=20)

inter <- gcIntermediate(gora_choinki,  lewy_rog, n=1, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col="dark green", lwd=30)

inter <- gcIntermediate(dol_choinki,  dol_pnia, n=1, addStartEnd=TRUE, breakAtDateLine=F)             
lines(inter, col="brown", lwd=35)

inter <- gcIntermediate(prawy_rog,  gora_choinki, n=1, addStartEnd=TRUE, breakAtDateLine=F)             
lines(inter, col="dark green", lwd=30)

inter <- gcIntermediate(prawy_rog,  lewy_rog, n=1, addStartEnd=TRUE, breakAtDateLine=F)             
lines(inter, col="dark green", lwd=30)

data2 <- rbind(srodek_choinki) %>% 
  as.data.frame()
colnames(data2) <- c("long","lat")

wypelnienie_lewe <- c(-45, -25)
data3 <- rbind(wypelnienie_lewe) %>% 
  as.data.frame()
colnames(data3) <- c("long","lat")

wypelnienie_prawe <- c(45, -25)
data4 <- rbind(wypelnienie_prawe) %>% 
  as.data.frame()
colnames(data4) <- c("long","lat")

wypelnienie_gorne <- c(0, 50)
data5 <- rbind(wypelnienie_gorne) %>% 
  as.data.frame()
colnames(data5) <- c("long","lat")

points(x=data2$long, y=data2$lat, col="dark green", cex=44, pch=20)
points(x=data3$long, y=data3$lat, col="dark green", cex=6, pch=20)
points(x=data4$long, y=data4$lat, col="dark green", cex=6, pch=20)
points(x=data5$long, y=data5$lat, col="dark green", cex=11, pch=20)

gwiazda <- c(0, 70)
data6 <- rbind(gwiazda) %>% 
  as.data.frame()
colnames(data6) <- c("long","lat")

points(x=data6$long, y=data6$lat, col="gold", cex=11, pch=20)

bombka1 <- c(-15, -20)
bombka2 <- c(-30, -5)
bombka3 <- c(10, 10)
bombka4 <- c(-10, 15)
bombka5 <- c(30, -10)
bombka6 <- c(5, -15)
bombka7 <- c(5, 30)
bombka8 <- c(20, -30)
bombka9 <- c(-15, 35)
bombka10 <- c(-35, -27)

bombki <- rbind(bombka1, bombka2, bombka3, bombka4, bombka5, bombka6, bombka7, bombka8, bombka9, bombka10) %>% 
  as.data.frame()
colnames(bombki) <- c("long","lat")

points(x=bombki$long, y=bombki$lat, col=c("red", "purple", "white", "blue", "yellow", "light blue"), cex=6, pch=20)

ggtitle(title(xlab = "MERRY CHRISTMAS AND A HAPPY NEW YEAR TO EVERYONE"))
