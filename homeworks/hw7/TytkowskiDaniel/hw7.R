library(ggplot2)
library(ggimage)
pienx=runif(1000,-2,2)
pieny=runif(1000,0,8)
pien <- data.frame(pienx, pieny)
choinka <- ggplot(pien,aes(x = pienx, y =pieny))+
  geom_point(color = 'brown')


korona1 = data.frame()

for(i in 8:25){
  add <- data.frame(runif(500,-50+i,50-i),rep(i,500))
  korona1 <- rbind(korona1, add)
}
names(korona1) = c("xc","yc")

for(i in 26:40){
  add <- data.frame(runif(500,-60+i,60-i),rep(i,500))
  names(add) = c("xc","yc")
  korona1 <- rbind(korona1,add)
}
for(i in 41:70){
  add <- data.frame(runif(500,-70+i,70-i),rep(i,500))
  names(add) = c("xc","yc")
  korona1 <- rbind(korona1,add)
}

choinka <- choinka+
  geom_point(data = korona1,aes(x = xc, y = yc), size = 3,color = 'darkgreen')
choinka

bombki <- data.frame()

for(i in seq(8,25,by=3)){
  add <- data.frame(runif(5,-50+i,50-i),rep(i,5))
  bombki <- rbind(bombki, add)
}
names(bombki) = c("xc","yc")

for(i in seq(26,40,by=3)){
  add <- data.frame(runif(5,-60+i,60-i),rep(i,5))
  names(add) = c("xc","yc")
  bombki <- rbind(bombki,add)
}
for(i in seq(41,59,by = 3)){
  add <- data.frame(runif(3,-70+i,70-i),rep(i,3))
  names(add) = c("xc","yc")
  bombki <- rbind(bombki,add)
}
for(i in seq(60,70,by = 3)){
  add <- data.frame(runif(1,-70+i,70-i),rep(i,1))
  names(add) = c("xc","yc")
  bombki <- rbind(bombki,add)
}


swiatelka <- data.frame()

for(i in seq(8,25,by=3)){
  add <- data.frame(runif(5,-50+i,50-i),rep(i,5))
  swiatelka <- rbind(swiatelka, add)
}
names(swiatelka) = c("xc","yc")

for(i in seq(26,40,by=3)){
  add <- data.frame(runif(5,-60+i,60-i),rep(i,5))
  names(add) = c("xc","yc")
  swiatelka <- rbind(swiatelka,add)
}
for(i in seq(41,59,by = 3)){
  add <- data.frame(runif(3,-70+i,70-i),rep(i,3))
  names(add) = c("xc","yc")
  swiatelka <- rbind(swiatelka,add)
}
for(i in seq(60,70,by = 3)){
  add <- data.frame(runif(1,-70+i,70-i),rep(i,1))
  names(add) = c("xc","yc")
  swiatelka <- rbind(swiatelka,add)
}


choinka <- choinka +
  geom_point(data = bombki , aes(x = xc, y =yc), size = 5, color = "darkred")
choinka <- choinka + geom_point(data = swiatelka, aes(x = xc, y = yc), size = 2, color = 'yellow')
choinka <- choinka+
  theme_void()+
  theme(panel.background = element_rect(fill='#3333cc')) 
choinka
