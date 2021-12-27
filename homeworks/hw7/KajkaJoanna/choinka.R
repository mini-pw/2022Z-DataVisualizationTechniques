library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

vec <- c()
for (i in 1:30) {
  vec <- append(vec, 8)
}
for (i in 1) {
  vec <- append(vec, 18)
}
for (i in 1:22) {
  vec <- append(vec, 22)
}
for (i in 1) {
  vec <- append(vec, 35)
}
for (i in 1:13) {
  vec <- append(vec, 36)
}
for (i in 1) {
  vec <- append(vec, 60)
}
choinka <- data.frame(vec)
choinka <- choinka %>% mutate(name = 0.5)
val = c(50.5)
val1 = c(56)
val2 = c(8)
sign = c(0.5)
gwiazda <- data_frame(val, sign)
tmp <- data_frame(val1, sign)
pien <- data_frame(val2, sign)
snieg_1 <- c()
snieg_1 <- append(snieg_1, runif(n=30,min=50,max=60))
snieg_1 <- append(snieg_1, runif(n=45,min=45,max=60))
snieg_1 <- append(snieg_1, runif(n=60,min=40,max=60))
snieg_1 <- append(snieg_1, runif(n=75,min=35,max=60))
snieg_1 <- append(snieg_1, runif(n=90,min=30,max=60))
snieg_1 <- append(snieg_1, runif(n=105,min=25,max=60))
snieg_1 <- append(snieg_1, runif(n=120,min=20,max=60))
snieg_1 <- append(snieg_1, runif(n=135,min=15,max=60))
snieg_1 <- append(snieg_1, runif(n=150,min=10,max=60))
snieg_1 <- append(snieg_1, runif(n=165,min=5,max=60))
snieg_1 <- append(snieg_1, runif(n=180,min=0,max=60))
bombki_1 <- c(40,40,38,38,38,36,35,33,30,30,28,28,26,24,22,23,25,22,20,20,18,19,18,16,16,14,14,12,12,12)


snieg_2 <- runif(n=1155, min = 0, max = 1)
bombki_2 <- c(0.5,0.6,0.4,0.45,0.55,0.48,0.46,0.5,0.6,0.4,0.45,0.55,0.48,0.46,0.3,0.65,0.35,0.5,0.6,0.4,0.45,0.55,0.48,0.46,0.33,0.65,0.32,0.5,0.6,0.4)

snieg_3 <- c(rep(1,30),rep(2,45),rep(3,60),rep(4,75),rep(5,90),rep(6,105),rep(7,120),rep(8,135),rep(9,150),rep(10,165),rep(11,180))
snieg <- data.frame(snieg_1, snieg_2, snieg_3)
bombki <- data.frame(bombki_1,bombki_2)

tree <- ggplot() + 
  geom_violin(data=choinka, aes(x=name, y=vec), fill="green4") +
  geom_point(data=tmp, aes(x=sign, y=val1), color = "lightskyblue", size = 30) +
  geom_text(data=gwiazda, aes(x=sign, y=val), color = "goldenrod1", label="???", size=25, family = "HiraKakuPro-W3") +
  geom_point(data = bombki, aes(x=bombki_2, y=bombki_1), color = "red",size = 3)+
  geom_point(data=snieg, aes(x=snieg_2, y=snieg_1, frame=snieg_3), color="white")+
  geom_col(data=pien, aes(x=sign, y=val2), fill = "chocolate4", width = 0.15) +
  expand_limits(x = c(0,1), y = 0) +
  theme(panel.background = element_rect("lightskyblue"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio=1/1)
  
tree_plotly <- ggplotly(tree = ggplot2::last_plot())

tree_plotly %>% style(hoverinfo = "none")


