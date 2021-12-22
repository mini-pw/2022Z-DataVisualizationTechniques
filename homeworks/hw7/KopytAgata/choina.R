library(ggplot2)
library(dplyr)
library(gganimate)

# drzewko
tree <- function(n) {
  b= seq(0,1,length.out=n)
  a=runif(n,0,1-b)
  ab <- data.frame(a,b)
  ab2 <- data.frame(-a,b)
  colnames(ab2) <- c('a','b')
  ab <- rbind(ab,ab2)
}
n1 = 300
Tree=tree(n1)

# bombki
n2 =50

#złote
bomb1 <- tree(n2) %>% filter(b<=0.96, b>=0.08)
bomb1 <- bomb1 %>% cbind(order=as.numeric(c(1:nrow(bomb1))))

#czerwone, chcę żeby było ich więcej na dole
bomb2 <- tree(n2) %>% filter(b<=0.9, b>=0.08, abs(a)<0.9)
bomb22 <- tree(n2) %>% filter(b<=0.5, b>=0.08,abs(a)<0.9)

bomb2 <- rbind(bomb2,bomb22)
bomb2 <- cbind(bomb2,order=as.numeric(c(1:nrow(bomb2))))


# wykres, który po zanimowaniu da choinkę
p <- ggplot()+
  geom_line(data = Tree,aes(a,b,color = abs(a)),size =3, show.legend = FALSE)+
  theme_void()+
  scale_color_gradient(low="#0d1f1c", high="#2e695e")+
  ggnewscale::new_scale_color()+
  geom_jitter(data =bomb2,aes(a,floor(b*10)/10, group =seq_along(b)),
             size=4, 
             shape =21,
             show.legend = FALSE,
             fill ='#A40A0A', 
             color ='#940000')+
  ggnewscale::new_scale_color()+
  geom_jitter(data =bomb1,aes(a,floor(b*10)/10, group =seq_along(b)),
              size=4, 
              shape =21,
              show.legend = FALSE,
              fill ='#DDC07E', 
              color ='#D1B576')
  

# w zależności od wybranego transition_reveal rysuję 2 załączone choinki :))
choinka_anim <- p +
  transition_reveal(b)+
  # transition_reveal(b-a)+
  shadow_trail(distance = 0.05, size=3)

animate(choinka_anim, end_pause = 30)
# anim_save("choinka.gif")