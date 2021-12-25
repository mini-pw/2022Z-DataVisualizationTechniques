library(ggplot2)
library(dplyr)
library(gganimate)
library(ggstar)
library(foreach)
#1 dataframe:
set.seed(430)
indices <- 1:50000
x<- runif(50000)
y<- runif(50000)
df <- data.frame(indices, x, y)
df <- df %>% mutate(part = ifelse(x>0.45 & x < 0.55 & y < 0.1, 'trunk',
                                  ifelse( y >= 0.1 & ((x <= 0.5 & y < x+0.1) | (y < 1.1 - x & x > 0.5 )), 'tree', #lowest tree part
                                          ifelse(y >= 0.45 & ((x>0.1 & y < 0.35+x & x <= 0.5) | (x < 0.9 & y< 1.35 - x & x > 0.5)), 'tree',
                                                 ifelse(y>=0.75 & ((x>0.3 & y < 0.45+x & x <= 0.5) | (x< 0.7 & y < 1.45 -x & x > 0.5)), 'tree','')
                                                 )
                                          ) 
                                  )
                    )
#bulbs
treeparts <- df %>% filter(part =='tree')
bulbs <- sample(treeparts$indices, 25)
df <- df %>% mutate(part = ifelse(indices %in% bulbs, 'bulb', part), size = ifelse(indices %in% bulbs, 10, 0.01))
#guirlande

x = c(seq(0.15, 0.7, length.out = 3000), seq(0.3, 0.7, length.out = 2000), seq(0.3, 0.6, length.out = 1000), seq(0.4, 0.65, length.out = 4000))
y = c(seq(0.25, 0.4, length.out = 3000), seq(0.45, 0.65, length.out = 2000), seq(0.65, 0.75, length.out = 1000), seq(0.85, 0.45, length.out = 4000))
indices = 50001:60000
part = rep('guirlande', 10000)
size = rep(1, 10000)

guirlande <- data.frame(indices, x, y, part, size) %>% mutate(y = runif(y, min = y-0.02, max = y + 0.02))

tree<-df %>% bind_rows(guirlande) %>% filter(part != '')

#plot
p <- tree %>% ggplot(aes(x=x, y=y, color = part))+geom_point(size = tree$size)+
  scale_color_manual(values = c('red', 'yellow', 'green', 'brown'))
p

p_anim <- p + transition_time(indices)+shadow_mark(past = T, future = F)
p_anim
anim_save("choineczka.gif", p_anim)
