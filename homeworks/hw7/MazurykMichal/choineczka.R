#choinka
library(dplyr)
library(ggplot2)
library("tidyverse")
library(gganimate)
library(Rcpp)
library(gifski)
library(av)

dane <- data.frame(x = runif(50000, -4, 4), y = runif(50000, 0, 12.5))

choinka<-dane %>% 
   mutate(
    "position" = case_when(
      (0<=y&y<1&0.5>=abs(x)) ~ "korzeń",
      (1<=y&y<5.5&y<=2*x+8&y<=(-2)*x+8)
      |(5.5<=y&y<8.5&y<=2*x+10.5&y<=(-2)*x+10.5)
      |(8.5<=y&y<11.7&y<=2*x+12&y<=(-2)*x+11.7) ~ "choinka",
       (0.15>=x^2+(y-12)^2) ~ "gwiazda",
      TRUE ~ "tło"
    ),"Marry"=position,"Christmas"=position,"To"=position,"Diego"=position
  )

ch1<-sample(1:10000,5000)
choinka[ch1,]<-choinka[ch1,] %>% mutate(Marry = case_when(
  position == "korzeń" ~ "korzeń1",
  position == "choinka" ~ "choinka1",
  position == "gwiazda" ~ "gwiazda1",
  position == "tło" ~ "tło1",
))
ch2<-sample(1:10000,5000)
choinka[ch2,]<-choinka[ch2,] %>% mutate(Christmas = case_when(
  position == "korzeń" ~ "korzeń2",
  position == "choinka" ~ "choinka2",
  position == "gwiazda" ~ "gwiazda2",
  position == "tło" ~ "tło2",
))
ch3<-sample(1:10000,5000)
choinka[ch3,]<-choinka[ch3,] %>% mutate(To = case_when(
  position == "korzeń" ~ "korzeń3",
  position == "choinka" ~ "choinka3",
  position == "gwiazda" ~ "gwiazda3",
  position == "tło" ~ "tło3",
))
ch4<-sample(1:10000,5000)
choinka[ch4,]<-choinka[ch4,] %>% mutate(Diego = case_when(
  position == "korzeń" ~ "korzeń4",
  position == "choinka" ~ "choinka4",
  position == "gwiazda" ~ "gwiazda4",
  position == "tło" ~ "tło4",
))

choinka<-choinka %>% 
  pivot_longer(cols = c("Marry","Christmas","To","Diego"), names_to = "state",values_to = "colorr")

color_opt<- c("korzeń"="#663300", 'choinka'="#006633",'gwiazda'="#FFFF00",'tło'='black',
              "korzeń1"="#996633", 'choinka1'="#FF0000",'gwiazda1'="#FFFFCC",'tło1'='white',
              "korzeń2"="#996633", 'choinka2'="#FF0099",'gwiazda2'="#FFFFCC",'tło2'='white',
              "korzeń3"="#996633", 'choinka3'="#66FFFF",'gwiazda3'="#FFFFCC",'tło3'='white',
              "korzeń4"="#996633", 'choinka4'="#FFFF99",'gwiazda4'="#FFFFCC",'tło4'='white'
              )
choinka<-choinka %>%  mutate(state =case_when(
  state == "Marry" ~ 1,
  state == "Christmas" ~ 2,
  state == "To" ~ 3,
  state == "Diego" ~ 4,
))
head(choinka)

anim_choinka1 <- choinka %>% 
  ggplot(aes(x=x,y=y,colour=colorr)) +
  geom_point(show.legend = FALSE, alpha = 0.7)+
  scale_color_manual(values = color_opt) +
  theme_void()+
  transition_states(state)

animate(anim_choinka1) 
  
  

