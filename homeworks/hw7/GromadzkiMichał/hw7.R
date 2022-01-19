library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)



x=0
y=0
color="green"
df <- data.frame(x,y,color)


for (i in seq(0,10,by=0.1)){
  for (j in seq(0,10,by=0.1)){
    if(j<i && j<10-i){
      if(runif(1)<0.05){
        df <- df %>% add_row(x=i,y=j,color="red") 
      }else if(runif(1)<0.1 && 0.05<runif(1)){
        df <- df %>% add_row(x=i,y=j,color="yellow")
      }else{
      df <- df %>% add_row(x=i,y=j,color="green")
      }
    }
  }
}



tree <- ggplot(df,aes(x=x,y=y,fill=color,color=color))+
  geom_point(shape=24,size=5)+
  scale_color_manual(values=c("#014325","#D11141","gold"))+
  scale_fill_manual(values=c("#014325","#D11141","gold"))+
  theme(legend.position = "None")+
  theme_void()
  



