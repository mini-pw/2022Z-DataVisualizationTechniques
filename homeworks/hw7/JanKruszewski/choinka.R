library(ggplot2)
library(ggimage)
x <-seq(0,6,0.1)
y <- -x^2+6*x
df <- data.frame(x, y)
bombki <- data.frame(1:5, 2.5)
df$Image <- NA
df[62, 3]="yellow-star-2355777_960_720.png"
df[nrow(df),1] = 3.01
df[nrow(df),2] = 8.5
bombki <- data.frame(x = c(1,2,3,3.5, 5, 4, 3.4, 3.15, 5, 1.5,2, 4, 2),
                     y = c(1.2, 6, 3, 2.2, 4, 5, 6, 4, 1.65, 3.5,1.15, 1.1, 5), 
                     kolory = c('orange','purple','blue','yellow','red','darkred','silver','gold', 'lightred','lightblue','pink', 'violet', 'khaki'))
View(df)

ggplot(df)+
  geom_line(aes(x, y),size = 3, color = "green")+
  geom_area(aes(x, y),fill = "forestgreen")+
  geom_image(aes(x,y,image = Image), size = 0.3) +
  geom_point(bombki,mapping =aes(x, y, colour = kolory),size=5)+
  scale_y_continuous(expand = c(0,1.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
        )
  
  
  
  
  

df[nrow(df),1] = 7
df[nrow(df),2] = 2
df[nrow(df)+1, 3]="24283.png"
df[31, 3]="24283.png"