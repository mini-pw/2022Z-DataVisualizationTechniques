library(ggplot2)
library(dplyr)
set.seed(1)
df10 <- data.frame(PF = rnorm(10000))
df9 <- data.frame(PF = rnorm(9000))
df8 <- data.frame(PF = rnorm(8000))
df7 <- data.frame(PF = rnorm(7000))
df6 <- data.frame(PF = rnorm(6000))
df5 <- data.frame(PF = rnorm(5000))
df4 <- data.frame(PF = rnorm(4000))
df3 <- data.frame(PF = rnorm(3000))
df2 <- data.frame(PF = rnorm(2000))
df1 <- data.frame(PF = rnorm(1000))
dfp1 <- data.frame(cout = rnorm(10,150,50))
dfp2 <- data.frame(PF = rnorm(10,0,1))
dfp<-mutate(dfp1,PF=dfp2)
colnames(dfp) <- c("count", "PF")
p2<-ggplot(df10, aes(x = PF)) + 
  geom_histogram(data=df10, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#27AE60", 
                 fill = "#E9F7EF")+
  geom_histogram(data=df9, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#E74C3C", 
                 fill = "#D4EFDF")+
  geom_histogram(data=df8, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#F1C40F", 
                 fill = "#A9DFBF")+
  geom_histogram(data=df7, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#E74C3C", 
                 fill = "#7DCEA0")+
  geom_histogram(data=df6, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#F1C40F", 
                 fill = "#52BE80")+
  geom_histogram(data=df5, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#E74C3C", 
                 fill = "#27AE60")+
  geom_histogram(data=df4, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#F1C40F", 
                 fill = "#229954")+
  geom_histogram(data=df3, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#E74C3C", 
                 fill = "#1E8449")+
  geom_histogram(data=df2, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#F1C40F", 
                 fill = "#196F3D")+
  geom_histogram(data=df1, mapping=aes(x = PF),
                 breaks = seq(-5, 5, by = 0.1), 
                 colour = "#E74C3C", 
                 fill = "#145A32")+
  geom_point(data = dfp,
             mapping = aes(x=PF,y=count),
             shape=22,
             fill="#F1C40F",
             color="#F1C40F",
             size=10)+
  geom_point(data = dfp,
             mapping = aes(x=PF,y=count),
             shape=23,
             fill="#F1C40F",
             color="#F1C40F",
             size=10)+
  geom_point(data = dfp,
             mapping = aes(x=PF,y=count),
             shape=8,
             fill="#F1C40F",
             color="#F1C40F",
             size=10)
p2