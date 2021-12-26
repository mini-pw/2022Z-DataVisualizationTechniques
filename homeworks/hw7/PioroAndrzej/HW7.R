library(ggplot2)
library(ggtext)
library(RColorBrewer)
library(gganimate)
library(Rcpp)


# x <- strsplit("WESOŁYCHŚFIĄT", "")[[1]]
x <- 0:12
y <- 0:25
data <- expand.grid(X=x, Y=y)
data$z <- rep(0, 338) # czarny - tlo
data$z[data$Y <= 1] <- 1 # bialy - snieg
data$z[data$X == 6] <- 2 # brazowy - pien
data$z[data$X == 6 & data$Y==24] <- 5 # zolty - gwiazda
data$z[data$X == 6 & data$Y==25] <- 0 # czarny - tlo


# rysowanie dwukolorowej choinki
j <- 0
for (i in 9:3){
  data$z[data$Y==i & data$X %in% (6-j):(6+j)] <- 3
  j <- j+1
}

j <- 0
for (i in 15:10){
  data$z[data$Y==i & data$X %in% (6-j):(6+j)] <- 4
  j <- j+1
}

j <- 0
for (i in 20:16){
  data$z[data$Y==i & data$X %in% (6-j):(6+j)] <- 3
  j <- j+1
}

j <- 0
for (i in 23:21){
  data$z[data$Y==i & data$X %in% (6-j):(6+j)] <- 4
  j <- j+1
}

# dodawanie kolorowych bombek
data$z[data$X == 2 & data$Y == 16] <- 7 # niebieski - bombki
data$z[data$X == 3 & data$Y == 5] <- 6 # czerwony - bombki
data$z[data$X == 4 & data$Y == 11] <- 7 # niebieski - bombki
data$z[data$X == 5 & data$Y == 6] <- 5 # zolty - bombki
data$z[data$X == 5 & data$Y == 18] <- 8 # fioletowy - bombki
data$z[data$X == 6 & data$Y == 11] <- 6 # czerwony - bombki
data$z[data$X == 7 & data$Y == 5] <- 6 # czerwony - bombki
data$z[data$X == 7 & data$Y == 13] <- 7 # niebieski - bombki
data$z[data$X == 7 & data$Y == 18] <- 5 # czerwony - bombki
data$z[data$X == 9 & data$Y == 12] <- 5 # zolty - bombki
data$z[data$X == 9 & data$Y == 16] <- 6 # czerwony - bombki
data$z[data$X == 10 & data$Y == 3] <- 8 # fioletowy - bombki




# dodawanie kolorowych prezentow
data$z[data$X == 2 & data$Y== 0] <- 6 # czerwony - prezenty
data$z[data$X == 4 & data$Y== 0] <- 7 # niebieski - prezenty
data$z[data$X == 9 & data$Y== 0] <- 8 # fioletowy - prezenty
data$z[data$X == 11 & data$Y== 0] <- 5 # zolty - prezenty











# data$z <- as.factor(sample(c(0,1,2,3), 338,replace=T))


# xrep(x = strsplit("WESOŁYCHŚFIĄT", "")[[1]],  25)
# rep(0:24, )



# kosmetyczne zmiany w wygladzie wykresu 
p <- ggplot(data =  data, aes(x = as.factor(X), y = Y)) + 
  geom_tile(aes(fill = as.factor(z)))+
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0),
                   labels=strsplit("WESOŁYCHŚWIĄT", "")[[1]])+
  labs(x = NULL,
       y = NULL,
       title = "<span style = 'color: red;'>Ho!</span> <span style = 'color: white;'>Ho!</span> <span style = 'color: green;'>Ho!</span>") +
  theme(plot.title = element_markdown(hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#020634"),
        axis.text.x = element_text(colour="white"))+
  scale_fill_manual(values = c("black", "white", "brown", "darkgreen", "green", "yellow", "red", "blue", "violet"))

# anim <- p + transition_manual(Y, cumulative = TRUE)
anim <- p +
  transition_time(Y) +
  shadow_mark()

animate(anim, nframes=200, fps=25, width=400, height=600, end_pause = 75)

