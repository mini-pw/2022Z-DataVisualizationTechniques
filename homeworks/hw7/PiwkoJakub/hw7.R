library(dplyr)
library(ggplot2)

group <- c(rep(1:1000, 2), rep(1001:1500, 2), rep(1501:2000, 2))
x <- c(sample(15:85, 1000, replace = TRUE), rep(50,1000),
       sample(20:80, 500, replace = TRUE), rep(50,500),
       sample(25:75, 500, replace = TRUE), rep(50,500)
       )
y <- c(sample(10:20, 1000, replace = TRUE), rep(80, 1000),
       sample(35:45, 500, replace = TRUE), rep(99, 500),
       sample(60:70, 500, replace = TRUE), rep(99, 500))
color <- rep(c('a' ,'b' ,'c' ,'d' ,'e', 'f', 'g', 'i', 'j', 'k'), 400)

df <- as.data.frame(list(group, x, y, color))
colnames(df) <- c("group", "x", "y", "color")

x2 <- c(sample(20:80, 30, replace = TRUE), sample(25:75, 30, replace = TRUE), 
        sample(35:65, 21, replace = TRUE), sample(40:60, 9, replace = TRUE))
y2 <- c(sample(10:35, 30, replace = TRUE), sample(35:60, 30, replace = TRUE),
        sample(60:75, 21, replace = TRUE), sample(75:90, 9, replace = TRUE))

ggplot() +
  geom_point(aes(x = sample(1:100, 200, replace = TRUE),
                 y = sample(1:100, 200, replace = TRUE)),
             color = "white", shape = 8) +
  geom_col(aes(x = 50, y = 80), width = 10, fill = "#5f4111") +
  geom_line(aes(x = x, y = y, group = group, color = color),
            data =  df, size = 1) +
  scale_color_manual(values = c("#04ff00", "#03a300", "#014500", "#699f60", "#476b41", 
                                "#a9db24", "#905429", "#14ab67", "#a2bc9c", "#6fff00")) +
  geom_point(aes(x = x2, y = y2),  color = "red", size = 4) +
  geom_point(aes(x = x2, y = y2),  color = "pink", size = 3) +
  geom_point(aes(x = x2, y = y2),  color = "purple", size = 2) +
  geom_point(aes(x = x2, y = y2),  color = "blue", size = 1) +
  geom_point(aes(x = 50, y= 100), color = "#ffe100", fill = "#ffe100", size = 12, shape= 25) +
  geom_point(aes(x = 50, y= 100), color = "#ffe100", fill = "#ffe100", size = 12, shape= 24) +
  geom_point(aes(x = 50, y= 100), color = "yellow", size = 20, shape= 8) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "#001a4f"))


ggsave("Choinka.png", units = c("cm"), width = 19, height = 15)
