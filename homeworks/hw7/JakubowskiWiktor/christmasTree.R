library(dplyr)
library(ggplot2)

xballs = c(3, 4, 5, 6, 7, 8, 9, 10)
yballs = c(1, -2, -2.2, -3, 5, 6.1, -7, 8)
anim = c(1, 2, 2, 1, 2, 1, 1, 2)
balls = as.data.frame(cbind(xballs, yballs))
balls <- as.data.frame(cbind(xballs, yballs, anim)) %>% group_by(anim)

x = seq(0, 10, 1)
df1 = as.data.frame(x)
df1<- df1 %>% mutate(y1 = 2*x)
df2 <- df1[2:2, ]
df2
tree <- ggplot() +
  geom_col(data = df1, aes(x, y1), fill = "#006400") + 
  geom_col(data = df1, aes(x, -y1), fill = "#006400") + 
  geom_point(data = balls, aes(xballs, yballs), color = "red", size = 5) + 
  geom_point(data = balls, aes(xballs, 2*yballs-2), color = anim, size = 5, shape = 4) + 
  geom_point(data = balls, aes(xballs, -yballs/2 -1), color = "white", size = 5, shape = 18) + 
  geom_point(data = balls, aes(xballs, -2*yballs +2), color = anim, size = 6, shape = 4) + 
  geom_col(data = df2, aes(x+10, y1), fill = "#964b00") + 
  geom_col(data = df2, aes(x+10, -y1), fill = "#964b00") + 
  geom_point(data = df2, aes(x-0.4, y1-2), color = "#dec20b", shape = 8, size = 15, stroke = 2) +
  coord_flip() + 
  scale_x_reverse() + 
  theme_bw() +
  theme(axis.title = element_text(color = "transparent", size = 11),
        axis.text = element_text(color = "transparent", size = 12),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "transparent"),
        panel.border = element_rect(color = "transparent"),
        plot.background = element_rect(fill = "navyblue", color = NA)) + 
  scale_color_manual(values = c("orange", "#1bcff7"))

ggsave('tree.png', tree)
  