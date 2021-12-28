library(ggplot2)
library(ggimage)
library(gganimate)
library(dplyr)

punkty <- matrix(data = c(0, 0, 
                          1, 0,
                          1, 1,
                          5.5, 1,
                          2, 2.25,
                          4.5, 2.25,
                          1.5, 3.5,
                          3.5, 3.5,
                          1, 4.75, 
                          2.5, 4.75,
                          0, 6,
                          -2.5, 4.75,
                          -1, 4.75,
                          -3.5, 3.5,
                          -1.5, 3.5,
                          -4.5, 2.25,
                          -2, 2.25,
                          -5.5, 1,
                          -1, 1,
                          -1, 0,
                          0, 0), ncol=2, byrow = TRUE)
punkty <- as.data.frame(punkty)
names(punkty) <- c("x", "y")
punkty <- punkty %>% 
  mutate(lp = c(1:nrow(punkty))) %>% 
  select(c(3,1,2))


p_line <- ggplot() + 
  geom_path(data = punkty %>% filter(lp <=3), 
            mapping = aes(x = x, y = y), size = 2, color = 'brown') +
  geom_path(data = punkty %>% filter(lp >= 3 & lp <= 19), 
            mapping = aes(x = x, y = y), size = 2, color = 'green') +
  geom_path(data = punkty %>% filter(lp >= 19), 
            mapping = aes(x = x, y = y), size = 2, color = 'brown') +
  xlim(-10, 10) +
  labs(title = "My Christmas Tree") +
  theme(axis.title=element_blank()) +
  transition_reveal(lp)
animate(p_line)

