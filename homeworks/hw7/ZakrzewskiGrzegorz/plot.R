library(dplyr)
library(ggplot2)

x <- tibble(x = seq(0, 1, 0.01))
y <- tibble(y = seq(0, 1, 0.01))

df <- x %>% 
  full_join(y, by = character()) %>% 
  mutate(val = "0")

choinka <- function(x, y) {
  y > 0.1 & y < 2*(x - 0.1) & y < -2*(x - 0.9)
}

pien <- function(x, y) {
  y <= 0.1 & x > 0.45 & x < 0.55
}

df[pien(df$x, df$y),]$val = "1"

df[choinka(df$x, df$y),]$val = sample(rep(c("2", "3", "4", "5"), c(30, 1, 1, 1)),
                                      dim(df[zielony(df$x, df$y),])[1],
                                      replace = TRUE)

df %>% 
  filter(val != 0) %>% 
  ggplot(aes(x = x, y = y, color = val)) +
  geom_point(show.legend = FALSE, size = 2) +
  scale_y_continuous(limits = c(0, 0.8)) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )+
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_color_manual(
    values = c("1" = "#4f3a1c", "2" = "#358028", "3" = "#fff838", "4" = "#de3737", "5" = "#61ff6b")
  )

