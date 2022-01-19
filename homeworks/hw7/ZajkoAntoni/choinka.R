library(ggplot2)

val_1 <- 50:0
val_2 = - val_1

data <- data.frame(val_1, val_2)

xmas_tree <- ggplot() +
  geom_segment(aes(x = 0:50,
                   xend = 0:50,
                   y = val_1,
                   yend = val_2), color = 'green') +
  geom_point(aes(x = 0:50, y = val_1), color = 'darkgreen') +
  geom_point(aes(x = 0:50, y = val_2), color = 'darkgreen') +
  geom_segment(aes(x = -5:0,
                   xend = -5:0,
                   y = rep(-5, 6),
                   yend = rep(5, 6)), color = 'brown') +
  geom_point(aes(x = -5:0, y = rep(-5, 6)), color = 'brown') +
  geom_point(aes(x = -5:0, y = rep(5, 6)), color = 'brown') +
  coord_flip() + 
  labs(x = '', y = '')

generate_x_chain <- function(k){
  (50 - 3*k)*(-1)^(k + 1)
}

y_chain_1 = sapply(0:15, generate_x_chain)
y_chain_2 = sapply(1:16, generate_x_chain)
x_chain_1 = 3*(0:15)
x_chain_2 = 3*(1:16)

xmas_tree <- xmas_tree + 
  geom_segment(aes(x = x_chain_1,
                   xend = x_chain_2,
                   y = y_chain_1,
                   yend = y_chain_2), size = 3, color = 'darkgoldenrod')


get_y_of_bauble <- function(x){
  sample(-(50 - x):(50 - x), 1)
}


x_baubles = sample(0:50, 7)
y_baubles = sapply(x_baubles, get_y_of_bauble)
sizes_baubles = sample(4:6, 7, replace = T)
colors = sample(c('red', 'green', 'blues'), 7, replace = T)

xmas_tree <- 
  xmas_tree + 
  geom_point(
    aes(x = x_baubles, y = y_baubles, 
        size = sizes_baubles, color = colors)) +
    scale_size(range = c(7, 9)) + 
  theme(legend.position = 'none')

xmas_tree <- xmas_tree + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))

library('ggstar')

xmas_tree + 
  geom_star(aes(x = 50, y = 0), 
            size=10,
            starshape = 1, fill = 'gold')

ggsave('tree.png', xmas_tree)

