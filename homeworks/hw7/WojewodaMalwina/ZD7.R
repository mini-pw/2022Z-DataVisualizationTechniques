library(ggplot2)
library(lemon)
library(gganimate)

#jak piramida wieku
population = c(rep(5, 4), 
               seq(from = 50, to = 25, by = -1),
               seq(from = 40, to = 20, by = -1),
               seq(from = 30, to = 10, by = -1),
               seq(from = 20, to = 6, by = -1), 
               seq(from = 12, to = 0, by = -1))

a <- rep(seq(from = 0, to = 9, by = 1/11), 2)
d <- data.frame(age = paste(a, a + 1/11, sep = "-"),
                sex = rep(x = c("Female", "Male"), each = 100),
                pop = rep(population, 2), 
                x = runif(200, -50, 50),
                y = runif(200, 0, 100),
                state = rep(1:10, each = 10))
colors <- c(rep("#593005", 4), rep("#066D1B", 96), rep("#593005", 4), rep("#066D1B", 96))

dSTATIC <- data.frame(age = paste(a, a + 1/11, sep = "-"),
                sex = rep(x = c("Female", "Male"), each = 100),
                pop = rep(population, 2))


choinka <- ggplot(d) +
  geom_col(aes(x = ifelse(test = sex == "Male", yes = -pop, no = pop), 
                         y = age), #data = dSTATIC, #niby jak się tak da to powinno 
           #zostać staticzne przy animacji ale wtedy co innego wywala :(((
           fill = colors) +
  scale_x_symmetric(labels = abs) +
  geom_point(aes(x, y), size = 2, color = "white", pch = 8) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    legend.title=element_blank(),
    legend.position = "none",
    panel.background = element_rect("grey")
  )
choinka

choinkaAnimowanaCoZleDzialaXD <- choinka + 
  transition_states(state, transition_length = 1, state_length = 1)

animate(choinkaAnimowanaCoZleDzialaXD)

