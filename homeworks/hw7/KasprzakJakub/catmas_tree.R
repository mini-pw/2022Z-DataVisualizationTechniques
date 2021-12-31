library(ggplot2)
# remotes::install_github("R-CoderDotCom/ggcats@main")
library(ggcats)

df <- data.frame(seq(0, 100, by=0.1))

df[1:1001,2] <- 10

colnames(df) <- c("X", "Y")

df[df$X == 50,2] <- 500

temp <- df[501,]
df[501,] <- df[1,]
df[1,] <- temp
df[1002,] <- c(50, 10)

n <- nrow(df) - 1
new_data <- data.frame(X = c(rep(df$X[1], n), df$X[-1]),
                       Y = c(rep(df$Y[1], n), df$Y[-1]))
new_data$grp <- as.factor(rep(1:n, times = 2))

cats <- data.frame(c(c(1, 2), c(3, 4)))

ggplot(new_data, aes(X, Y, group = grp)) +
  geom_line(color = "dark green") +
  geom_rect(xmin = 40, xmax = 60, ymin = -20, ymax = 10, fill = "brown") +
  geom_cat(aes(x = 30, y = 160), cat = "nyancat", size = 6) +
  geom_cat(aes(x = 60, y = 60), cat = "pusheen_pc", size = 6) +
  geom_cat(aes(x = 68, y = 250), cat = "pop_close", size = 6) +
  geom_cat(aes(x = 43, y = 360), cat = "bongo", size = 6) +
  geom_cat(aes(x = 50, y = 500), cat = "pusheen", size = 5)
  