# import libraries
library(ggplot2)
library(dplyr)
library(gganimate)

# data frame xmas tree and presents
df = data.frame(x = c(rep(0, 5), -2, 1.5, 3, -3),
                y = c(-2, 1, 4, 7, 10, -3, -3.5, -2.75, -3.25),
                kolor = c("brown", "#196c04", "#269e07", "#39e20e", "yellow",
                          "red", "orange", "pink", "purple"),
                rozmiar = c(25, 80, 75, 60, 40, rep(15, 4)),
                ksztalt = c(15, rep(17, 3), 42, rep(15,4)))


# create snowflakes data frame
snowflakes_number <- 400
frames_number <- 100

x <- rep(0, snowflakes_number * frames_number)
y <- rep(0, snowflakes_number * frames_number)

# start position of snowflakes
x[1:snowflakes_number] <- runif(snowflakes_number, min = -5.1, max = 5.1)
y[1:snowflakes_number] <- runif(snowflakes_number, min = -4.1, max = 22)

# generate changed snowflakes positions
for(i in 1:(frames_number-1)){
  # x
    x[(snowflakes_number*i+1):(snowflakes_number*(i+1))] <- 
      x[(snowflakes_number*(i-1)+1):(snowflakes_number*i)] - 
      runif(snowflakes_number, min = -0.1, max = 0.1)
    
  # y
    y[(snowflakes_number*i+1):(snowflakes_number*(i+1))] <- 
      y[(snowflakes_number*(i-1)+1):(snowflakes_number*i)] - 
      runif(snowflakes_number, min = 0, max = 0.25)
    
  # reset snowflake postion (it hits ground) 
    x <- ifelse(y < -4.1, runif(snowflakes_number, min = -5, max = 5), x) 
    y <- ifelse(y < -4.1, 11.1, y) 
}

# create data frame
df_snowflakes <- data.frame(x = x,
                            y = y, 
                            time = rep(1:frames_number, each = snowflakes_number))

# create animation
p <- ggplot() +
  geom_point(aes(x, y), df, shape = df$ksztalt,
             size = df$rozmiar, color = df$kolor) +
  geom_point(aes(x, y), df_snowflakes, size = 15, alpha = 0.5,
               color = "white", pch = 42) +
  xlim(-5, 5) +
  ylim(-4, 11) +
  theme_void() +
  theme(
    panel.background = element_rect("#0e72e2"),
    legend.position = "null",
    plot.margin = unit(c(-0.1,-0.1,-0.1,-0.1), "cm")
  ) +
  transition_time(time) +
  ease_aes('linear')

# show animation
animate(p, frames_number)