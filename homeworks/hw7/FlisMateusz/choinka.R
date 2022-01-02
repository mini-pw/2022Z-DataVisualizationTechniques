library(ggplot2)
library(dplyr)
library(plotly)

gwiazdka <- cbind(0,3.5,2,50)

bombka1 <- cbind(0.5,1.5,3,22)
bombka2 <- cbind(-0.34,2.2,3,22)
bombka3 <- cbind(-0.74,1,3,22)
bombka4 <- cbind(0.2,0.6,4,22)
bombka5 <- cbind(-0.14,1.7,4,22)
bombka6 <- cbind(0.4,2.1,4,22)
bombka7 <- cbind(0.9,0.8,5,22)
bombka8 <- cbind(-0.9,0.34,4,22)
bombka9 <- cbind(0.97,0.4,4,22)
bombka10 <- cbind(-0.57,1.4,5,22)
bombka11 <- cbind(-0.09,1.24,5,22)
bombka12 <- cbind(0.95,1.22,5,22)
bombka13 <- cbind(-0.27,2.62,5,22)
bombka14 <- cbind(0.08,2.52,4,22)

x <- runif(6000, min = -2, max = 2)
y <- runif(6000, min = 0, max = 5)

choinka <- (abs(x) > -y + 2 + 0.5 * floor(y))
wielkosc <- ifelse(choinka, 5, 10)



data <- rbind(cbind(x,y,choinka,wielkosc),
              gwiazdka,bombka1,bombka2,
              bombka3,bombka4,bombka5,
              bombka6,bombka7,bombka8,
              bombka9,bombka10,bombka11,
              bombka12,bombka13,bombka14)
data <- as.data.frame(data)

Noax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

fig <- plot_ly(
            data = data,
            x = ~x,
            y = ~y,
            color = ~choinka,
            colors = c("olivedrab4",
                       "dodgerblue3",
                       "gold1","red4",
                       "orange2","brown2"),
            type = 'scatter',
            symbol = ~choinka,
            symbols = c('circle','star',
                        'star','star-square',
                        'star-diamond','diamond-tall'),
            mode = 'markers',
            marker = list(size = ~wielkosc, alpha = 1)
            ) 

hide_colorbar(fig) %>% layout(xaxis = Noax, yaxis = Noax)    









