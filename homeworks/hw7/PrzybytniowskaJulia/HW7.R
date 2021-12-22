library(plotly)
library(dplyr)
seed(123)

data <- runif(4000, -4,4)
data1 <- runif(4000, 0,8)

data2 <- data.frame(cbind(data, data1)) %>% 
  filter(abs(data) + abs(data1) <4)
x <- nrow(data2)
data2 <- data2 %>% 
  mutate(random = runif(x, 0,1)) %>% 
  mutate(Extra = case_when(
           data1 == max(data1) ~ 1,
           TRUE ~ 0
         ))
help <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)


plot_ly(data2,
       x=~data,
       y = ~data1,
       type = "scatter", 
       mode = "markers", 
       color= ~random,
       symbol = ~Extra, 
       symbols = c("circle", "star"),
       size = ~Extra, 
       hoverinfo = 'x+y',
       colors = c("#081c15", "#1b4332", "#40916c", "#f8961e", "#ffd166")) %>%  
  layout(xaxis = help,
         yaxis = help)%>% hide_colorbar()


