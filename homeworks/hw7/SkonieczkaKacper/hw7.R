library(tidyr)
library(plotly)
library(reshape2)
library(pracma)


x <- seq(-100, 100, by = 1)
y1 <- dnorm(x, mean = 0, sd = 25)
y2 <- dnorm(x, mean = 0, sd = 25)


leaf1 <- sort(runif(101,0,1))
leaf2 <- sort(runif(101,0,1))
trunk <- c(replicate(100, 0),-0.001,replicate(100, 0))
star1 <- data.frame(star1_x = replicate(201,0), star1_y = max(y1), style1 = "star")
snow1 <- data.frame(snow1_x  = replicate(201,-50), snow1_y = linspace(0.015,0,201), etaps = 1:201)

xmasBalls <- data.frame(xmasBalls_x = c(replicate(16,c(-25,0,25,
                                                         -15,0,15,
                                                         -10,0,10,
                                                         -7,-20,5)),
                                                          0,0,25,0,0,
                                                          -5,-25,0,0),
                        xmasBalls_y = c(replicate(16,c(0.0025,0.005,0.0025,
                                                         0.005,0.01,0.01,
                                                         0.01,0.0125,0.0125,
                                                         0.0125,0.007,0.007)),
                                                         0.005,0.005,0.005,0.005,0.005,
                                                         0.008,0.005,0.005,0.003),
                        bstyle = c(replicate(200, c("XMasBalls1")),"XMasBalls1"))


tree <- data.frame(numbers = x,
                         leaf  =y1,
                         trunk = trunk,
                         leaf2 = y2)

Ctree <- cbind(tree,star1,xmasBalls,snow1)

plot1 <- plot_ly(Ctree)
plot1 %>%  
  add_trace(x=~star1_x, y = ~star1_y, 
            type = 'scatter',
            symbol = ~style1,
            symbols = c('star'), 
            mode = 'markers',
            color = I('yellow'),
            marker = list(size = 50),
            "star") %>% 
  add_trace(x = ~xmasBalls_x , y = ~xmasBalls_y, 
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 20, color = "rgb(215, 0, 60)"), name = "Xmas ball 1") %>% 
  add_trace(x = ~xmasBalls_x , y = ~xmasBalls_y, 
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 20, color = "rgb(59, 149, 254)"), name = "Xmas ball 2") %>% 
  add_trace(x = ~xmasBalls_x , y = ~xmasBalls_y, 
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 20, color = "rgb(246, 56, 247)"), name = "Xmas ball 3") %>% 
  add_bars(x=~numbers, y=~leaf,width = 0.5,marker = list(color= 'rgb(25, 174, 0)'),showlegend = FALSE) %>% 
  add_bars(x=~numbers,y=~trunk,width = 20,marker = list(color= 'rgb(130, 31, 0)'),showlegend = FALSE) %>% 
  add_bars(x=~numbers,y=~leaf2,width = 0.5,marker = list(color= 'rgb(25, 123, 0)'),showlegend = FALSE)  %>% 
  layout(title = list(text = 'GAUSSIAN XMAS TREE', y = 0.95, x = 0.47), legend=list(title=list(text='Customize')),
         xaxis = list(showgrid = FALSE,showticklabels = FALSE, title = ""),
         yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
         paper_bgcolor='#FF8A8A',
         plot_bgcolor='rgb(233,233,233)')
  

  
  
