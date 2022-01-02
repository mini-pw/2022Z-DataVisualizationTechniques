library(plotly)
library(dplyr)
library(RColorBrewer)
#h+sqrt(x^2+y^2) == const
dataX <- runif(5000,-12,12);
dataZ <- runif(5000,-12,12);
dataY <- runif(5000,0,100);
dataC <- abs(sqrt(dataX^2 + dataZ^2)+0.2*dataY - 15);
data <- data.frame(dataX,dataZ,dataY,dataC)




data <- data %>% filter(sqrt(dataX^2 + dataZ^2)+0.2*dataY < 15) %>%rowwise() %>%  mutate(col = ifelse(
    dataC < 1,
    ifelse(sample(1:100,1,replace = TRUE)<10, "red",
                                        ifelse(sample(1:100,1,replace = TRUE)<10,"yellow",
                                                                            ifelse(sample(1:100,1,replace = TRUE)<10,"blue","green"))
           ), ifelse(sample(1:100,1,replace = TRUE)<50,"darkgreen","pinegreen")))




kolorki <- c("#3e6257","#008800","green","red","yellow","blue");
kolorki <-setNames(kolorki, c("pinegreen","darkgreen","green","red","yellow","blue"));

p <- plot_ly(data = data, x = ~dataX, y = ~dataZ, z = ~dataY, color = ~col, colors = kolorki, type = "scatter3d", mode = "markers") %>% 
    layout(
        plot_bgcolor = rgb(0,0,0),
        paper_bgcolor = rgb(0,0,0),
        showlegend = F,
        scene=list(
            
            xaxis = list(title = "", showgrid = F, showline = F, showticklabels = F,showspikes = F, showticklabels = F, visible = F),
            zaxis = list(title = "", showgrid = F, showline = F, showticklabels = F,showspikes = F, showticklabels = F, visible = F),
            yaxis = list(title = "", showgrid = F, showline = F, showticklabels = F,showspikes = F, showticklabels = F, visible = F)
        )
    ) %>% hide_colorbar()

p


