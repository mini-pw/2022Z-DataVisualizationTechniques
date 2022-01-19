library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(emoji)

EmojiCount <- function(df1, df2, df3,start, end, color1, color2, color3){
  
        index1 <- df1$sender == "Kacper Skonieczka"
        df1$sender[index1] <- "Kacper"
        
        index2 <- df2$sender == "Dawid Płudowski"
        df2$sender[index2] <- "Dawid"
        
        index <- df3$sender == "Julia Przybytniowska"
        df3$sender[index] <- "Julia"
        
        df <- rbind(df1, df2, df3) 
        df <- filter(df,as.numeric(year(as.Date(df$time))) >= start & year(as.Date(df$time)) <=end)

        emoji <- df %>% 
          rwhatsapp::lookup_emoji(df, text_field = "content") %>% 
          separate_rows(emoji,  sep = ',') %>% 
          filter(emoji != "")
        
        
        my_emoji <- emoji %>% 
          filter(sender != "someone") %>% 
          group_by(emoji, sender) %>% 
          summarise(Sum = n()) %>%
          pivot_wider(names_from = sender, values_from = Sum, values_fill = 0)
        
        my_emoji$max <- pmax(my_emoji$Kacper, my_emoji$Dawid, my_emoji$Julia) 
        
        my_emoji <- my_emoji %>% arrange(desc(max)) %>% head(10)
        
        plot_ly(my_emoji, 
                x=~emoji)%>% 
          add_bars(y=~Kacper,  marker = list(color= color1),
                   hovertemplate = paste('%{text}',', Kacper', '<br>Total number: %{y:.5s}<br>'),
                   text = as.character(my_emoji$emoji),
                   texttemplate = '',
                     name = "Kacper") %>% 
          add_bars(y=~Dawid,  marker = list(color= color2),
                   hovertemplate = paste('%{text}',', Dawid', '<br>Total number: %{y:.5s}<br>'),
                   text = as.character(my_emoji$emoji),
                   texttemplate = '',
                   name = 'Dawid')  %>% 
          add_bars(y=~Julia,  marker = list(color= color3),
                   hovertemplate = paste('%{text}',', Julia', '<br>Total number: %{y:.5s}<br>'),
                   text = as.character(my_emoji$emoji),
                   texttemplate = '',
                     name = 'Julia') %>% 
          layout(xaxis = list(title = "",
                              tickfont = list(size=17)),
                     yaxis = list(title = ''),
                     showlegend = TRUE, 
                 legend = list(orientation = "h",xanchor = "center", x = 0.5,y = 5)) %>% config(displayModeBar = F)
 
}


ReactionsCount<- function(df1, df2, df3, start, end, color1, color2, color3){
  
  index1 <- df1$sender != "Kacper Skonieczka"
  df1$sender[index1] <- "Kacper"
  
  index2 <- df2$sender != "Dawid Płudowski"
  df2$sender[index2] <- "Dawid"
  
  index <- df3$sender != "Julia Przybytniowska"
  df3$sender[index] <- "Julia"
  
  if (end <= 2018){
    renderPrint({paste("no reactions")})
  }else{
  
  df <- data.frame(rbind(df1, df2, df3))
  df <- filter(df,as.numeric(year(as.Date(df$time))) >= start & year(as.Date(df$time)) <=end)
  
  reactions <- df %>%
    filter(messages.reactions != "NA" & (sender == "Julia" | sender == "Kacper" | sender== "Dawid"))%>% 
    count(messages.reactions, sender) %>% 
    arrange(desc(n)) %>% 
    pivot_wider(names_from = sender, values_from = n, values_fill = 0) %>% 
    filter(Kacper>=30 | Dawid>= 30 | Julia>= 30)

  plot_ly(reactions, 
          x=~messages.reactions)%>% 
    add_bars(y=~Kacper,  marker = list(color= color1),
             hovertemplate = paste('%{text}',', Kacper', '<br>Total number: %{y:.5s}<br>'),
             text = as.character(reactions$messages.reactions),
             texttemplate = '',
             name = "Kacper") %>% 
    add_bars(y=~Dawid,  marker = list(color= color2),
             hovertemplate = paste('%{text}',', Dawid', '<br>Total number: %{y:.5s}<br>'),
             text = as.character(reactions$messages.reactions),
             texttemplate = '',
             name = 'Dawid')  %>% 
    add_bars(y=~Julia,  marker = list(color= color3),
             hovertemplate = paste('%{text}',', Julia', '<br>Total number: %{y:.5s}<br>'),
             text = as.character(reactions$messages.reactions),
             texttemplate = '',
             name = 'Julia') %>% 
    layout(xaxis = list(title = "",
           tickfont = list(size=17)),
           yaxis = list(title = ''),
           showlegend = TRUE, 
           legend = list(orientation = "h",xanchor = "center", x = 0.5,y = 5)) %>% config(displayModeBar = F)
  }
  
}

