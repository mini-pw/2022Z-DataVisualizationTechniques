library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(plotly)
library(emoji)
library(zoo)
library(data.table)
library(rwhatsapp)

usWithSomeonePlot <- function(df,start, end, color, someoneColor){
  df2 <- df %>% 
    filter(as.numeric(substr(df$time, 1,4)) >= start & as.numeric(substr(df$time,1,4)) <=end)
  
  index1 <- df2$sender == "Kacper Skonieczka"
  df2$sender[index1] <- "Kacper"
  
  index <- df2$sender == "Julia Przybytniowska"
  df2$sender[index] <- "Julia"
  
  index2 <- df2$sender != "someone" & df2$sender != "Julia" & df2$sender != "Kacper"
  df2$sender[index2] <- "Dawid"
  
  #messages by sender
  messages_count <- df2 %>% 
    group_by(sender) %>% 
    summarise(Messages = n())
  
  #photos_by sender
  photo_count <- df2%>% 
    filter(has_photo == "TRUE") %>% 
    group_by(sender) %>% 
    summarise(Photos = n())
  
  #sum of words by sender
  words_count <- df2 %>% 
    group_by(sender) %>% 
    unnest_tokens(input = content, output = word, format = "text",  drop = TRUE, to_lower = TRUE) %>% 
    count(sender) %>% 
    select(sender, "Words" = n)
  
  #sum of emoji by sender
  emoji <- df2 %>% 
    rwhatsapp::lookup_emoji(df, text_field = "content") %>% 
    separate_rows(emoji,  sep = ',') %>% 
    filter(emoji != "")
  
  emoji_count <- emoji %>% 
    group_by(sender) %>% 
    count() %>% 
    select(sender, "Emoji" = n)
  
  #sum of given reactions
  reactions_count <- df2 %>% 
    filter(messages.reactions != "NA") %>% 
    group_by(sender) %>% 
    count() %>% 
    rename(Reactions = n)
  C <- reactions_count[1,2]
  reactions_count[1,2] <- reactions_count[2,2]
  reactions_count[2,2] <- C
  
  data <- words_count %>% 
    inner_join(photo_count, by.x = sender, by.y = sender) %>% 
    inner_join(messages_count, by.x = sender, by.y = sender) %>% 
    inner_join(emoji_count, by.x = sender, by.y = sender)
  
  if (end >= 2018){
    data <- data %>% 
      inner_join(reactions_count, by.x = sender, by.y = sender)
  }
  
  data <- data %>% 
    setDT() %>% 
    melt( id = "sender") %>% 
    group_by(variable) %>% 
    mutate(Sum = sum(value), proc = (value/Sum)*100)
  
  plot_ly(data.frame(data),
          x=data$proc,
          y=~variable, 
          type='bar',
          color = ~sender,
          colors = c(color, someoneColor),
          text = paste(data$value, "%"),
          textposition = "top",
          hoverinfo = 'text', 
          hovertext = paste('Number: ', data$value,
                            '<br>Total number: ', data$Sum,
                            '<br>Percentage: ', round(data$proc,digits = 2), "%")
  ) %>%
    layout(xaxis = list(title = '', zeroline = FALSE, 
                        showline = FALSE, ticksuffix = "%"), 
           yaxis = list(title = ''), 
           barmode = 'stack')%>% 
    layout(legend = list(orientation = "h",xanchor = "center", x = 0.5)) %>% 
    add_annotations(text = data$value,
                    x = unlist(tapply(data$proc,data$variable, FUN=cumsum))-(data$proc/2), 
                    showarrow = FALSE, 
                   font = list(
                     color = "white"
                   )) %>% config(displayModeBar = F)
}



  