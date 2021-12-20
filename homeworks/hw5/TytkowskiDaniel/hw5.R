library(dplyr)
library(tidyr)
library(plotly)


df = read.csv("complete.csv.xls")

#Data_frame
#tworze data_frame z zsumowanaymi ilosciami przyznanych nagrod Nobla 
#do wybranego roku wlacznie, z podzialem na kategorie


df3 <- data_frame()

for(i in 1901:2001){
  
  tmp_df <- df %>% 
    filter(awardYear <= i) %>% 
    group_by(category) %>% 
    count() %>% 
    mutate(Year = i)
  
  df3 <- rbind(df3, tmp_df)
}

#Wykres

fig <- plot_ly(showlegend = FALSE,data = df3, x = ~category, y = ~n, frame = ~Year, type = "bar") %>% 
  layout(title = "Count of awarded Nobel prizes in particular categories by set Year",
         yaxis = list(title = "Count"),
         xaxis = list(title = ""))
#wlaczajac animacje ladnie widac w jakim tempie przyznawano nagroody w roznych dziedzinach
fig


