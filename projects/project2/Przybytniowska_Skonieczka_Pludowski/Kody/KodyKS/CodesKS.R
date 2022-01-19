library("rjson")
library("tools")
library("lubridate")
library("tidyr")
library("plotly")
library("zoo")
library("dplyr")
library("stringr")


color1 <- "#1E90FF"
color2 <- "#fa3c4c"
color3 <- '#EE82EE'

####################################

##
FriendNick1 <- "Friend 1"
FriendNick2 <- "Friend 2"
FriendNick3 <- "Friend 3"
FriendNick4 <- "Friend 4"
FriendNick5 <- "Friend 5"
FriendNick6 <- "Friend 6"
FriendNick7 <- "Friend 7"
FriendNick8 <- "Friend 8"
FriendNick9 <- "Friend 9"
FriendNick10 <-"Friend 10"
##


Sys.setlocale("LC_TIME", "English")

friend <- list(FriendNick1, FriendNick2, FriendNick3,
               FriendNick4, FriendNick5, FriendNick6,
               FriendNick7, FriendNick8,FriendNick9,
               FriendNick10)

###################################

activityPlot2 <- function(dfActivityPlot,
                          friend1,
                          friend2,
                          friend3,
                          from = 2010,
                          to = 2022){
  
  Sys.setlocale("LC_TIME", "English")
  dfActivityPlot %>% 
    filter(year(dates) >= from,  year(dates) <= to) -> dfActivityPlot
  
  dfActivityPlot$dates <- weekdays(as.Date(dfActivityPlot$dates))
  
  dfActivityPlot$dates <- factor(dfActivityPlot$dates, levels =  c("Monday", "Tuesday",
                                                                      "Wednesday", "Thursday",
                                                                      "Friday", "Saturday", "Sunday"))
 
   dfActivityPlot %>% 
    group_by(FriendNick, dates) %>% 
    summarise(number = sum(number)) -> dfActivityPlot
  
   
   dfActivityPlot %>% 
     pivot_wider(names_from = FriendNick, values_from = number, values_fill = 0) -> dfActivityPlot
  
   
  if(!(friend1 %in% colnames(dfActivityPlot)) & 
     !(friend2 %in% colnames(dfActivityPlot)) &
     !(friend3 %in% colnames(dfActivityPlot))) {
    days <- c("Monday", "Tuesday",
              "Wednesday", "Thursday",
              "Friday", "Saturday", "Sunday")
    df <- data.frame(days = as.factor(days), zeros = 0)
    df$days <- factor(df$days, levels =  c("Monday", "Tuesday",
                                           "Wednesday", "Thursday",
                                           "Friday", "Saturday", "Sunday"))
    plot_ly(df, x=~days, y=~zeros, type = "bar") %>% 
      layout(yaxis = list(range = c(0,10)))
  } else {
    plot1 <- plot_ly(dfActivityPlot, x=~dates)
    if(friend1 %in% colnames(dfActivityPlot)){
      plot1 %>% add_bars(y=~get(friend1),  marker = list(color= "#1E90FF"),
                         hovertemplate = paste('%{text}', '<br>Number of messages: %{y:.5s}<br>'),
                         text = as.character(dfActivityPlot$dates),
                         texttemplate = '',
                         name = friend1) -> plot1
      
    }
    if(friend2 %in% colnames(dfActivityPlot)){
      plot1 %>% add_bars(y=~get(friend2), marker = list(color= color2),
                         hovertemplate = paste('%{text}', '<br>Number of messages: %{y:.5s}<br>'),
                         text = as.character(dfActivityPlot$dates),
                         texttemplate = '',
                         name = friend2) -> plot1
      
    }
    if(friend3 %in% colnames(dfActivityPlot)){
      plot1 %>% add_bars(y=~get(friend3), marker = list(color= '#EE82EE'),
                         hovertemplate = paste('%{text}', '<br>Number of messages %{y:.5s}<br>'),
                         text = as.character(dfActivityPlot$dates),
                         texttemplate = '',
                         name = friend3) -> plot1
      
    }
    plot1 %>% layout(xaxis = list(title = "Day",dtick=1),
                     yaxis = list(title = 'Number of messages'),
                     showlegend = FALSE) %>% 
      config(displayModeBar = F)
  }
  
}


ourActivityPlot <- function(df1,color = '1E90FF', start = 2010, end = 2022){
  df1 <- data.frame(date = as.Date(df1$time))
  
  df1 %>% 
    filter(year(date) >= start  & year(date) <= end) -> df1
  
  df1$date <- as.yearmon(df1$date)

  df1 %>% 
    group_by(date) %>% 
    summarise(number = n()) -> df1
  

  
  plot_ly(df1,
          x=~date,
          y = ~number,
          type="bar",
          marker = list(color= color),
          name = "Person 1") %>%
    layout(yaxis = list(title = 'Number of messages', tick0 = 100, dtick = 300),
           xaxis = list(title = "Date",tickformat="%b<br>%Y"))%>% 
    config(displayModeBar = F)
}

hourActivity <- function(hourRDF,how,start,end){
  hourRDF$date <- as.Date(hourRDF$date)
  
  
  hourRDF %>% 
    filter(year(hourRDF$date) >= start, year(hourRDF$date) <= end) ->
    hourRDF
  
  if(how == "Day"){
    hourRDF$date <- weekdays(hourRDF$date)
    hourRDF %>% 
      group_by(date) %>% 
      summarise(number1 = sum(replace(number1, is.na(number1), 0)),
                number2 = sum(replace(number2, is.na(number2), 0)),
                number3 = sum(replace(number, is.na(number), 0))) -> hourRDF
    colnames(hourRDF)[1] <- "Day"
    
    hourRDF$Day <- factor(hourRDF$Day, levels =  c("Monday", "Tuesday",
                                           "Wednesday", "Thursday",
                                           "Friday", "Saturday", "Sunday"))
  } else if(how == "Month"){
    hourRDF$date <- month(hourRDF$date)
    hourRDF %>% 
      group_by(date) %>% 
      summarise(number1 = sum(replace(number1, is.na(number1), 0)),
                number2 = sum(replace(number2, is.na(number2), 0)),
                number3 = sum(replace(number, is.na(number), 0)))  -> hourRDF
    colnames(hourRDF)[1] <- "Month"
    hourRDF$Month <- c("January","February","March","April","May","June","July","August","September",
                  "October","November","December")
    hourRDF$Month <- factor(hourRDF$Month, levels =   c("January","February","March","April","May","June","July","August","September",
                                            "October","November","December"))
  }else {
    hourRDF %>% 
      group_by(hour) %>% 
      summarise(number1 = sum(replace(number1, is.na(number1), 0)),
                number2 = sum(replace(number2, is.na(number2), 0)),
                number3 = sum(replace(number, is.na(number), 0)))  -> hourRDF
    colnames(hourRDF)[1] <- "Hour"
  }
  
  
  plot_ly(hourRDF,
          x=~get(how),
          y=~number1,
          type="bar",
          marker = list(color= '1E90FF'),
          name = "Kacper") %>% 
    add_trace(x=~get(how),y=~number2 , type="bar", marker = list(color= color2), name = "Dawid") %>% 
    add_trace(x=~get(how), y=~number3, type="bar", marker = list(color= 'EE82EE'), name = "Julia") %>% 
    layout(xaxis = list(title = how,dtick=1),
           yaxis = list(title = 'Number of sent messages'),
           legend=list(title=list(text='Autors:')))%>% 
    config(displayModeBar = F)
  
}


densityPlot <- function(df1,df2,df3, start,end){
  ddf1 <- density(unclass(as.POSIXct(df1$time)),
                  from = min(unclass(as.POSIXct(as.character(start), format = "%Y"))),
                  to = max(unclass(as.POSIXct(as.character(as.numeric(end) + 1), format = "%Y"))))
  ddf2 <- density(unclass(as.POSIXct(df2$time)),
                  from = min(unclass(as.POSIXct(as.character(start), format = "%Y"))),
                  to = max(unclass(as.POSIXct(as.character(as.numeric(end) + 1), format = "%Y"))))
  ddf3 <- density(unclass(as.POSIXct(df3$time)),
                  from = min(unclass(as.POSIXct(as.character(start), format = "%Y"))),
                  to = max(unclass(as.POSIXct(as.character(as.numeric(end) + 1), format = "%Y"))))
  
  ddf1$x <- as.POSIXct(ddf1$x, origin = "1970-01-01")
  ddf2$x <- as.POSIXct(ddf1$x, origin = "1970-01-01")
  ddf3$x <- as.POSIXct(ddf1$x, origin = "1970-01-01")
  
  
  plot_ly() %>% 
    add_trace(x = ddf1$x, y = ddf1$y, type = 'scatter', mode = 'lines', line = list(color= color1), name = "Kacper") %>% 
    add_trace(x = ddf2$x, y = ddf2$y, type = 'scatter', mode = 'lines', line = list(color= color2), name = "Dawid") %>% 
    add_trace(x = ddf3$x, y = ddf3$y, type = 'scatter', mode = 'lines', line = list(color= color3), name = "Julia") %>% 
    layout(xaxis = list(title = "Date"),
           yaxis = list(title = 'Density'),
           legend=list(title=list(text='Autors:')))%>% 
    config(displayModeBar = F)

}


countMessages <- function(dfG){
  count <- length(dfG$sender)
  count
  
}

countWord <- function(dfG){
  number <- length(unlist(strsplit(c(dfG$content), " ")))
  
}


countPhotos <- function(dfG){
  number <- sum(dfG$has_photo)
  number
}
