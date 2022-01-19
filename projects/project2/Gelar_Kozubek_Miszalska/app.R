
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rjson)
library(hms)
library(lubridate)
library(ggiraph)


Load_data <- function(){

    relative_path1 = "../MyData_Hubert" #Path to MyData folder
    relative_path2 = "../MyData_Pawel"
    relative_path3 = "../MyData_Nikola"
    file = "StreamingHistory0.json" # File name
    
    parsed <- fromJSON(file = paste(relative_path1, file, sep = "/"))
    data1<- as.data.frame(do.call(rbind, parsed)) %>% mutate(person = "1")
    
    parsed <- fromJSON(file = paste(relative_path2, file, sep = "/"))
    data2<- as.data.frame(do.call(rbind, parsed)) %>% mutate(person = "2")
    
    parsed <- fromJSON(file = paste(relative_path3, file, sep = "/"))
    data3<- as.data.frame(do.call(rbind, parsed)) %>% mutate(person = "3")
    
    result = bind_rows(data1, data2, data3)
    
    result$msPlayed <- as.numeric(result$msPlayed)
    result$artistName <- as.character(result$artistName)
    result$endTime <- as.POSIXct(as.character(result$endTime))
    result$trackName <- as.character(result$trackName)
    
    return(result)
}
Plot1 <- function(x, n, date){
    n <- as.numeric(n)
    df <- streaming_history
    osoby <- c("Osoba 1", "Osoba 2", "Osoba 3")
    df %>% filter(endTime >=date[1], endTime <=date[2]) %>% 
        filter(endTime >="2021-01-01 00:00", person == n) %>% 
        group_by(artistName) %>% 
        summarise(tim = sum(msPlayed, na.rm = T)) %>% 
        arrange(-tim) %>% 

        slice(1:x) -> d 
    ggplot(data = d, aes(x=reorder(artistName, -tim), y=(tim/60000), data_id = artistName)) + 
        geom_bar_interactive(stat="identity", fill = "#444444") +
    theme(
        panel.background = element_rect(fill = "#1DD05D", colour = "#000000"),
        panel.grid = element_line(colour = "#119942"),
        axis.text.x = element_text(angle = 45, hjust=1)
    ) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(d$tim/60000) *1.1)) +

        labs(title = paste0("Najdłużej słuchani wykonawcy w roku 2021, od ", 1, " do ",x,", osoby: ", osoby[n]),
             y = "Czas słuchania (w minutach)", x = "Wykonawca")  -> g1
    ggiraph(ggobj = g1, width_svg = 13, options = list(opts_selection(type = "single"))) -> g2
    g2 <- girafe_options(g2, opts_toolbar(position = "topright", saveaspng = F))
    g2
}
Plot2 <- function(date){
  streaming_history %>% filter(endTime >=date[1], endTime <=date[2]) %>%  ggplot(aes(x = as_hms(endTime), weight = msPlayed, colour = person, group = person)) + 
    geom_density() + scale_color_discrete() + scale_x_time(breaks = hms::hms(hours = 1:24)) + labs(title = "Odsłuchiwanie utworów w ciągu dnia")
}
Plot3 <- function(selected_artists, date){
  streaming_history %>% filter(endTime >=date[1], endTime <=date[2]) %>% mutate(weekday = lubridate::wday(endTime, label = T, week_start = 1), ) %>% 
    group_by(weekday, person) %>%  
    summarise(day_sum = sum(msPlayed)) %>% 
    ggplot(aes(x = weekday,fill = person, y = day_sum)) + 
    geom_col(position = "dodge") + 
    scale_fill_discrete() + 
    labs(title = "Odsłuchiwanie utworów w zależności od dnia tygodnia")
    
}
Plot4 <- function(n, selected_artists, date){
    n <- as.numeric(n)
    df <- streaming_history
    osoby <- c("Osoba 1", "Osoba 2", "Osoba 3")
    df$hour <- substr(df$endTime,12,13)
    df%>% filter(endTime >=date[1], endTime <=date[2]) %>% 
        filter(endTime >="2021-01-01 00:00", person == n) %>% 
        mutate(is_selected = (artistName %in% selected_artists)) -> d
    d_col <-  d %>% 
        group_by(hour, is_selected) %>% 
        summarise(tim = sum(msPlayed, na.rm = T)) %>% 
        arrange(-tim) 
    ggplot() + 
        geom_col(data = d_col, aes(x = hour,y=(tim/60000), color = is_selected), fill = "#444444") +
        theme(
            panel.background = element_rect(fill = "#1DD05D", colour = "#000000"),
            panel.grid = element_line(colour = "#119942"),
            legend.position = "none"
        ) + 
        labs(title = "Czas słuchania ze względu na godzinę",
             y = "Czas słuchania (w minutach)", x = "Godzina")  -> g1
    g1
}
Plot5 <- function(a,n, selected_artists){
  n <- as.numeric(n)
  osoby <- c("Osoba 1", "Osoba 2", "Osoba 3")
  streamingHistory <- streaming_history
  
  Hours <- streamingHistory %>% 
    mutate(date=as.Date(endTime, format = "%Y-%m-%d"), is_selected = (artistName %in% selected_artists))%>%
    filter(date >= "2021-01-01", person == n) %>%
    group_by(date, is_selected) %>% 
    group_by(date = floor_date(date, "week"), is_selected) %>%
    summarize(hours = sum(msPlayed)/360000 ) %>% 
    arrange(date)
  
  df2 <- ggplot(Hours, aes(x = date, y = hours, color = is_selected)) + 
    geom_col() +
    labs(title ="Aktywność na Spotify osoby", osoby[n], x= "Okres", y= "Godziny przesłuchanej muzyki w skali tygodnia") + 
    theme(panel.background = element_rect(fill = "#1DD05D", colour = "#000000"),
          panel.grid = element_line(colour = "#119942"), legend.position = "none") +
    scale_x_date(limits = as.Date(a))
  df2
}
Plot6 <- function(x, n, date){
  n <- as.numeric(n)
  osoby <- c("Osoba 1", "Osoba 2", "Osoba 3")
  
  Track <- streaming_history%>% filter(endTime >=date[1], endTime <=date[2])%>%
    filter(person == n)%>%
    group_by(trackName)%>%
    summarize(number = n())%>%
    arrange(desc(number))%>%
    slice_head(n = x)
  
  ggplot(data=Track, aes(x=reorder(trackName,-number), y=number))+
    geom_bar(stat="identity", fill = "#444444") +
    theme(
      panel.background = element_rect(fill = "#1DD05D", colour = "#000000"),
      panel.grid = element_line(colour = "#119942"),
      axis.text.x = element_text(angle = 45, hjust=1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(Track$number) *1.1)) +
    labs(title = paste0("Najczęściej dosłuchiwane utwory"),
         y = "Liczba odtworzeń", x = "Utwór")  -> g3
  g3
}

TimeSpent <- function(n,date){
  n <- as.numeric(n)
  df <- streaming_history
  osoby <- c("Osoba 1", "Osoba 2", "Osoba 3")
  df %>%
    filter(endTime >=date[1], endTime <=date[2]) %>%
    filter(endTime >="2021-01-01 00:00", person == n)-> d
  round(sum(d$msPlayed)/60000, digits = 0) -> min
  return(as.character(min))
  
  }


streaming_history <- Load_data()

ui <- dashboardPage(
  dashboardHeader(title = "Spotify"),
  dashboardSidebar(
    setSliderColor(c("#1DD05D", "#1DD05D", "#1DD05D"), c(1, 2, 3)),
            selectInput("osoba", h4("Osoba"), 
                        choices = list("Osoba 1" = 1, "Osoba 2" = 2,
                                       "Osoba 3" = 3), selected = 1),
            sliderInput("x_wykonawcow",
                        h4("Liczba uworów/wykonawców:"),
                        min = 1,
                        max = 21,
                        value = 10),
            sliderInput("slider",
                        "Dates:",
                        min = as.Date("2021-01-01","%Y-%m-%d"),
                        max = as.Date("2021-12-31","%Y-%m-%d"),
                        value=as.Date(c("2021-01-01","2021-12-31")),
                        timeFormat="%Y-%m-%d")

            
        ),
        

  dashboardBody(
    fluidRow(
      tabBox(
        tabPanel("Utwory",plotOutput("Tracks")),
        tabPanel("Wykonawcy",girafeOutput("Top10"))),
      box(plotOutput("hours"))),
    fluidRow(
           box(plotOutput("TimeDensity")),
           valueBoxOutput("TimeBox")),
    fluidRow(
           box(plotOutput("Weekdays")),
           box(plotOutput("Time"))),
           
           setBackgroundImage(
             src = "https://img.freepik.com/free-vector/abstract-background-with-spotify-logo_52683-26645.jpg?size=626&ext=jpg",
             shinydashboard = TRUE
           )
        ),
  skin = "green"
)



server <- function(input, output) {
  
  
  

  output$Top10 <- renderGirafe({
    Plot1(input$x_wykonawcow, input$osoba, input$slider)
  })
  
  output$TimeDensity <- renderPlot({
    Plot2(input$slider)
  })
  
  output$Weekdays <- renderPlot({

    Plot3(selected_artists(), input$slider)
  })
  
  output$hours <- renderPlot({
    Plot4(input$osoba, selected_artists(),input$slider)

  })
  
  output$Time <- renderPlot({
    Plot5(input$slider, input$osoba, selected_artists())
    
  })
  
  output$Tracks <- renderPlot({
    Plot6(input$x_wykonawcow, input$osoba,input$slider)
  })
  output$TimeBox <- renderValueBox({
    valueBox(
      TimeSpent(input$osoba,input$slider), "Minuty słuchania", icon = icon("time", lib = "glyphicon"),
      color = "green"
    )
  })
  
  selected_artists <- reactive({
    input$Top10_selected
  })
  
  
}





shinyApp(ui = ui, server = server)
