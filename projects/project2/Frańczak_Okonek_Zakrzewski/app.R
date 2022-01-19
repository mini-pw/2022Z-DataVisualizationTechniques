library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes) # czerwony motyw
library(shinyWidgets) # zmiana stylu sliderów

##########################################################
# Przygotowanie danych
##########################################################

# łączenie plików .csv w jedną ramkę danych

history <- bind_rows(
  readr::read_csv(file.path("G_Z", "history.csv")),
  readr::read_csv(file.path("P_O", "history.csv")),
  readr::read_csv(file.path("K_F", "history.csv")),
  .id = "osoba"
)

channels <- bind_rows(
  readr::read_csv(file.path("G_Z", "channels.csv")),
  readr::read_csv(file.path("P_O", "channels.csv")),
  readr::read_csv(file.path("K_F", "channels.csv")),
  .id = "osoba"
)

videos <- bind_rows(
  readr::read_csv(file.path("G_Z", "videos.csv")),
  readr::read_csv(file.path("P_O", "videos.csv")),
  readr::read_csv(file.path("K_F", "videos.csv")),
  .id = "osoba"
)
 
## df - główna ramka danych 
 
df <- inner_join(history, channels %>% select(!c(title, osoba)), by = c('channelId' = 'id')) %>% 
  inner_join(videos %>% select(!osoba), by = c('titleId' = 'id'), suffix = c('.channel', '.video') )

## kolory
grzegorz <- "maroon"
piotr <- "slateblue4"
kinga <- "dodgerblue"
color <- c(grzegorz, piotr, kinga)

## skala
potega <- seq(0,12,3)
skrot <- c("", "tys.", "mln", "mld", "bln")
wartosci <- data.frame(potega = potega, skrot = skrot)

znajdz_skale <-function(w){
  max <- max(w)/4
  p <- 0
  while(max >= 10){
    p <- p + 1
    max <- max/10
  }
  p <- p - p %% 3
  wartosci %>% filter(potega == p)
}

###########################
## do wykresu 1.

plot1_df <- df %>%
  mutate(day = lubridate::as_date(time)) %>%
  mutate(hour = lubridate::local_time(time, units = "hours", tz = "Europe/Warsaw"))

###########################
## do wykresu 2.

problem<-list(
  "videos"=list("viewCount","likeCount","commentCount"),
  "channels"=list("viewCount","subscriberCount","videoCount")
)
# notacja w postaci 5x10^4
fancy_scientific <- function(l) {
  l <- format(l, scientific = TRUE)
  l <- gsub("0e\\+00","0",l)
  l <- gsub("^(.*)e", "'\\1'e", l)
  l <- gsub("e", "%*%10^", l)
  parse(text=l)
}

##########################################################
# Server, czyli rysowanie wykresów
##########################################################

server <- function(input, output, session) {
  
  observe({
    tmp <- plot1_df %>% 
      filter(osoba %in% input$osoba)
    
    minDay = min(tmp$day)
    maxDay = max(tmp$day)

    updateSliderInput(session, "dzien",
      min = minDay,
      max = maxDay,
      value = c(max(maxDay - 14, minDay), max(tmp$day)), 
    )
  })
  
  output$plot1 <- renderPlot({
    col <- data.frame(color = color,
                      name = 1:3,
                      label = c("Grzegorz", "Piotr", "Kinga"))
    col <- col %>%
      filter(name %in% input$osoba)
    
    minDay = input$dzien[[1]]
    maxDay = input$dzien[[2]]

    plot1_df %>%
      filter(osoba %in% input$osoba) %>% 
      filter(day >= minDay & day <= maxDay) %>% 
      ggplot(aes(x = day, y = hour, color = osoba)) +
      geom_point(size = 4) +
      scale_y_continuous(limits = c(0, 24), expand = c(0, 0), breaks = seq(0, 24)) +
      scale_x_date(date_labels="%d %b", date_breaks = "1 day") +
      scale_color_manual(
        labels = col$label,
        values = col$color
      ) +
      labs(
        x = NULL,
        y = "Godzina",
        color = "Osoba"
      ) +
      theme(
        text = element_text(size = 20, family = "sans"),
        panel.grid.minor = element_blank())
  })
  
  ###########################
  
  
  observe({
    
    vector1<-c("Liczba wyświetleń","Liczba polubień","Liczba komentarzy")
    vector2<-c("Liczba wyświetleń","Liczba subskrypcji","Liczba filmów na kanale")
    
    if(input$rodzaj=="channels"){
      updateSelectInput(session, "typ", choices =setNames( problem[[input$rodzaj]],vector2))
    }
    else{
      updateSelectInput(session, "typ", choices =setNames( problem[[input$rodzaj]],vector1))
    }
    
    
  })
  
  output$plot2 <- renderPlot({
    tmp<-get(input$rodzaj) %>% 
      filter(get(input$typ)<quantile(get(input$typ),input$przedzial)) %>% 
      filter(osoba == input$osoba2)
    
    ggplot(tmp,aes(x=get(input$typ)))+
      geom_histogram(fill="black", col="grey",boundary=0,bins=input$bins)+
      scale_x_continuous(labels = fancy_scientific)+
      labs(
        x=input$typ,
        title="Histogram statystyk związanych z oglądanymi przez nas kanałami i filmami"
      )
    
    
  })
  
  ###########################
  
  output$plot3 <- renderPlot({
   
     col <- data.frame(color = color,
                      name = 1:3,
                      label = c("Grzegorz", "Piotr", "Kinga"))
    col <- col %>%
      filter(name %in% input$osoba3)
    
    options(scipen = 999)
    
    if(input$channel == "Wyświetlenia"){
      channel <- "viewCount.channel"
    }else if(input$channel == "Liczba subskrybcji"){
      channel <- "subscriberCount"
    }else{
      channel <- "videoCount"
    }
    
    if(input$video == "Wyświetlenia"){
      video <- "viewCount.video"
    }else if(input$video == "Polubienia"){
      video <- "likeCount"
    }else{
      video <- "commentCount"
    }
    
    tmp <- df %>% 
      filter(osoba %in% input$osoba3) 
    tmp <- tmp%>% 
      filter(tmp[[video]]<=quantile(tmp[[video]] ,input$zakres))
    
    wart_y <- znajdz_skale(tmp[[channel]])
    wart_x <- znajdz_skale(tmp[[video]])
    
    plot <- tmp %>% 
      ggplot(aes(y = tmp[[channel]], x = tmp[[video]], color = osoba)) +
      geom_count() + 
      ylab(paste(input$channel, " [",wart_y$skrot,"]", sep = "" )) + 
      xlab(paste(input$video, " [",wart_x$skrot,"]", sep = "" )) + 
      scale_x_continuous(breaks = seq(0, max(tmp[[video]]), by = max(tmp[[video]])/4),
                         labels = signif(seq(0, max(tmp[[video]]), 
                                      by = max(tmp[[video]])/4)/10^(wart_x$potega), 3)) +
      scale_y_continuous(breaks = seq(0, max(tmp[[channel]]), by = max(tmp[[channel]])/4),
                         labels = signif(seq(0, max(tmp[[channel]]), 
                                            by = max(tmp[[channel]])/4)/10^(wart_y$potega),2)) +
      scale_color_manual(
        labels = col$label,
        values = col$color, 
        name = "Osoba"
      ) +
      scale_size_continuous(name = "Wyświetlenia") + 
      labs(
        title = paste("Zależność pomiędzy parametrem", tolower(input$channel), "dla kanału a",
                         tolower(input$video), "dla filmu"))
    plot
  })
}

##########################################################
# UI
##########################################################

ui1 <- fluidPage(
    chooseSliderSkin("Flat"),
    
    titlePanel("O jakiej porze dnia oglądamy filmy na YT?"),

    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "osoba",
          "Wybierz osobę",
          inline = TRUE,
          selected = c("1", "2", "3"),
          choiceNames = c("Grzegorz", "Piotr", "Kinga"),
          choiceValues = c("1", "2", "3")
        ),
        
        width = 3
      ),
      mainPanel(
        
      ),
      fluid = FALSE
    ),
    
    fixedRow(
      column(12,
        plotOutput("plot1", height = "500px"),
        sliderInput(
          "dzien",
          label = NULL,
          min = min(plot1_df$day),
          max = max(plot1_df$day),
          value = c(min(plot1_df$day), max(plot1_df$day)),
          step = 1,
          width = "93%",
          timeFormat = "%d %b"
        )
      ),
      
      div("Przedział czasowy", style = "text-align: center; font-size: 1.5em; width: 93%")
    ),
    
)

###########################

ui2 <- fluidPage(
  titlePanel("Jakie filmy i kanały oglądamy najczęściej?"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("osoba2",
                   "Wybierz osobę:", 
                   selected = c("1"),
                   inline = TRUE,
                   choiceNames = c("Grzegorz", "Piotr", "Kinga"),
                   choiceValues = c("1", "2", "3")),
      sliderInput("bins",
                  "Liczba kubełków:",
                  min = 1,
                  max = 50,
                  value = 30),
      selectInput("rodzaj",
                  "Wybierz rodzaj: ",
                  choices=setNames(names(problem),c("filmy","kanały")) 
      ),
      selectInput("typ","Wybierz typ: ", problem[[1]]),
      sliderInput("przedzial",
                  "Jaką część wyników (kwantyl) chcesz zobaczyć?",
                  min=0,
                  max=1,
                  value=0.75)
    ),
    
    mainPanel(
      plotOutput("plot2")
    )
  )
)

###########################

ui3 <- fluidPage(
  titlePanel("Z jakich kanałów pochodzą filmy, które oglądamy"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "osoba3",
        "Wybierz osobę",
        selected = c("1", "2", "3"),
        inline = TRUE,
        choiceNames = c("Grzegorz", "Piotr", "Kinga"),
        choiceValues = c("1", "2", "3")
      ),
      selectInput("channel",
                  "Wybierz parametr dla kanału: ",
                  c("Wyświetlenia", "Liczba subskrybcji", "Liczba filmów")
      ),

      selectInput("video",
                  "Wybierz parametr dla filmu: ", 
                  c("Wyświetlenia", "Polubienia", "Liczba komentarzy")),
      
      sliderInput("zakres",
                  "Jaką część wyników (kwantyl) chcesz zobaczyć?",
                  min=0,
                  max=1,
                  value=0.95)  
    ),
    
    
    mainPanel(
      plotOutput("plot3")
    )
  )
)

##########################################################

app_ui <- navbarPage(
  theme = shinytheme("united"),
  title = div(icon("youtube"), "Project 2"),
  tabPanel("Kiedy oglądamy?", ui1, icon = icon("clock")),
  tabPanel("Jakie filmy?", ui2, icon = icon("star")),
  tabPanel("Z jakich kanałów?", ui3, icon = icon("thumbs-up")),
)

shinyApp(ui = app_ui, server = server)
