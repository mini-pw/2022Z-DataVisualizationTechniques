library(shiny)
library(plotly)
library(tidyr)
library(readxl)
library(dplyr)
library(Rcpp)
library(ggplot2)
library(ggrepel)
library(shinythemes)
library(rsconnect)
library(stringr)


df_M <- read.csv("Data/Michal_telefon.csv") %>% 
  mutate(data = as.Date(date)) %>% 
  filter(data >= as.Date("2021-12-20"), data <= as.Date("2022-01-08")) %>% 
  select( -data)
df_D <- read.csv("Data/ComputerData.csv")
df_D2 <- read.csv("Data/TelephoneData.csv")
dfpc <- df_D
dftel <- df_D2
dfpc1 <- dfpc %>% 
  filter(data.app %in% c("chrome.exe", "firefox.exe"))
dfpc1$date <- as.Date(dfpc1$date)

df_J <- df_D
df_J2 <- df_D2
df_D <- df_D %>%
  group_by(data.app, date, name) %>%
  summarise(time = sum(duration))
df_D2 <- df_D2 %>%
  group_by(data.app, date, name) %>%
  summarise(time = sum(duration))
df_M2 <- df_M %>% select(data.app, date, name, time = duration)
df_D2 <- rbind(df_D2, df_M2)
df_D$date = as.Date(df_D$date)
df_D2$date = as.Date(df_D2$date)
df_J3 <- read.csv("Data/UnlockData.csv")
df_J3$date <- as.Date(df_J3$date)
df_J3$time <- as.POSIXct(df_J3$time)

df_J2 <- df_J2 %>% 
  mutate(hours = substring(timestamp, first = 1, last = 14))
df_J2$hours <- as.POSIXct(paste(str_replace(df_J2$hours, "T", " "), "00:00", sep = ""))
df_J2$date <- as.Date(df_J2$date)

df_J <- df_J %>% 
  mutate(hours = substring(timestamp, first = 1, last = 14))
df_J$hours <- as.POSIXct(paste(str_replace(df_J$hours, "T", " "), "00:00", sep = ""))
df_J$date <- as.Date(df_J$date)

df_J3 <- df_J3 %>% 
  mutate(hours = substring(timestamp, first = 1, last = 14))
df_J3$hours <- as.POSIXct(paste(str_replace(df_J3$hours, "T", " "), "00:00", sep = ""))

#df_M$date <- as.Date(df_M$date)

uiTime <- fluidPage(
  
  titlePanel("Czas spêdzony na korzystaniu z urz¹dzeñ„"),
  
  h6(""),
  
  fluidRow(style = "background-color:#393939;",
           column(7, 
                  plotly::plotlyOutput('plotTime', height = 500)),
           column(5,
                  align = "center",
                  h4(textOutput("text")),
                  h6("Czas wyra¿ony w minutach"),
                  tableOutput("table")
           )),
  
  h6(""),
  
  fluidRow(style = "background-color:#303030;",
           align = "center",
           column(6,
                  dateRangeInput(
                    inputId = "dataJ", 
                    label = "Zakres:",
                    start = "2021-12-19",
                    end = "2022-01-08"
                  )),
           column(6, style = "height:120px;",
                  h6(""),
                  radioButtons(inputId = "type",
                               label = "Rodzaj danych:",
                               choices = c("Z telefonu", "Z komputera"),
                               selected = "Z telefonu"))
           
  ),
  
  fluidRow(style = "background-color:#303030;",
           align = "center",
           column(6, 
                  checkboxGroupInput(inputId = "name",
                                     label = "Osoba:",
                                     choices = c("Dominik", "Michal", "Jakub"),
                                     selected = "Michal")),
           column(6, 
                  conditionalPanel(
                    condition = "input.type == \"Z komputera\"",
                    style = "height:120px;",
                    h6(""),
                    radioButtons(inputId = "seq",
                                 label = "Interwa³y:",
                                 choices = c("Dziennie", "Godzinowo"),
                                 selected = c("Dziennie")))
           )),
  h2(" ")
)

uiUnlocking <- fluidPage(
  
  titlePanel("Odblokowywanie telefonu"),
  
  h5(""),
  
  fluidRow(style = "background-color:#393939;",
           plotly::plotlyOutput("plotUnlocking2")),
  
  fluidRow(style = "background-color:#393939;",
           h6(""),
           column(4,
                  radioButtons(inputId = "seq2",
                               label = "Interwa³y:",
                               choices = c("Dziennie", "Godzinowo"),
                               selected = c("Dziennie"))),
           column(4, 
                  checkboxGroupInput(inputId = "name2",
                                     label = "Osoba:",
                                     choices = c("Dominik", "Michal", "Jakub"),
                                     selected = "Jakub")),
           column(4,
                  dateRangeInput(
                    inputId = "dataJ2", 
                    label = "Zakres:",
                    start = "2021-12-19",
                    end = "2022-01-09"))
  ),
  h3(""),
  
  fluidRow(style = "background-color:#393939;",
           plotly::plotlyOutput("plotUnlocking", height = 250)),
  
  fluidRow( style = "background-color:#393939;",
            column(12, align = "center",
                   h6(""),
                   dateInput(inputId = "data",
                             label = "Data:",
                             value = "2021-12-20",
                             min = "2021-12-20",
                             max = "2022-01-08"
                   ))),
  h3("")
  
)


###
ui_D <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Czas spêdzony ka¿ego dnia na danej aplikacji na komputerze oraz na 
                telefonie wyra¿ony w minutach"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "Osoba",
        label = "Osoba:",
        choices = c("Jakub", "Michal", "Dominik"),
        selected = "Jakub"
      ),
      selectInput(
        inputId = "AppKom1",
        label = "Wybrana aplikacja na komputerze:",
        choices = unique(filter(df_D, name == 'Jakub')$data.app),
        selected = "Teams.exe"
      ),
      dateRangeInput(
        inputId = "data1", 
        label = "Zakres",
        start = "2021-12-19",
        end = "2022-1-8"
      ),
      h4(""),
      selectInput(
        inputId = "AppTel1",
        label = "Wybrana aplikacja na telefonie:",
        choices = unique(filter(df_D2, name == 'Jakub')$data.app),
        selected = "TikTok"
      ),
      dateRangeInput(
        inputId = "data2", 
        label = "Zakres",
        start = "2021-12-19",
        end = "2022-1-8"
      ),
    ),
    
    mainPanel(
      plotlyOutput("plot1"),
      
      plotlyOutput("plot2"),
    )
  )
)


uiWebsites1 <- fluidPage(
  titlePanel("Most popular websites and time spent on them"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "person",
        label = "Select person",
        choices = unique(dfpc$name),
        selected = "Jakub"
      ),
      selectInput(
        inputId = "website",
        label = "Select website",
        #MusiaÅ‚em rÄ™cznie wybieraÄ‡ i dodawaÄ‡ strony, bo nie ma Å¼adnej konwencji
        #co do nazwy strony w data.title :(
        choices = c("Facebook", "Wikipedia", "Twitter", "Github", "YouTube",
                    "Netflix", "Google", "All websites"),
        selected = "Facebook"
        
      ),
      sliderInput(
        inputId = "slider",
        label = "Choose range",
        min(dfpc1$date),
        max(dfpc1$date),
        value = c(min(dfpc1$date),max(dfpc1$date))
      )
    ),
    mainPanel(
      plotlyOutput("plotWebsites")
    )
  )
)

ui_Michal <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Czas spêdzony dziennie na poszczególnych stronach
             internetowych oraz zale¿noœæ d³ugoœci u¿ywania aplikacji
             do listy poszczególnych uruchomieñ jej"),
  fluidRow(style = "background-color:#393939;",
           plotlyOutput("plot1_M")
  ),
  fluidRow(style = "background-color:#393939;",
           column(3,
                  selectInput(
                    inputId = "website_M",
                    label = "Wybierz stronê™",
                    choices = c("Facebook", "Wikipedia", "Twitter", "Github", "YouTube",
                                "Netflix", "Google", "Wszystkie strony"),
                    selected = "Facebook"
                  ),
           ),
           column(3,
                  selectInput(
                    inputId = "person_M",
                    label = "Wybierz osobê™",
                    choices = unique(dfpc$name),
                    selected = "Jakub"
                  )
           ),
           column(6,
                  sliderInput(
                    inputId = "slider_M",
                    label = "Wybierz okres czasu",
                    min(dfpc1$date),
                    max(dfpc1$date),
                    value = c(min(dfpc1$date),max(dfpc1$date))
                  )
           )
  ),
  fluidRow(style = "background-color:#393939;",
           plotlyOutput("plot2_M")
  ),
  fluidRow(style = "background-color:#393939;",
           column(3,
                  selectInput(
                    inputId = "person1_M",
                    label = "Wybierz osobÄ™",
                    choices = unique(dfpc$name),
                    selected = "Jakub"
                  )
           ),
           column(3,
                  selectInput(
                    inputId = "Device_M",
                    label = "Wybierz urzÄ…dzenie",
                    choices = c("PC", "Telefon"),
                    selected = "PC"
                  )
           ),
           column(6,)
  ),
  fluidRow(style = "background-color:#393939;",
           column(12, align = "center",
                  h6(""),
                  dateInput(inputId = "data_M",
                            label = "Data:",
                            value = "2021-12-22",
                            min = "2021-12-22",
                            max = "2022-01-08"
                  )))
)

server <- function(input, output) {
  
  output$plotTime <- renderPlotly({
    if (is.null(input$name)) {
      df <- as.data.frame(t(c("Wybierz osobê", 1, 1)))
      p <- ggplot(df) +
        geom_text(aes(x = V2, y = V2, label = V1), size = 10, color = "white") +
        theme(
          panel.background = element_rect(fill = '#303030', colour = '#303030'),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.background =element_rect(fill = '#303030', colour = '#303030'),
        )
      ggplotly(p,
               tooltip = NULL) %>% 
        layout(yaxis = list(fixedrange = TRUE), xaxis = list(fixedrange = TRUE)) %>% 
        config(modeBarButtonsToRemove = c("lasso2d", "pan2d", 
                                          "hoverClosestCartesian", "hoverCompareCartesian"))
      
    } else {
      if (input$type == "Z telefonu") {
        df <- df_J2
        dev <- "telefonie"
        df <- df %>% 
          filter(#name %in% input$name, 
            date >= input$dataJ[1], date <= input$dataJ[2]) %>% 
          group_by(date, name) %>% 
          summarise(time = sum(duration)/60, .groups = "drop") %>% 
          complete(date, name, fill = list(time = 0))
        df_M1 <- df_M %>% filter(date >= input$dataJ[1], date <= input$dataJ[2]) %>% 
          group_by(date,name) %>% 
          summarise(time = sum(duration)/60, .groups = "drop")
        if (input$type == "Z telefonu"){df <- rbind(df, df_M1)}
        #CzÄ™Å›Ä‡ dla danych z Iphone'a
        #if((input$type == "Z telefonu")&(input$name == "Michal")){
        #  df1 <- df_M %>% group_by(date,name) %>% 
        #    summarise(time = sum(duration)/60, .groups = "drop") %>% 
        #    complete(date, name, fill = list(time = 0))
        #  df <- rbind(df,df1)
        # }
        
        p <- ggplot(df %>% filter(name %in% input$name)) +
          geom_col(aes(x = date, y = time, fill = name,
                       text = paste("Czas: ", round(time, 3), "<br>Data: ", date, "<br>Osoba: ", name)),
                   color = "white", position = "dodge") +
          scale_fill_manual(values = c("navyblue", "#656561", "#a20cad")) +
          labs(title = paste("Czas spêdzony na", dev, "dziennie", sep = " "),
               x = "Data",
               y = "Czas w minutach:",
               color = "Osoba:") + 
          theme_gray() +
          theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
                plot.background = element_rect(fill = '#303030', colour = '#303030'),
                legend.background = element_rect(fill = '#303030', colour = '#303030')) +
          theme(text = element_text(color = "white"))  +
          theme(text = element_text(color = "white"),
                axis.text.x =  element_text( color = "white"),
                axis.text.y =  element_text(color = "white"))
        ggplotly(p,
                 tooltip = "text") %>% 
          layout(yaxis = list(fixedrange = TRUE))
      }
      else {
        df <- df_J
        dev <- "komputerze"
        if (input$seq == "Dziennie") {
          
          df <- df %>% 
            filter(#name %in% input$name, 
              date >= input$dataJ[1], date <= input$dataJ[2]) %>% 
            group_by(date, name) %>% 
            summarise(time = sum(duration)/60, .groups = "drop") %>% 
            complete(date, name, fill = list(time = 0))
          df_M1 <- df_M %>% filter(date >= input$dataJ[1], date <= input$dataJ[2]) %>% 
            group_by(date,name) %>% 
            summarise(time = sum(duration)/60, .groups = "drop")
          if (input$type == "Z telefonu"){df <- rbind(df, df_M1)}
          #CzÄ™Å›Ä‡ dla danych z Iphone'a
          #if((input$type == "Z telefonu")&(input$name == "Michal")){
          #  df1 <- df_M %>% group_by(date,name) %>% 
          #    summarise(time = sum(duration)/60, .groups = "drop") %>% 
          #    complete(date, name, fill = list(time = 0))
          #  df <- rbind(df,df1)
          # }
          
          p <- ggplot(df %>% filter(name %in% input$name)) +
            geom_col(aes(x = date, y = time, fill = name,
                         text = paste("Czas: ", round(time, 3), "<br>Data: ", date, "<br>Osoba: ", name)),
                     color = "white", position = "dodge") +
            scale_fill_manual(values = c("navyblue", "#656561", "#a20cad")) +
            labs(title = paste("Czas spêdzony na", dev, "dziennie", sep = " "),
                 x = "Data",
                 y = "Czas w minutach:",
                 color = "Osoba:") + 
            theme_gray() +
            theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
                  plot.background = element_rect(fill = '#303030', colour = '#303030'),
                  legend.background = element_rect(fill = '#303030', colour = '#303030')) +
            theme(text = element_text(color = "white"))  +
            theme(text = element_text(color = "white"),
                  axis.text.x =  element_text( color = "white"),
                  axis.text.y =  element_text(color = "white"))
          ggplotly(p,
                   tooltip = "text") %>% 
            layout(yaxis = list(fixedrange = TRUE))
          
        }
        else {
          df <- df %>% 
            filter(name %in% input$name,
                   date >= input$dataJ[1], date <= input$dataJ[2]) %>% 
            group_by(hours, name) %>% 
            summarise(time = sum(duration)/60, .groups = "drop") %>% 
            complete(hours, name, fill = list(time = 0))
          
          roznica <- 0
          new_time <- c()
          for (i in 1:length(df$time)) {
            t <- df$time[i] + roznica
            if (t > 60) {
              roznica <- t - 60
              t <- 60
            }
            new_time <- c(new_time, t)
          }
          df$time <- new_time
          
          p <- ggplot(df) +
            geom_col(aes(x = hours, y = time, fill = name,
                         text = paste("Czas: ", round(time, 3), "<br>Godzina: ", hours, "<br>Osoba: ", name)), 
                     color = "white", position = "dodge") +
            scale_fill_manual(values = c("navyblue", "#656561", "#a20cad")) +
            labs(title = paste("Czas spêdzony na", dev, "godzinowo", sep = " "),
                 x = "Godziny",
                 y = "Czas w minutach",
                 color = "Osoba:") + 
            theme_gray() +
            theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
                  plot.background = element_rect(fill = '#303030', colour = '#303030'),
                  legend.background = element_rect(fill = '#303030', colour = '#303030')) +
            theme(text = element_text(color = "white")) +
            theme(text = element_text(color = "white"),
                  axis.text.x =  element_text( color = "white"),
                  axis.text.y =  element_text(color = "white"))
          ggplotly(p,
                   tooltip = "text") %>% 
            layout(yaxis = list(fixedrange = TRUE))
        }}
    }
    
  })
  
  output$plotUnlocking <- renderPlotly({
    p <- ggplot(df_J3 %>% 
                  filter(date == input$data)) +
      geom_point(aes(x = time, y = name, color = name,
                     text = paste("Czas: ", time, "<br>Osoba: ", name))) +
      scale_color_manual(values = c("#3ba8fc", "#656561", "#a20cad")) +
      scale_x_datetime(limits = c(as.POSIXct(paste(input$data, "00:00:00", sep = " ")), 
                                  as.POSIXct(paste(input$data, "23:59:59", sep = " ")))) + 
      labs(title = paste("Pojedyncze odblokowania telefonu w dniu",
                         as.character(input$data), sep = " "),
           x = "Godzina",
           y = "Osoba") + 
      theme_gray() +
      theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
            plot.background = element_rect(fill = '#303030', colour = '#303030'),
            legend.position = "none") +
      theme(text = element_text(color = "white")) +
      theme(text = element_text(color = "white"),
            axis.text.x =  element_text( color = "white"),
            axis.text.y =  element_text(color = "white"))
    ggplotly(p,
             tooltip = "text") %>% 
      layout(yaxis = list(fixedrange = TRUE))
    
  })
  
  output$plotUnlocking2 <- renderPlotly({
    if (is.null(input$name2)) {
      df <- as.data.frame(t(c("Wybierz osobÄ™!", 1, 1)))
      p <- ggplot(df) +
        geom_text(aes(x = V2, y = V2, label = V1), size = 10, color = "white") +
        theme(
          panel.background = element_rect(fill = '#303030', colour = '#303030'),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.background =element_rect(fill = '#303030', colour = '#303030'),
        )
      ggplotly(p,
               tooltip = NULL) %>% 
        layout(yaxis = list(fixedrange = TRUE), xaxis = list(fixedrange = TRUE)) %>% 
        config(modeBarButtonsToRemove = c("lasso2d", "pan2d", 
                                          "hoverClosestCartesian", "hoverCompareCartesian"))
    } else {
      if (input$seq2 == "Dziennie") {
        df = df_J3 %>% 
          filter(name %in% input$name2,
                 date >= input$dataJ2[1], date <= input$dataJ2[2]) %>% 
          group_by(date, name) %>% 
          summarise(Odblokowania = n(), .groups = "drop") %>% 
          complete(date, name, fill = list(Odblokowania = 0))
        p <- ggplot(df) +
          geom_col(aes(x = date, y = Odblokowania, fill = name,
                       text = paste0("Odblokowania: ", Odblokowania,
                                     "<br>Data: ", date, "<br>Osoba: ", name)),
                   color = "white", position = "dodge") +
          scale_fill_manual(values = c("navyblue", "#656561", "#a20cad")) +
          labs(title = "Suma odblokowañ dziennie",
               x = "Data",
               y = "Liczba odblokowañ",
               fill = "Osoba:") + 
          theme_gray() +
          theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
                plot.background = element_rect(fill = '#303030', colour = '#303030'),
                legend.background = element_rect(fill = '#303030', colour = '#303030')) +
          theme(text = element_text(color = "white"))  +
          theme(text = element_text(color = "white"),
                axis.text.x =  element_text( color = "white"),
                axis.text.y =  element_text(color = "white"))
        ggplotly(p, 
                 tooltip = "text") %>% 
          layout(yaxis = list(fixedrange = TRUE))
      } else {
        df = df_J3 %>% 
          filter(name %in% input$name2,
                 date >= input$dataJ2[1], date <= input$dataJ2[2]) %>% 
          group_by(hours, name) %>% 
          summarise(Odblokowania = n(), .groups = "drop") %>% 
          complete(hours, name, fill = list(Odblokowania = 0))
        p <- ggplot(df) +
          geom_col(aes(x = hours, y = Odblokowania, fill = name,
                       text = paste0("Odblokowania: ", Odblokowania,
                                     "<br>Godzina: ", hours, "<br>Osoba: ", name)),
                   color = "white", position = "dodge") +
          scale_fill_manual(values = c("#104ea2", "#656561", "#a20cad")) +
          labs(title = "Suma odblokowañ„ godzinowo",
               x = "Data",
               y = "Liczba odblokowañ",
               fill = "Osoba:") + 
          theme_gray() +
          theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
                plot.background = element_rect(fill = '#303030', colour = '#303030'),
                legend.background = element_rect(fill = '#303030', colour = '#303030')) +
          theme(text = element_text(color = "white")) +
          theme(text = element_text(color = "white"),
                axis.text.x =  element_text( color = "white"),
                axis.text.y =  element_text(color = "white"))
        ggplotly(p,
                 tooltip = "text") %>% 
          layout(yaxis = list(fixedrange = TRUE))
      }
    }
  })
  
  ###
  output$plot1 <- renderPlotly({
    txt <-  paste("Czas spêdzony na komputerze na",input$AppKom1, sep=" ")
    p <- ggplot(filter(df_D, name == input$Osoba  & data.app == input$AppKom1), 
                aes(x=date, y=time/60), group=1) +
      geom_line(color="#375a7f", size = 1.5)+
      geom_point(aes(text = paste0("Data: ", date, "<br>Czas: ",  round(time/60, 2))),
                 color = "white", size = 2) +
      labs(x = "Data", y = "Czas",
           title = txt )+
      #scale_x_date(limits = as.Date(c("2021-12-19","2021-12-30")))
      scale_x_date(limits = input$data1)+
      theme_gray()+
      theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
            plot.background = element_rect(fill = '#303030', colour = '#303030'))+
      theme(text = element_text(size=12, color = "white"),
            axis.text.x =  element_text( color = "white"),
            axis.text.y =  element_text( color = "white"))
    
    ggplotly(p,
             tooltip = "text") %>% 
      layout(yaxis = list(fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
  })
  
  output$plot2 <- renderPlotly({
    txt <-  paste("Czas spêdzony na telefonie na",input$AppTel1, sep=" ")
    p <- ggplot(filter(df_D2, name == input$Osoba  & data.app == input$AppTel1), 
                aes(x=date, y= time/60)) +
      geom_line(color="#375a7f", size = 1.5)+
      geom_point(aes(text = paste0("Data: ", date, "<br>Czas: ", round(time/60, 2))), color = "white", size = 2)+
      labs(x = "Data", y = "Czas",
           title = txt)+
      #scale_x_date(limits = as.Date(c("2021-12-19","2021-12-30")))
      scale_x_date(limits = input$data2)+
      theme_gray()+
      theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
            plot.background = element_rect(fill = '#303030', colour = '#303030'))+
      theme(text = element_text(size=12, color = "white"),
            axis.text.x =  element_text(color = "white"),
            axis.text.y =  element_text( color = "white"))
    
    
    ggplotly(p,
             tooltip = "text") %>% 
      layout(yaxis = list(fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
  })
  
  
  observeEvent(input$Osoba, {
    updateSelectInput(inputId = "AppKom1", 
                      choices = unique(filter(df_D, name == input$Osoba)$data.app))
  })
  observeEvent(input$Osoba, {
    updateSelectInput(inputId = "AppTel1", 
                      choices = unique(filter(df_D2, name == input$Osoba)$data.app))
  })
  
  output$plot1_M <- renderPlotly({
    df <- dfpc1
    date <- unique(df$date)
    zeroes <- numeric(length(date))
    tmp <- data.frame(date,zeroes)
    df <- df %>% 
      filter(name == input$person_M)
    if(input$website_M == "Google"){
      df <- df %>% filter(grepl("Szukaj w", data.title) | grepl("Google Search", data.title))
    }
    else if(input$website_M == "Github"){
      df <- df %>% 
        filter(grepl("TWDProject2", data.title) | grepl("mini-pw", data.title))
    }
    else if (input$website_M != "Wszystkie strony"){
      df <- df %>% filter(grepl(input$website_M,data.title))
    }
    df <- df %>% 
      group_by(date) %>% 
      summarise(sum = sum(duration)/60)
    df <- right_join(df, tmp, by = 'date') %>% 
      mutate(suma = case_when(is.na(sum) ~ 0, TRUE ~sum)) %>% 
      filter(date >= input$slider_M[1], date <= input$slider_M[2])
    plot <- ggplot(df, aes(x = date, y = suma, 
                           text = paste0("Data: ", date, "<br>Czas: ", round(suma, 3)))) +
      geom_col(fill = "navyblue", color = "white") +
      labs(title = "Czas spêdzony dziennie na danej stronie internetowej",
           x = "data", y = "czas (w minutach)") +
      theme_gray()+
      theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
            plot.background = element_rect(fill = '#303030', colour = '#303030'))+
      theme(text = element_text(size=10, color = "white"),
            axis.text.x =  element_text(size=10, color = "white", angle = 45),
            axis.text.y =  element_text(size=10, color = "white"),
            plot.title = element_text(hjust = 0.5))
    ggplotly(plot,
             tooltip = "text") %>% 
      layout(yaxis = list(fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
  })
  
  output$plot2_M <- renderPlotly({
    if (input$Device_M == "PC") {df1 <- dfpc}
    else {df1 <- dftel %>% filter(data.app != "Systemowy menad¿er pulpitu")}
    if ((input$person1_M == "Michal") & (input$Device_M == "Telefon")){
      df <- df_M %>% filter(date == input$data_M)
    }
    else{
      df1 <- df1 %>% filter(name == input$person1_M, date == input$data_M) %>% 
        group_by(data.app)  
      df2 <- df1  %>% summarise(number.of.activations = n())
      df3 <- df1 %>% summarise(duration = sum(duration)/60)
      df <- inner_join(df2, df3, by = "data.app")
    }
    if(length(df[[1]]) == 0) df <- data.frame(date = input$data_M, 
                                              number.of.activations = 0,
                                              duration = 0,
                                              data.app = "")
    
    plot <- plot <- ggplot(df,
                           aes(y = number.of.activations, x = duration, label = data.app, 
                               text = paste0("Aplikacja: ", data.app, "<br>Liczba uruchomieÅ„: ", number.of.activations,
                                             "<br>Czas:", round(duration, 3)))) +
      geom_point(colour = "#3ba8fc") +
      labs(title = "Stosunek d³ygoœci u¿ywania alpikacji w danym dniu do 
           iloœci uruchomieñ aplikacji",
           x = "Czas spêdzony w aplikacji (w minutach)",
           y = "Iloœæ uruchomieñ aplikacji") +
      theme_gray()+
      theme(panel.background = element_rect(fill = '#222222', colour = '#222222'),
            plot.background = element_rect(fill = '#303030', colour = '#303030'))+
      theme(text = element_text(size=10, color = "white"),
            axis.text.x =  element_text(size=10, color = "white"),
            axis.text.y =  element_text(size=10, color = "white"),
            plot.title = element_text(hjust = 0.5))
    ggplotly(plot,
             tooltip = "text")
    
  })
  
  output$table <- renderTable({
    if (input$type == "Z telefonu") {
      df <- df_J2 %>% 
        group_by(date, name) %>% 
        summarise(time = sum(duration)/60, .groups = "drop") %>% 
        complete(date, name, fill = list(time = 0))
      df_M1 <- df_M %>% 
        group_by(date, name) %>% 
        summarise(time = sum(duration)/60, .groups = "drop")
      df <- rbind(df, df_M1)
    }
    else{
      df <- df_J %>% 
        group_by(date, name) %>% 
        summarise(time = sum(duration)/60, .groups = "drop") %>% 
        complete(date, name, fill = list(time = 0))
    }
    df <- df %>% 
      filter(date >= input$dataJ[1], date <= input$dataJ[2])
    tab <- df %>% 
      group_by(name) %>% 
      summarise(sum = round(sum(time), 2),
                mean = round(mean(time), 2),
                max = round(max(time), 2),
                min = round(min(time), 2))
    tab <- df %>% 
      group_by(name) %>% 
      filter(time == max(time)) %>% 
      distinct(time, .keep_all = T) %>% 
      rename(daymax = date) %>% 
      inner_join(tab, by = "name")
    tab <- df %>% 
      group_by(name) %>% 
      filter(time == min(time)) %>% 
      distinct(time, .keep_all = T) %>% 
      rename(daymin = date) %>% 
      inner_join(tab, by = "name")
    
    if (input$type == "Z komputera") {
      tmp <- df_J %>% 
        #filter(date >= input$dataJ[1], date <= input$dataJ[2]) %>% 
        filter(date >= as.Date("2021-12-21"), date <= as.Date("2022-01-8")) %>% 
        group_by(name, hours) %>% 
        summarise(sum = (sum(duration)/60), .groups = "drop") %>%
        complete(name, hours, fill = list(sum = 0)) %>% 
        mutate(sum = case_when(
          sum <= 60 ~ sum,
          sum > 60 ~ 60)) %>% 
        mutate(godzina = substring(hours, first = 12)) %>% 
        group_by(name, godzina) %>% 
        summarise(srednia = mean(sum)) %>% 
        filter(srednia == max(srednia)) %>% 
        mutate(srednia = round(srednia, 2))
      tab <- inner_join(tab, tmp, by = "name") %>% 
        filter(name %in% input$name) %>% 
        select(name, sum, mean, max, daymax, min, daymin, godzina, srednia)
      tab <- t(tab)
      names <- c("", "Ca³kowity czas korzystania",
                 "Œredni dzienny czas korzystania",
                 "Najd³u¿sze dzienne u¿ytkowanie",
                 "Dzieñ najd³u¿szego u¿ytkowania",
                 "Najkrótsze dziennie u¿ytkowanie",
                 "Dzieñ najkrótszego u¿ytkowania",
                 "Godzina Œrednio najd³u¿szego u¿ytkowania",
                 "Œredni czas u¿ytkowania w najpopularniejszej godzinie")
      tab <- cbind(names, tab)
      
    }
    else {
      tab <- tab %>% 
        filter(name %in% input$name) %>% 
        select(name, sum, mean, max, daymax, min, daymin)
      
      tab <- t(tab)
      names <- c("", "Ca³kowity czas korzystania",
                 "Œredni dzienny czas korzystania",
                 "Najd³u¿sze dzienne u¿ytkowanie",
                 "Dzieñ najd³u¿szego u¿ytkowania",
                 "Najkrótsze dziennie u¿ytkowanie",
                 "Dzieñ najkrótszego u¿ytkowania",)
      tab <- cbind(names, tab)
      
      
    }
    
    colnames(tab) <- rep(" ", length(colnames(tab)))
    tab
  }, bordered = TRUE)
  
  output$text <- renderText ({
    if (input$type == "Z komputera")
      dev <- "komputera"
    else 
      dev <- "telefonu"
    text <- paste("Podsumowanie statystyczne czasu korzystania z ", dev, "od ",
                  as.character(input$dataJ[1]), " do ", as.character(input$dataJ[2]))
  })
}

ui <- navbarPage(
  title = "",
  tabPanel("Czas korzystania", uiTime, icon = icon("hourglass")),
  tabPanel("Aplikacje", ui_D, icon = icon("clock")),
  tabPanel("Blokada telefonu", uiUnlocking, icon = icon("lock")),
  tabPanel("Strony internetowe", ui_Michal, icon = icon("globe"))
)

shinyApp(ui = ui, server = server)