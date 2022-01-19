library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(gganimate)
library(DT)
library(VennDiagram)
library(shinycssloaders)
library(fmsb)
library(httr)
library(rsconnect)

source("functions.R")
source("connectapi.R")


# przekopiować z password.txt
clientID = '28e3d7ae0359454888cc25815aa18732'
secret = 'c0f866dbe2db4388b51d5f1f714ca8be'

response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

mytoken = content(response)$access_token

HeaderValue = paste0('Bearer ', mytoken)

# -----------PRZYKŁAD JAK UŻYWAĆ--------------
# artistID = "6QfFTZJHFSe9Xyes6DkAli" ---> wyszukujemy ID ReTo
# URI = paste0('https://api.spotify.com/v1/artists/', artistID)
# response2 = GET(url = URI, add_headers(Authorization = HeaderValue))
# Artist = content(response2)
# Artist$followers$total   -->  liczba followersów ReTo

p_streaming <- read_json("data/patryk/StreamingHistory0.json")
j_streaming <- bind_rows(read_json("data/janek/StreamingHistory0.json"),
                         read_json("data/janek/StreamingHistory1.json"),
                         read_json("data/janek/StreamingHistory2.json"))
l_streaming <- read_json("data/Lukasz/StreamingHistory0.json")

streaming <- bind_rows("p"=p_streaming, "j"=j_streaming, "l"=l_streaming,
                       .id="user")
streaming <- fix_streaming(streaming)

streaming <-
  streaming %>% 
  mutate(season = case_when(
    month %in% c('12', '1', '2') ~ 'Winter',
    month %in% c('3', '4', '5') ~ 'Spring',
    month %in% c('6', '7', '8') ~ 'Summer',
    month %in% c('9', '10', '11') ~ 'Fall'
  ))

p_streaming <- fix_streaming(p_streaming)
j_streaming <- fix_streaming(j_streaming)
l_streaming <- fix_streaming(l_streaming)

artists <- read.csv("data/artistID")
tracks <- read.csv("data/trackID")
popArtist<- read.csv("data/artistPop", check.names = F)
popTrack <- read.csv("data/trackPopDuration", check.names = F)

colnames(popArtist) <- c("artistName", "id", "Popularność")
colnames(popTrack) <- c("artistName", "trackName", "id", "Popularność", "CzasTrwania")

futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

# dodatkowe dane o utworach
# streaming %>% 
#   select(trackName) %>% 
#   unique() %>% 
#   inner_join(tracks) -> streamingID
# 
# df <- data.frame()
# for(i in seq(101,dim(streamingID)[1], 100)) {
#   df <- rbind(df, get_track_audio_features(streamingID[(i-100):(i-1), "id"]))
#   print(i)
# }
# df <- rbind(df ,get_track_audio_features(streamingID[4101:4119, "id"]))
# 
# trackInfo <- inner_join(streamingID, df, "id") %>% unique()
# write.csv(trackInfo, file="data/trackInfo")

trackInfo <- read.csv("data/trackInfo")

getArtistInfo <- function(artist_name) {
  artistID <- artists %>% filter(artistName==artist_name)
  URI = paste0('https://api.spotify.com/v1/artists/', artistID[[2]])
  response2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  content(response2)
}

tempxd <- function(streaming) {
  streaming %>% 
    #group_by(artistName, trackName) %>%
    #summarise(totalTime = sum(msPlayed), .groups="drop") 
    #arrange(-totalTime) %>% 
    #select(artistName, trackName, totalTime) %>% 
    # unique() %>% 
    inner_join(trackInfo, by=c("trackName", "artistName"))
  #select(year, month, minute, hour, danceability, energy, acousticness, valence, speechiness)
  #mutate_all(mean)
  #head(1) -> x
  
  #rbind(rep(1,5), rep(0,5), x)
}

p_mood <- tempxd(p_streaming)
j_mood <- tempxd(j_streaming)
l_mood <- tempxd(l_streaming)
all_mood <- bind_rows("p"=p_mood, "j"=j_mood, "l"=l_mood,
                      .id="user")

all_mood <-
  all_mood %>% 
  mutate(season = case_when(
    month %in% c('12', '1', '2') ~ 'Winter',
    month %in% c('3', '4', '5') ~ 'Spring',
    month %in% c('6', '7', '8') ~ 'Summer',
    month %in% c('9', '10', '11') ~ 'Fall'
  ))


ui1 <- fluidPage(
  h1("Jak zmieniała się słuchana przez nas muzyka w zależności od pory roku?",align= "center"),
  hr(),
  fluidRow(
    column(4,checkboxGroupInput("who", "Wybierz osoby:",
                                choiceNames=c("Patryk", "Łukasz", "Janek"),
                                choiceValues=c("p", "l", "j"),
                                selected=c("p", "l", "j")),
           align = "center"),
    column(4,selectInput("stats", "Wybierz kategorię:",
                         choices=c("Taneczność" = "danceability",
                                   "Energia" = "energy",
                                   "Akustyczność" = "acousticness",
                                   "Wartościowość" = "valence",
                                   "Mowa" = "speechiness")
    ),align = "center"),
    column(4,checkboxGroupInput("season", "Wybierz pory roku:",
                                choiceNames=c("Wiosna", "Lato", "Jesień", "Zima"),
                                choiceValues=c("Spring", "Summer", "Fall", "Winter"),
                                selected=c("Spring", "Summer", "Fall", "Winter")
    ),align = "center")
  ),
  hr(),
  fluidRow(
    
    column(12,
           conditionalPanel(condition = "input.stats == 'danceability'",
                            h3("Rozkład taneczności słuchanych przez nas utworów",
                               align = "center")
           ),
           conditionalPanel(
             condition = "input.stats == 'energy'",
             h3("Rozkład energii słuchanych przez nas utworów", align = "center")),
           
           conditionalPanel(condition = "input.stats == 'acousticness'",
                            h3("Rozkład akustyczności słuchanych przez nas utworów", align = "center")
           ),
           conditionalPanel(
             condition = "input.stats == 'valence'",
             h3("Rozkład wartościowości słuchanych przez nas utworów", align = "center")),
           
           conditionalPanel(condition = "input.stats == 'speechiness'",
                            h3("Rozkład mowy słuchanych przez nas utworów", align = "center")
           ),
           plotly::plotlyOutput("feelings")%>% withSpinner(type=2, color.background="White"))),
  fluidRow(
    column(12,
           conditionalPanel(condition = "input.stats == 'danceability'",
                            h5("Taneczność - jak bardzo utwór nadaje się do tańczenia, im większa
             wartość, tym bardziej się nadaje", align = "center")
           ),
           conditionalPanel(
             condition = "input.stats == 'energy'",
             h5("Energia - miara intensywności i aktywności utworu, utwory o
             wysokiej wartości są zwykle szybkie i głośne", align = "center")),
           
           conditionalPanel(condition = "input.stats == 'acousticness'",
                            h5("Akustyczność - miara jak akustyczny jest utwór", align = "center")
           ),
           conditionalPanel(
             condition = "input.stats == 'valence'",
             h5("Wartościowość - miara jak muzycznie pozytywny jest utwór, utwory
             o wysokiej wartości są zwykle wesołe, natomiast o niskiej smutne", align = "center")),
           
           conditionalPanel(condition = "input.stats == 'speechiness'",
                            h5("Mowa - im w utworze bardziej dominują słowa, tym większa jest
             wartość", align = "center")
           )
    )
  ),hr(),
  fluidRow(
    column(12,h3("W jakich godzinach najczęściej słuchamy muzyki?", align = "center"),
           plotOutput("plot1")%>% withSpinner(type=2, color.background="White")))
)


ui2 <- fluidPage(
  conditionalPanel(condition = "input.kategoria == 'Wykonawcy'",
                   h1("Wykresy przedstawiające ulubionych wykonawców", align = "center")
  ),
  conditionalPanel(
    condition = "input.kategoria == 'Utwory'",
    h1("Wykresy przedstawiające ulubione utwory", align = "center")),
  hr(),
  fluidRow(
    column(4,selectInput("kategoria",
                         "Wybierz kategorię:",
                         c("Wykonawcy","Utwory")),align = "center"),
    column(4,sliderInput("n", 
                         label = "Ilość:", 
                         value = 5,
                         min = 1,
                         max = 10),align = "center"
    ),
    column(4,checkboxGroupInput("who2", "Wybierz osoby:",
                                choiceNames=c("Patryk", "Łukasz", "Janek"),
                                choiceValues=c("p", "l", "j"),
                                selected=c("p", "l", "j")),align = "center"
    )
  ),
  hr(),
  fluidRow(
    
    column(6,
           conditionalPanel(condition = "input.kategoria == 'Wykonawcy'",
                            h3("Ile czasu poświęciliśmy na słuchanie ulubionych wykonawców?", align = "center")
           ),
           conditionalPanel(
             condition = "input.kategoria == 'Utwory'",
             h3("Ile czasu poświęciliśmy na słuchanie ulubionych utworów?", align = "center")),
           plotly::plotlyOutput("plot2")%>% withSpinner(type=2, color.background="White"),
           style='margin-bottom:30px;border:1px solid; padding: 10px;'),
    column(6,
           conditionalPanel(condition = "input.kategoria == 'Wykonawcy'",
                            h3("Ile minut słuchaliśmy ulubionych wykonawców w danym miesiącu?", align = "center")
           ),
           conditionalPanel(
             condition = "input.kategoria == 'Utwory'",
             h3("Ile minut słuchaliśmy ulubionych utworów w danym miesiącu?", align = "center")),
           plotly::plotlyOutput("plot3")%>% withSpinner(type=2, color.background="White"),
           style='margin-bottom:30px;border:1px solid; padding: 10px;')
  ),
  fluidRow(
    column(6, 
           conditionalPanel(
             condition = "input.kategoria == 'Wykonawcy'",
             h3("Wykonawców o jakiej popularności słuchaliśmy najczęściej?", align = "center")
           ),
           conditionalPanel(
             condition = "input.kategoria == 'Utwory'",
             h3("Utwory o jakiej popularności słuchaliśmy najczęściej?", align = "center")),
           plotly::plotlyOutput("plot6")%>% withSpinner(type=2, color.background="White"),
           style='margin-bottom:30px;border:1px solid; padding: 10px;'),
    column(6,conditionalPanel(
      condition = "input.kategoria == 'Wykonawcy'",
      h3("Ile średnio trwało jedno odtworzenie ulubionych wykonawców?", align = "center")),
      conditionalPanel(
        condition = "input.kategoria == 'Utwory'",
        h3("Jaki procent utworu średnio stanowiło jedno odtworzenie?", align = "center")),
      plotly::plotlyOutput("plot7")%>% withSpinner(type=2, color.background="White"),
      style='margin-bottom:30px;border:1px solid; padding: 10px;')
  )
)

ui2a <- fluidPage(
  conditionalPanel(condition = "input.kategoriaa == 'Wykonawcy'",
                   h1("Tabela przedstawiająca ulubionych wykonawców",align = "center")
  ),
  conditionalPanel(
    condition = "input.kategoriaa == 'Utwory'",
    h1("Tabela przedstawiająca ulubione utwory", align = "Center")),
  hr(),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("kategoriaa",
                             "Wybierz kategorię:",
                             c("Wykonawcy","Utwory")),
                 strong("Wybierz czyją tabelę chcesz zobaczyć:"),
                 p(""),
                 fixedRow(
                   column(12,actionButton(inputId = "button_p", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-1/p200x200/117042018_2583202521944871_6439550514274272204_n.jpg?_nc_cat=109&ccb=1-5&_nc_sid=7206a8&_nc_ohc=QFaAaBs81foAX9puy1r&_nc_ht=scontent-ham3-1.xx&oh=00_AT9_MBy9ndkBmvrSfEiU61e6ODO0xvL68sqQsXmxxyb_dQ&oe=61F79921');  background-size: cover; background-position: center;")
                          ,align = "center")
                 ),
                 fixedRow(
                   column(12,actionButton(inputId = "button_l", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/35237401_2001879876551410_721839996499132416_n.jpg?_nc_cat=104&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=Z8XbrlmH_TwAX-7-QXY&_nc_ht=scontent-ham3-1.xx&oh=00_AT-5GJS2QFCaFNJEeHDoVLmhPnL9LF-diIB6L1sFhn76Dw&oe=61F8B18A');  background-size: cover; background-position: center;")
                          ,align = "center")
                 ),
                 fixedRow(
                   column(12,actionButton(inputId = "button_j", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/123193344_2840269192876438_7586431629465567622_n.jpg?_nc_cat=103&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=b5KeJi9WozQAX93Qn0Q&_nc_ht=scontent-ham3-1.xx&oh=00_AT-7Dk8EPiY94-U98NLYzsLvZWlYMCE9CJk_DkxG238MyQ&oe=61F76FDD');  background-size: cover; background-position: center;")
                          ,align = "center")
                 )
    ),
    mainPanel(
      fixedRow(
        column(12, offset = 1,uiOutput("text_header"))
      ),
      fixedRow(
        column(12, offset = 1,
               dataTableOutput("tabela")%>% withSpinner(type=2, color.background="White")))
      
    )
  )
)


ui3 <- fluidPage(
  tabsetPanel(
    tabPanel("Utwory, artyści i gatunki",
             fluidRow(
               column(12,h3("Zgodność utworów i artystów",align="center"))
             ),
             fluidRow(
               column(h4("Procent wspólnych utworów",align="center"),
                      plotOutput("plot4") %>% withSpinner(type=2, color.background="White"),
                      width=6
               ),
               column(h4("Procent wspólnych artystów",align="center"),
                      plotOutput("plot5") %>% withSpinner(type=2, color.background="White"),
                      width=6
               )),
             fluidRow(
               column(12,h3("Ulubione gatunki muzyczne",align="center"))
             ),
             fluidRow(
               column(12,plotOutput("genres", height=900, width=900) %>% withSpinner(type=2, color.background="White"),
                      align="center")
             )
    ),
    tabPanel("Cechy ulubionych utworów",
             sidebarLayout(
               sidebarPanel(
                 width=3,
                 sliderInput("top",
                             label="Liczba ulubionych utworów",
                             value=10,
                             min=2,
                             max=50),
                 hr(),
                 h6("Taneczność - jak bardzo utwór nadaje się do tańczenia, im większa
             wartość, tym bardziej się nadaje"),
                 h6("Energia - miara intensywności i aktywności utworu, utwory o
             wysokiej wartości są zwykle szybkie i głośne"),
                 h6("Akustyczność - miara jak akustyczny jest utwór"),
                 h6("Wartościowość - miara jak muzycznie pozytywny jest utwór, utwory
             o wysokiej wartości są zwykle wesołe, natomiast o niskiej smutne"),
                 h6("Mowa - im w utworze bardziej dominują słowa, tym większa jest
             wartość")
               ),
               mainPanel(
                 column(12,h3("Cechy naszych ulubionych utworów"), align = "center"),
                 plotOutput("spider") %>% withSpinner(type=2, color.background="White")
               )
             )
    )
  )
  # style = "overflow-y: auto;" 
)


server <- function(input, output) {
  
  rv <-reactiveVal("")
  observeEvent(input$button_l,{rv("Łukasz")})
  observeEvent(input$button_p,{rv("Patryk")})
  observeEvent(input$button_j,{rv("Janek")})
  output$text_header <- renderUI(h2(rv(),align = "center"))
  
  observeEvent(input$kategoriaa, {output$tabela <- renderDataTable({})
  rv("")})
  observeEvent(input$button_l,{
    if(input$kategoriaa == "Wykonawcy"){
      output$tabela <- renderDataTable({
        l_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }else if(input$kategoriaa == "Utwory"){
      output$tabela <- renderDataTable({
        l_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }
  })
  
  
  observeEvent(input$button_p,{
    if(input$kategoriaa == "Wykonawcy"){
      output$tabela <- renderDataTable({
        p_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }else if(input$kategoriaa == "Utwory"){
      output$tabela <- renderDataTable({
        p_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }
  })
  
  observeEvent(input$button_j,{
    if(input$kategoriaa == "Wykonawcy"){
      output$tabela <- renderDataTable({
        j_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }else if(input$kategoriaa == "Utwory"){
      output$tabela <- renderDataTable({
        j_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }
  })
  
  
  output$plot1 <- renderPlot({
    p <- streaming %>% 
      filter(user %in% input$who) %>%
      filter(season %in% input$season) %>%
      group_by(user) %>% 
      mutate(totalTime = sum(msPlayed)) %>% 
      group_by(user, hour) %>% 
      summarise(avgTime = sum(msPlayed)/totalTime )%>%
      mutate(Użytkownik = case_when(user == "l" ~ "Łukasz",
                                    user == "j" ~ "Janek",
                                    user == "p" ~ "Patryk"),`Średni czas` = avgTime,
             Godzina = hour) %>% 
      ggplot() +
      geom_line(aes(x=Godzina, y=`Średni czas`, color = Użytkownik), size=1.75) +
      theme_bw() +
      labs(
        x="Godzina",
        y="Procent całkowitego czasu słuchania"
      ) +
      scale_y_continuous(expand=expansion(add=c(0, 0.0058)),
                         breaks=seq(0, 0.13, by=0.01)) +
      scale_x_continuous(expand=c(0, 0, 0, 0),
                         breaks=seq(0, 23, by=1)) +
      theme(panel.grid.major.y = element_line(linetype=5),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())
    
    p
  })
  
  output$plot1b <- renderPlot({
    pes <- streaming %>%
      filter(month %in% input$month) %>%
      group_by(month) %>% 
      mutate(totalTime = sum(msPlayed)) %>% 
      group_by(month, hour) %>% 
      summarise(avgTime = sum(msPlayed)/totalTime ) %>% 
      ggplot() +
      geom_line(aes(x=hour, y=avgTime), size=1.75) +
      theme_bw() +
      labs(
        x="Godzina",
        y="Procent całkowitego czasu słuchania"
      ) +
      scale_y_continuous(expand=expansion(add=c(0, 0.0058)),
                         breaks=seq(0, 0.13, by=0.01)) +
      scale_x_continuous(expand=c(0, 0, 0, 0),
                         breaks=seq(0, 23, by=1)) +
      theme(panel.grid.major.y = element_line(linetype=5),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())
    
    pes
  })
  
  
  output$plot2 <- plotly::renderPlotly({
    if(input$kategoria == "Wykonawcy"){
      
      top <- streaming %>% filter(user %in% input$who2) %>% 
        group_by(artistName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
        arrange(-Minuty) %>% select(artistName) %>% head(input$n) %>% left_join(streaming) %>% 
        filter(user %in% input$who2) %>% group_by(artistName,user) %>% summarise(Minuty = sum(msPlayed)/60000) %>% 
        mutate(Minuty = round(Minuty,0))%>%
        mutate(Użytkownik = case_when(user == "l" ~ "Łukasz",
                                      user == "j" ~ "Janek",
                                      user == "p" ~ "Patryk"))
      
      colnames(top) <- c("Wykonawca","user","Minuty","Użytkownik")
      
      
      ggplot(top, aes(x=Wykonawca,y=Minuty, fill = Użytkownik)) +
        geom_col(position =  "dodge") + theme_bw() +
        labs(
          x="Wykonawca",
          y="Minuty"
        ) +
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank()) + coord_flip()
      
      
    }else if(input$kategoria == "Utwory"){
      top <- streaming %>% filter(user %in% input$who2) %>% 
        group_by(trackName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
        arrange(-Minuty) %>% select(trackName) %>% head(input$n) %>% left_join(streaming) %>% 
        filter(user %in% input$who2) %>% group_by(trackName,user) %>% summarise(Minuty = sum(msPlayed)/60000)%>%
        mutate(Minuty = round(Minuty,0))%>%
        mutate(Użytkownik = case_when(user == "l" ~ "Łukasz",
                                      user == "j" ~ "Janek",
                                      user == "p" ~ "Patryk"))
      
      colnames(top) <- c("Utwór","user","Minuty","Użytkownik")
      
      ggplot(top, aes(x=Utwór,y=Minuty, fill = Użytkownik)) +
        geom_col(position =  "dodge") + theme_bw() +
        labs(
          x="Utwor",
          y="Minuty"
        ) +
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank()) + coord_flip()
    }
  }) %>% bindCache(input$kategoria, input$n, input$who2)
  
  output$plot3 <- plotly::renderPlotly({
    if(length(input$who2)==0){
      ggplot() + theme_bw() +
        labs(
          x="Miesiąc",
          y="Minuty"
        ) +
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())+
        scale_x_continuous(breaks = seq(0,12,by=1))
    }else{
      if(input$kategoria == "Wykonawcy"){
        top <- streaming %>% filter(user %in% input$who2) %>% 
          group_by(artistName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
          arrange(-Minuty) %>% select(artistName) %>% head(input$n) %>% left_join(streaming) %>% 
          filter(user %in% input$who2) %>% 
          group_by(artistName, month) %>% summarise(Minuty = round(sum(msPlayed)/60000),0)
        colnames(top) <- c("Wykonawca","Miesiąc","Minuty")
        
        
        ggplot(top, aes(x=Miesiąc,y=Minuty, color = Wykonawca)) +
          geom_line() + theme_bw() +
          labs(
            x="Miesiąc",
            y="Minuty"
          ) +
          theme(panel.grid.major.y = element_line(linetype=5),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())+
          scale_x_continuous(breaks = seq(0,12,by=1))
      }else if(input$kategoria == "Utwory"){
        top <- streaming %>% filter(user %in% input$who2) %>% 
          group_by(trackName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
          arrange(-Minuty) %>% select(trackName) %>% head(input$n) %>% left_join(streaming) %>% 
          filter(user %in% input$who2) %>% 
          group_by(trackName, month) %>% summarise(Minuty = round(sum(msPlayed)/60000),0)
        colnames(top) <- c("Utwór","Miesiąc","Minuty")
        
        
        ggplot(top, aes(x=Miesiąc,y=Minuty, color = Utwór)) +
          geom_line() + theme_bw() +
          labs(
            x="Miesiąc",
            y="Minuty"
          ) +
          theme(panel.grid.major.y = element_line(linetype=5),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())+
          scale_x_continuous(breaks = seq(0,12,by=1))
      }
    }
  })%>% bindCache(input$kategoria, input$n, input$who2)
  
  
  output$plot4 <- renderPlot({
    p <- p_streaming %>% select(trackName, artistName) %>% 
      unique()
    l <- l_streaming %>% select(trackName, artistName) %>% 
      unique()
    j <- j_streaming %>% select(trackName, artistName) %>% 
      unique()
    
    pl_len <- dim(full_join(p, l))[1]
    pj_len <- dim(full_join(p, j))[1]
    lj_len <- dim(full_join(l, j))[1]
    
    pl_common_len <- dim(inner_join(p, l))[1]
    pj_common_len <- dim(inner_join(p, j))[1]
    lj_common_len <- dim(inner_join(l, j))[1]
    
    df <- data.frame(who=c("Patryk i Łukasz", "Patryk i Janek", "Łukasz i Janek"),
                     perc=c(pl_common_len/pl_len*100,
                            pj_common_len/pj_len*100,
                            lj_common_len/lj_len*100))
    
    
    df %>% 
      ggplot() +
      geom_col(aes(x=who, y=perc), fill="lightblue") +
      theme_bw() +
      labs(
        x="Pomiędzy kim",
        y="Procent zgodności"
      ) +
      scale_y_continuous(expand=expansion(add=c(0, 0.0058)),
                         limits=c(0, 20)) +
      theme(panel.grid.major.y = element_line(linetype=5),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())
  })
  
  output$plot5 <- renderPlot({
    p <- p_streaming %>% select(artistName) %>% 
      unique()
    l <- l_streaming %>% select(artistName) %>% 
      unique()
    j <- j_streaming %>% select(artistName) %>% 
      unique()
    
    pl_len <- dim(full_join(p, l))[1]
    pj_len <- dim(full_join(p, j))[1]
    lj_len <- dim(full_join(l, j))[1]
    
    pl_common_len <- dim(inner_join(p, l))[1]
    pj_common_len <- dim(inner_join(p, j))[1]
    lj_common_len <- dim(inner_join(l, j))[1]
    
    df <- data.frame(who=c("Patryk i Łukasz", "Patryk i Janek", "Łukasz i Janek"),
                     perc=c(pl_common_len/pl_len*100,
                            pj_common_len/pj_len*100,
                            lj_common_len/lj_len*100))
    
    
    df %>% 
      ggplot() +
      geom_col(aes(x=who, y=perc), fill="lightblue") +
      theme_bw() +
      labs(
        x="Pomiędzy kim",
        y="Procent zgodności"
      ) + 
      scale_y_continuous(expand=expansion(add=c(0, 0.0058)),
                         limits=c(0, 20)) +
      theme(panel.grid.major.y = element_line(linetype=5),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())
  })
  
  output$plot6 <- plotly::renderPlotly({
    if(input$kategoria == "Wykonawcy"){
      top <- merge(streaming,popArtist,by = "artistName") %>% 
        filter(user %in% input$who2) %>% select(Popularność)
    }else if(input$kategoria == "Utwory"){
      top <- merge(streaming,popTrack, by =c("trackName","artistName")) %>% 
        filter(user %in% input$who2) %>% select(Popularność)
    }
    
    ggplot(top, aes(x=Popularność)) + geom_histogram(binwidth = 1) +
      theme_bw() +
      labs(
        x="Popularność",
        y="Ilość"
      ) + 
      theme(panel.grid.major.y = element_line(linetype=5),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())
    
  }) %>% bindCache(input$kategoria,input$who2)
  
  output$plot7 <- plotly::renderPlotly({
    if(input$kategoria == "Wykonawcy"){
      top <- streaming %>% filter(user %in% input$who2) %>% 
        group_by(artistName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
        arrange(-Minuty) %>% select(artistName) %>% head(input$n) %>% left_join(streaming) %>% 
        filter(user %in% input$who2) %>% select(artistName,msPlayed) %>% group_by(artistName) %>% 
        mutate(Średnio = mean(msPlayed)/60000)
      colnames(top) <- c("Wykonawca","msPlayed","Średnio")
      
      
      ggplot(top, aes(x=Wykonawca,y=Średnio)) +
        geom_point() + theme_bw() +
        labs(
          x="Wykonawca",
          y="Średnia długość jednego odtworzenia"
        ) +
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank()) + coord_flip()
    }
    else if(input$kategoria == "Utwory"){
      top <- streaming %>% filter(user %in% input$who2) %>% 
        group_by(trackName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
        arrange(-Minuty) %>% select(trackName) %>% head(input$n) %>% left_join(streaming) %>% 
        filter(user %in% input$who2)
      top <- merge(top,popTrack, by = c("trackName","artistName")) %>% 
        select(trackName, msPlayed,CzasTrwania) %>% mutate(Procent = (msPlayed/CzasTrwania)*100) %>% 
        group_by(trackName) %>% mutate(Srednio = round(mean(Procent),2))
      colnames(top) <- c("Utwór", "","","","Średni procent")
      
      ggplot(top,aes(x=Utwór,y=`Średni procent`)) +
        geom_point() + theme_bw() +
        labs(
          x="Utwór",
          y="Średni procent przesuchanego utworu w trakcie jednego odtworzenia"
        ) +
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank()) + coord_flip()
    }
  })
  
  
  output$genres <- renderPlot({
    temp <- function(streaming){
      streaming %>%
        group_by(artistName) %>%
        summarise(totalTime = sum(msPlayed)) %>%
        arrange(-totalTime) %>%
        head(25) %>%
        select(artistName)
    }
    
    p_art <- temp(p_streaming)
    j_art <- temp(j_streaming)
    l_art <- temp(l_streaming)
    
    p_gen <- c()
    j_gen <- c()
    l_gen <- c()
    
    for(i in 1:25) {
      p_gen <- c(p_gen, unlist(getArtistInfo(p_art[[i, 1]])[["genres"]]))
      j_gen <- c(j_gen, unlist(getArtistInfo(j_art[[i, 1]])[["genres"]]))
      l_gen <- c(l_gen, unlist(getArtistInfo(l_art[[i, 1]])[["genres"]]))
    }
    
    data = list(Patryk=unique(p_gen), Łukasz=unique(l_gen),
                Janek=unique(j_gen))
    
    
    grid.newpage()
    v <- venn.diagram(data, filename = NULL,col=c("#440154ff", '#21908dff', '#fde725ff'),
                      fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
                      fontfamily = "sans",
                      cex=1,
                      cat.cex = 3,
                      cat.default.pos = "outer",
                      cat.fontfamily = "sans",
                      cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
                      cat.pos=c(315, 45, 0),
                      rotation.degree=360)
    v[[7]]$label <- stringr::str_to_title(paste(setdiff(setdiff(l_gen, p_gen), j_gen), collapse="\n"))
    v[[8]]$label <- stringr::str_to_title(paste(setdiff(setdiff(j_gen, p_gen), l_gen), collapse="\n"))
    v[[9]]$label <- stringr::str_to_title(paste(setdiff(intersect(p_gen, l_gen), j_gen), collapse="\n"))
    v[[10]]$label <- stringr::str_to_title(paste(intersect(intersect(p_gen, l_gen), j_gen), collapse="\n"))
    v[[11]]$label <- stringr::str_to_title(paste(setdiff(intersect(p_gen, j_gen), l_gen), collapse="\n"))
    v[[12]]$label <-stringr::str_to_title(paste(setdiff(setdiff(p_gen, l_gen), j_gen), collapse="\n"))
    grid.draw(v)
    
  }, height=900, width=900)
  
  output$spider <- renderPlot({
    temp <- function(streaming, howMany) {
      streaming %>% 
        group_by(artistName, trackName) %>% 
        summarise(totalTime = sum(msPlayed), .groups="drop") %>% 
        arrange(-totalTime) %>% 
        head(howMany) %>% 
        select(artistName, trackName, totalTime) %>% 
        # unique() %>% 
        inner_join(trackInfo, by=c("artistName", "trackName")) %>% 
        select(danceability, energy, acousticness, valence, speechiness) %>%
        mutate_all(mean) %>%
        head(1) -> x
      
      rbind(rep(1,5), rep(0,5), x)
    }
    
    p_af <- temp(p_streaming, input$top)
    j_af <- temp(j_streaming, input$top)
    l_af <- temp(l_streaming, input$top)
    
    par(mar=c(0, 2, 4, 2))
    par(mfrow=c(1,3))
    radarchart(p_af, axistype=4, plwd=4, plty=1, cglcol="darkgray", cglty=1,
               cglwd=1, vlcex=1.35, calcex=0.8, cex.main=3,
               vlabels=c("Taneczność", "Energia", "Akustyczność",
                         "Wartościowość", "Mowa"),
               axislabcol="darkgray",
               pcol=rgb(0.3, 0.8, 0.8, 1),
               pfcol=rgb(0.3, 0.8, 0.8, 0.3),
               title="Patryk")
    radarchart(j_af, axistype=4, plwd=4, plty=1, cglcol="darkgray", cglty=1,
               cglwd=1, vlcex=1.35, calcex=0.8, cex.main=3,
               vlabels=c("Taneczność", "Energia", "Akustyczność",
                         "Wartościowość", "Mowa"),
               axislabcol="darkgray",
               pcol=rgb(0, 0.75, 0.2, 1),
               pfcol=rgb(0, 0.75, 0.2, 0.3),
               title="Janek")
    radarchart(l_af, axistype=4, plwd=4, plty=1, cglcol="darkgray", cglty=1,
               cglwd=1, vlcex=1.35, calcex=0.8, cex.main=3,
               vlabels=c("Taneczność", "Energia", "Akustyczność",
                         "Wartościowość", "Mowa"),
               axislabcol="darkgray",
               pcol=rgb(0.8, 0.8, 0, 1),
               pfcol=rgb(0.8, 0.8, 0, 0.3),
               title="Łukasz")
    
  })
  
  output$feelings <- plotly::renderPlotly({
    
    pog <- all_mood %>% 
      filter(user %in% input$who) %>%
      filter(season %in% input$season) 
    
    if(input$stats == "danceability"){
      pog <- pog %>% mutate(Kategoria = danceability)
      ggplot(pog, aes(x= Kategoria)) + geom_histogram(binwidth = 0.05) +
        theme_bw() +
        labs(
          x="Taneczność",
          y="Ilość"
        ) + 
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    }else if(input$stats == "energy"){
      pog <- pog %>% mutate(Kategoria = energy)
      ggplot(pog, aes(x= Kategoria)) + geom_histogram(binwidth = 0.05) +
        theme_bw() +
        labs(
          x="Energia",
          y="Ilość"
        ) + 
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    }else if(input$stats == "acousticness"){
      pog <- pog %>% mutate(Kategoria = acousticness)
      ggplot(pog, aes(x= Kategoria)) + geom_histogram(binwidth = 0.05) +
        theme_bw() +
        labs(
          x="Akustyczność",
          y="Ilość"
        ) + 
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    }else if(input$stats == "valence"){
      pog <- pog %>% mutate(Kategoria = valence)
      ggplot(pog, aes(x= Kategoria)) + geom_histogram(binwidth = 0.05) +
        theme_bw() +
        labs(
          x="Wartościowość",
          y="Ilość"
        ) + 
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    }else if(input$stats == "speechiness"){
      pog <- pog %>% mutate(Kategoria = speechiness)
      ggplot(pog, aes(x= Kategoria)) + geom_histogram(binwidth = 0.05) +
        theme_bw() +
        labs(
          x="Mowa",
          y="Ilość"
        ) + 
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    }
  })
  
  
}
app_ui <- navbarPage(
  title= "Nasz Spotify",
  tabPanel("Pory roku a nasza muzyka", ui1, icon = icon("clock")),
  navbarMenu("Najczęściej słuchane", 
             tabPanel("Wykresy", ui2, icon = icon("chart-line")),
             tabPanel("Tabela", ui2a, icon = icon("table")),
             icon = icon("heart")),
  tabPanel("Zgodność muzyki", ui3, icon = icon("handshake")),
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  footer = shiny::HTML('<footer class="text-center text-sm-start" style="width:100%;">
  <hr>
              <span style="font-weight: bold">Autorzy:<span/>
              <br/>
              <span style="font-weight: bold">Patryk Rakus<span/>
              <a href="https://github.com/rakusp">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width="20" height="20">
              </a>
              <br/>
              <span style="font-weight: bold">Jan Skwarek<span/>
              <a href="https://github.com/janskwr">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width="20" height="20">
              </a>
              <br/>
              <span style="font-weight: bold">Łukasz Tomaszewski<span/>
              <a href="https://github.com/tomaszewskil">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width="20" height="20">
              </a>
              </div>
              
              </footer>')
)

shinyApp(ui = app_ui, server = server)
