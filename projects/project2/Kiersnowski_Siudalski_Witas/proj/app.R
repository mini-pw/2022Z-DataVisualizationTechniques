library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(leaflet)
library(shinyTime)
library(ggplot2)
library(forcats)
library(stringi)
library(tm)
library(shinydashboard)
library(fresh)
library(plotly)

# mytheme <- create_theme(
#   adminlte_color(
#     light_blue = "#728C69"
#   ),
#   adminlte_sidebar(
#     width = "200px",
#     dark_bg = "#728C69",
#     dark_hover_bg = "#B2D3C2",
#     dark_color = "#2C1112"
#   ),
#   adminlte_global(
#     content_bg = "#fbf8f3",
#     box_bg = 	"#dcc59c",
#     info_box_bg = "#dcc59c"
#   )
# )
# mytheme <- create_theme(
#   adminlte_color(
#     light_blue = "#08303A"
#   ),
#   adminlte_sidebar(
#     width = "200px",
#     dark_bg = "#08303A",
#     dark_hover_bg = "#e74c3c",
#     dark_color = "#437483"
#   ),
#   adminlte_global(
#     content_bg = "#F8FEFE",
#     box_bg = 	"#7BA9B6",
#     info_box_bg = "#7BA9B6"
#   )
# )
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#3D210C"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#3D210C",
    dark_hover_bg = "#e74c3c",
    dark_color = "#F2E5A5"
  ),
  adminlte_global(
    content_bg = "#fff",
    box_bg = 	"#F2E5A5",
    info_box_bg = "#F2E5A5"
  )
)



activitydf <- read.csv("data/dataCSV/ActivitySegmentalmostFinal.csv", encoding = "UTF-8") %>% 
  mutate(ActivityType = ifelse(ActivityType == "IN_PASSENGER_VEHICLE", "IN_CAR", ActivityType))

pointsdf <- read.csv("data/dataCSV/PointsalmostFinal.csv", encoding = "UTF-8")

placevisitdf <- read.csv("data/dataCSV/PlacesVisitedalmostFinal.csv", encoding = "UTF-8") 

activity2 <- activitydf %>%  mutate(StartingHour = format(as.POSIXct(StartingtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"), "%H")) %>%
    mutate(StartingMinute = format(as.POSIXct(StartingtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"), "%M")) %>%
    mutate(EndHour = format(as.POSIXct(EndtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"), "%H")) %>%
    mutate(EndMinute = format(as.POSIXct(EndtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"	), "%M")) %>% 
    mutate(DurationInMinutes = (EndtimeStampInMS-StartingtimeStampInMS)/60000) %>% 
    mutate(MiddleOfActivityHour = 
               as.numeric(format(as.POSIXct((EndtimeStampInMS+StartingtimeStampInMS)/2000, origin="1970-01-01",tz = "Europe/Warsaw"), "%H"))) %>% 
    mutate(MiddleOfActivitiMin=as.numeric(format(as.POSIXct((EndtimeStampInMS+StartingtimeStampInMS)/2000, origin="1970-01-01",tz = "Europe/Warsaw"), "%M"))) %>% 
    mutate(MiddleOfActivity = MiddleOfActivityHour+MiddleOfActivitiMin/60) %>% 
    mutate(Weekday=format(as.POSIXct((EndtimeStampInMS+StartingtimeStampInMS)/2000, origin="1970-01-01",tz = "Europe/Warsaw"), "%a")) %>% 
    mutate(dates = as.Date(as.POSIXct(StartingtimeStampInMS / 1000, origin="1970-01-01"))) 

df2 <- placevisitdf  %>% select(-PlaceId, -X)


dfa <- activitydf %>% 
    select(-X) %>% 
    rename(startlong = StartingLongitude, startlat = StartingLatitude,
           endlong = EndingLongitude, endlat = EndingLatitude, 
           starttime = StartingtimeStampInMS, endtime = EndtimeStampInMS)


df <- pointsdf %>% select(-X) %>%
    mutate(Activity = "UNKNOWN_ACTIVITY_TYPE", Place = NA) %>%
    mutate(long = Longitude / 1e7, lat = Latitude / 1e7, .keep = "unused") %>% 
    mutate(data = as.Date(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"))) %>%
    mutate(hm = format(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"), "%H:%M")) %>%
    arrange(User, TimeStampInMS)


for (i in 1:nrow(dfa)){
    ind <- which(df$TimeStampInMS >= dfa[i,]$starttime &
                     df$TimeStampInMS <= dfa[i,]$endtime &
                     df$User == dfa[i,]$User)
    if(length(ind) > 1){
        ind <- ind[1:length(ind) - 1]
        df[ind,]$Activity <- dfa[i,]$ActivityType
    }
}


for (i in 1:nrow(df2)){
    places <- which((df$lat == df2[i,]$Latitude / 1e7) & (df$long == df2[i,]$Longitude / 1e7))
    df[places,]$Place <- df2[i,]$Name              
}

df <- df %>% mutate(ActivityColor = case_when(
    Activity == "WALKING" ~ "blue",
    Activity == "IN_CAR" ~ "purple",
    Activity == "IN_TRAM" ~ "yellow",
    Activity == "IN_TRAIN" ~ "navy",
    Activity == "UNKNOWN_ACTIVITY_TYPE" ~ "grey",
    Activity == "IN_BUS" ~ "orange",
    Activity == "IN_SUBWAY" ~ "maroon",
    Activity == "SKIING" ~ "white"
))

acti <- activitydf %>%
    mutate(StartTime = as.POSIXct(StartingtimeStampInMS/1000, origin = "1970-01-01")) %>%
    mutate(EndTime = as.POSIXct(EndtimeStampInMS/1000, origin = "1970-01-01")) %>%
    mutate(data = as.Date(StartTime)) %>%
    group_by(User, data) %>%
    summarise(dystans = sum(Distance)/1000) %>%
    mutate(cum_distance = cumsum(dystans))

daty <- sort(unique(acti$data))


server <- function(input, output, session) {
    
    
    output$stepBox <- renderInfoBox({
    stepCount <- dfa %>% filter(User == input$users) %>%
            filter(as.Date(as.POSIXct(starttime/1000, origin = "1970-01-01")) == input$DatesMerge) %>%
            filter(ActivityType == "WALKING") %>% select(Distance)
    stepCount <- ifelse(length(stepCount$Distance) > 0, round(sum(stepCount$Distance)/0.65, 0), 0)  
        infoBox(
            "Number of steps:", stepCount, icon = icon("shoe-prints"),
            color = "red"
        )
    })
    
    output$distanceBox <- renderInfoBox({
      totalDistance <- dfa %>% filter(User == input$users) %>%
        filter(as.Date(as.POSIXct(starttime/1000, origin = "1970-01-01")) == input$DatesMerge) %>%
        select(Distance)
      totalDistance <- ifelse(length(totalDistance$Distance) > 0, round(sum(totalDistance$Distance)/1000, 1), 0)  
      infoBox(
        "Total distance:", paste(totalDistance, "km"), icon = icon("road"),
        color = "red"
      )
    })
    
    output$transportBox <- renderInfoBox({
      transport <- dfa %>% filter(User == input$users) %>%
        filter(as.Date(as.POSIXct(starttime/1000, origin = "1970-01-01")) == input$DatesMerge) %>%
        mutate(time = endtime - starttime) %>%
        group_by(ActivityType) %>% summarise(sum = sum(time)) %>% arrange(desc(sum)) %>% slice(1)
      transport <- ifelse(length(transport$sum) > 0,
                          stri_replace_all_regex(transport$ActivityType,
                                                 pattern = c("IN_|_TYPE", "_"),
                                                 replacement = c(""," "),
                                                 vectorize_all = FALSE),
                          "No Activity this day ):") 
      infoBox(
        "Main transport:", transport, icon = icon("bus"),
        color = "red"
      )
    })
    
    output$FirstGKPlot <- renderPlotly({
        
        
        
        activity3<- activity2  %>% 
            filter(DurationInMinutes<=300)
        activity3$Weekday <- factor(activity3$Weekday, c("Mon","Tue","Wed", "Thu", "Fri", "Sat", "Sun"))
        if(input$weekday=="day"){
            p<-ggplot(activity3, aes(y=Distance/1000, x=MiddleOfActivity, group=User, color=User, size=sqrt(DurationInMinutes)/3))+
                scale_color_brewer(palette = "Dark2")+
                geom_point()+
                ylab("Distance in km")+ 
                scale_size_identity()+
                xlab("Middle time of activity (hours)")+
                ylim(0,input$MaxDistance)+
                theme_bw()+
                theme(plot.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))+
                theme(legend.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))}
        
        else{
            p<-ggplot(activity3, aes(y=Distance/1000, x=Weekday, group=User, color=User, size=sqrt(DurationInMinutes)/3))+
                geom_jitter(height = 0, width=0.3)+ 
                scale_size_identity()+
                scale_color_brewer(palette = "Dark2")+
                ylab("Distance in km")+
                xlab("weekday of activity")+
                ylim(0,input$MaxDistance)+
                theme_bw()+
                theme(plot.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))+
                theme(legend.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))
        }
        ggplotly(p) %>% config(displayModeBar = F)
    })
    
    output$SecondGKPlot<- renderPlot({
        Users<-c('User1', 'User2', 'User3')
        newdf<-data.frame(Users) 
        newdf$PartyTime<-c(
            sum(activity2 %>% filter((StartingHour<4)&(User=="User1")&(DurationInMinutes<300)) %>% select(DurationInMinutes)),
            sum(activity2 %>% filter((StartingHour<4)&(User=="User2")&(DurationInMinutes<300)) %>% select(DurationInMinutes)),
            sum(activity2 %>% filter((StartingHour<4)&(User=="User3")&(DurationInMinutes<300)) %>% select(DurationInMinutes))
        )
        
        
        p<- ggplot(newdf, aes(y=Users, x=PartyTime)) + geom_col(fill="brown", width=0.5)+
          scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 5000))+
          theme_bw()+
          theme(plot.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))+
          theme(legend.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))
          
        p+coord_flip()
        
        
        
    })
    
    output$Mapka <- renderLeaflet({
        
        dfm <- df %>% 
            filter(User %in% input$users) %>%
            filter(data == input$DatesMerge &
                       hm > format(as.POSIXct(input$timeS), "%H:%M") &
                       hm < format(as.POSIXct(input$timeE), "%H:%M"))
        
    
        if (length(dfm$User) == 0){
            map3 <- leaflet(dfm) %>% setView(lng = 21.017532, lat = 52.237049, zoom = 7)
        }
        else{
            map3 <- leaflet(dfm) %>% addAwesomeMarkers(~long,
                                  ~lat,
                                  icon = makeAwesomeIcon(
                                      icon = ~if_else(is.na(Place), "diamond", "map-marker"),
                                      library = "glyphicon",
                                      markerColor = "red",
                                      iconColor = "black"),
                                  popup = ~Place)
            nn <- nrow(dfm)-1
            if (nn > 0){
                for (i in 1:nn)
                    if (dfm$User[i] == dfm$User[i+1]){
                        map3 <- map3 %>% 
                            addPolylines(lat=c(dfm[i,]$lat,dfm[i+1,]$lat),
                                         lng=c(dfm[i,]$long,dfm[i+1,]$long),
                                         color = dfm[i,]$ActivityColor,
                                         popup = ~paste("Distance: ", round(raster::pointDistance(c(dfm[i,]$long,
                                                                                                    dfm[i,]$lat),
                                                                                                  c(dfm[i+1,]$long,
                                                                                                    dfm[i+1,]$lat),
                                                                                                  lonlat = TRUE), 0), "m"))
                    }
            }
            
        }
            map3 %>% addProviderTiles("OpenStreetMap.Mapnik", group = "Map View") %>%
                addProviderTiles("Esri.WorldImagery", group = "Satellite View") %>%
                addLayersControl(baseGroups = c("Map View", "Satellite View"),
                                 options = layersControlOptions(collapsed = FALSE)) %>%
                addLegend(
                    position = "bottomright",
                    colors = unique(df$ActivityColor),
                    labels = tolower(
                        stri_replace_all_regex(unique(df$Activity),
                                               pattern = c("IN_|_TYPE", "_"),
                                               replacement = c(""," "),
                                               vectorize_all = FALSE)),
                    opacity = 1,
                    title = "Mode of transport"
                    )
        
        
    })
    
    
    output$carbon <- plotly::renderPlotly({
        df <- activitydf %>%
            mutate(sladPoj = case_when(ActivityType ==  "WALKING" ~ 0   ,
                                       ActivityType == "IN_CAR" ~ 96,
                                       ActivityType =="IN_TRAM" ~ 35,
                                       ActivityType =="IN_TRAIN" ~ 41,
                                       ActivityType =="IN_BUS" ~52,
                                       ActivityType =="IN_SUBWAY" ~ 31,
                                       ActivityType =="UNKNOWN_ACTIVITY_TYPE" ~ 0,
                                       TRUE ~ 0)
            ) %>%
            mutate(sladW = sladPoj*Distance/1000) %>% 
            mutate(StartTime = as.POSIXct(StartingtimeStampInMS/1000, origin = "1970-01-01")) %>% 
            mutate(EndTime = as.POSIXct(EndtimeStampInMS/1000, origin = "1970-01-01")) %>% 
            filter(ActivityType != "WALKING" & ActivityType != "SKIING")
        
        
        p <- df %>% 
            filter( StartTime > as.POSIXct(input$days[1]) & EndTime < as.POSIXct(input$days[2]+1)) %>% 
            mutate(ActivityType = case_when(ActivityType == "IN_CAR" ~ "IN CAR",
                                            TRUE ~ "OTHER" ,
            )) %>% 
            group_by(User, ActivityType) %>%
            summarise(CarbonFootprint = sum(sladW)/1000)  %>% 
            mutate(ActivityType = fct_reorder(ActivityType, CarbonFootprint, .desc = F)) %>% 
            ggplot(aes(x = User,y = CarbonFootprint, fill = ActivityType)) +
            geom_col(width=0.5) +
          scale_fill_brewer(palette = "Dark2")+
            theme_bw() +
            labs(title = "Estimated carbon footprint of our travels",
                 y = "Carbon footprint [kg]",
                 x = "")+
          theme(plot.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))+
          theme(legend.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))+
          coord_flip()
        
        ggplotly(p) %>% config(displayModeBar = F)
        
        
    })
    

    
    output$TypAktywnosci <- plotly::renderPlotly({
      
      
      if(length(input$period) != 0){ 
      df <- activitydf %>% 
          mutate(StartTime = as.POSIXct(StartingtimeStampInMS/1000, origin = "1970-01-01")) %>% 
          mutate(Period = case_when(StartTime <   as.POSIXct(as.Date("2021-12-19")  ) ~ "PW",
                                  StartTime >   as.POSIXct(as.Date("2022-01-10")) & StartTime <   as.POSIXct(as.Date("2022-01-15")) ~ "PW"  ,
                                  StartTime >   as.POSIXct(as.Date("2021-12-20")) & StartTime <   as.POSIXct(as.Date("2021-12-23"))  ~ "ZDALNIE",
                                  StartTime >   as.POSIXct(as.Date("2022-01-03")) & StartTime <   as.POSIXct(as.Date("2022-01-06"))  ~ "ZDALNIE",
                                  StartTime >   as.POSIXct(as.Date("2022-01-17"))  ~ "PW",
                                  TRUE ~ "WEEKEND"
            )
          ) %>% 
        filter(Period %in% input$period) 
        
        
            df <- df %>% 
            group_by(User, ActivityType) %>% 
            summarise(odl = sum(Distance)/1000, czas = (sum(EndtimeStampInMS)-sum(StartingtimeStampInMS))/(1000*60*60)  ) %>% 
            filter(User == input$whichUser) %>% 
            rename(value = input$kategoria) %>% 
            mutate(ActivityType = fct_reorder(ActivityType, value, .desc = F)) 
        
        p <- ggplot(df, aes(x = ActivityType, y=value) ) +
            geom_col( fill="brown", width=0.5) +
            theme_bw()+
            scale_x_discrete(breaks=df$ActivityType,
                             labels=stri_replace_all_fixed(df$ActivityType, '_', ' ')) +
            coord_flip()+
          theme(plot.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))
        
        if(input$kategoria == "odl"){
            p <- p +
                labs(title = "Distance travelled by Activity Type",
                     y = "Distance[km]",
                     x = "")
        }else{
            p <- p +
                labs(title = "Time spent by Activity Type",
                     y = "Time[h]",
                     x = "")
        }
        
      }else{
        p <-  ggplot()+theme_bw()+theme(plot.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))+
          labs(title = "Select at least one period!")
      }
        
        ggplotly(p) %>% config(displayModeBar = F)
    })
    
    output$liniowy <- plotly::renderPlotly({
        
        x <- data.frame(User = c("User3", "User3"), data = c(daty[1], daty[2]), dystans = c(0, 0), cum_distance = c(0,0))
        
        acti <- rbind(acti, x)
        
        p <- acti %>%
            ggplot(aes(x=data, y = cum_distance, color=User)) +
            geom_line()+
            labs(title = "Total distance travelled",
                 y = "Distance[km]",
                 x = "")+
            theme_bw() +
            scale_x_continuous(breaks= daty[seq(1, 29, by=4)])+
            theme(axis.text.x = element_text(angle = 45))+
          scale_color_brewer(palette = "Dark2")+
          theme(plot.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))+
          theme(legend.background = element_rect(fill = "#F2E5A5", color = "#F2E5A5"))
        
        ggplotly(p) %>% config(displayModeBar = F)
    }
    )
}

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Map", tabName = "mapka", icon = icon("map")),
        menuItem("How much did we travel?", tabName = "grzegorzk", icon = icon("running")),
        menuItem("How did we travel?", tabName = "marcelw", icon = icon("road"))
    )
)

body <- dashboardBody(
  
  use_theme(mytheme), 

    tabItems(
        tabItem(tabName = "mapka",
                fluidRow(
                    column(width = 3, box(status = "danger", width = NULL,
                                          selectInput("users", "User",
                                                      choices = unique(df$User), selected = 1),
                                          dateInput("DatesMerge",
                                  "Choose date:",
                                  min = min(df$data),
                                  max = max(df$data),
                                  value = as.Date("2022-01-14"),
                                  format="yyyy-mm-dd"),
                                  tags$h5(tags$b("Time inputs dont work for info Boxes!")),
                        timeInput("timeS", "Start time:", minute.steps = 5, value = strptime("11:00:00", "%T")),
                        timeInput("timeE", "End time:", minute.steps = 5, value = strptime("21:00:00", "%T"))),
                        
                    infoBoxOutput("distanceBox", width = NULL),
                    infoBoxOutput("transportBox", width = NULL),
                    infoBoxOutput("stepBox", width = NULL)
                    ),
                    box(width = 8, status = "danger",
                        shinycssloaders::withSpinner(
                          leafletOutput("Mapka", height = 642)
                        )
                    )
                    )
        ),
        
        tabItem(tabName = "grzegorzk",
                fluidRow(
                  box(status = "danger",width = 4,
                      tags$h4(tags$b("The graph shows Party index, which is calculated as the time spent active between 0 AM and 4 AM.")),
                      plotOutput("SecondGKPlot",height = 967)),
                  column(width=8,
                    box(status = "danger",width = NULL,
                      
                        radioButtons("weekday", "Do you want to see the activity distribution throughout the week or day?", c("week", "day")),
                        sliderInput("MaxDistance", "What is the longest distance in km you want to see on the graph?", 10, 500, 40),
                        plotlyOutput("FirstGKPlot")
                    ),
                    box(status = "danger",width = NULL,
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("liniowy"))))
                    

                    
                           
                        
                  )
               
                
        ),
        tabItem(tabName = "marcelw",
                fluidRow(
                    box(width =6,status = "danger", height=800,
                        selectInput(
                            inputId = "whichUser",
                            label = "Select user:",
                            choices = list("User1", "User2", "User3"),
                            selected = "User1"
                        ),
                        column(width = 6,
                        radioButtons("kategoria", 
                                     "Select parameter:",
                                     choiceNames = c("Distance", "Time"),
                                     choiceValues = c("odl", "czas"),
                                     selected = "odl"
                        )
                        ),
                        column(width = 6,
                        checkboxGroupInput("period", 
                                           "Select period:",
                                           choices = list("Weekends and holidays" ="WEEKEND", 
                                                          "Normal classes" = "PW",
                                                          "Online classes" = "ZDALNIE"),
                                           selected = "PW",
                        )
                        ),
                        column(width =12,
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("TypAktywnosci", height=600)
                        )                  
                        )
                         
                    ),
                    box(width = 6, status = "danger", height=800,
                        
                        dateRangeInput(
                            inputId = "days",
                            label = "Select range of dates:",
                            start = daty[1],
                            end = daty[length(daty)],
                            min = daty[1],
                            max = daty[length(daty)],
                            format = "yyyy-mm-dd"
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput("carbon", height=600)),
                        h5("Estimated carbon footprint is calculated on the grounds of the data about 2018 year in UK. They
                        include carbon dioxide, but also other greenhouse gases.
We assumed that
 every car journey was in medium petrol car with 3 people inside;
 and average number of passengers in bus was 17." ),
                        h5("Source: https://ourworldindata.org/grapher/carbon-footprint-travel-mode")
                    )
                ),
                
#                 
#                 fluidRow(
#                     box(status = "danger",
#                         shinycssloaders::withSpinner(
#                             plotly::plotlyOutput("TypAktywnosci")
#                         )    
#                     ),
#                     box(status = "danger",
#                         shinycssloaders::withSpinner(
#                             plotly::plotlyOutput("carbon")),
#                         h5("Estimated carbon footprint is calculated on the grounds of the data about 2018 year in UK. They
#                         include carbon dioxide, but also other greenhouse gases.
# We assumed that
#  every car journey was in medium petrol car with 3 people inside;
#  and average number of passengers in bus was 17." ),
#                         h5("Source: https://ourworldindata.org/grapher/carbon-footprint-travel-mode")
#                         )
#                     )
#                 
                
                    
                    
                        
                   
        )
        
    )
)


app_ui <- dashboardPage(
    dashboardHeader(title = "Our journey"),
    sidebar,
    body
)

shinyApp(app_ui, server)
