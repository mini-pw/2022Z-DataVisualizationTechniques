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
library(shinyWidgets)

activitydf <- read.csv("ActivitySegmenttestData.csv")

pointsdf <- read.csv("PointstestData.csv")

placevisitdf <- read.csv("PlacesVisitedtestData.csv") 

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

df2 <- read.csv("PlacesVisitedtestData.csv", encoding = "UTF-8")  %>% select(-PlaceId, -X)

df <- read.csv("PointstestData.csv", encoding = "UTF-8")
dfa <- read.csv("ActivitySegmenttestData.csv", encoding = "UTF-8") %>% 
    select(-X, -Distance) %>% 
    rename(startlong = StartingLongitude, startlat = StartingLatitude,
           endlong = EndingLongitude, endlat = EndingLatitude, 
           starttime = StartingtimeStampInMS, endtime = EndtimeStampInMS)

dfg1 <- dfa %>% select(startlong, startlat, starttime, User) %>%
    rename(Longitude = startlong, Latitude = startlat, TimeStampInMS = starttime)

dfg2 <- dfa %>% select(endlong, endlat, endtime, User) %>%
    rename(Longitude = endlong, Latitude = endlat, TimeStampInMS = endtime)

dfg <- rbind(dfg1, dfg2)

df <- df %>% select(-X) %>% rbind(dfg) %>%
    mutate(Activity = "UNKNOWN_ACTIVITY_TYPE", Place = NA) %>%
    mutate(long = Longitude / 1e7, lat = Latitude / 1e7, .keep = "unused") %>% 
    mutate(data = as.Date(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"))) %>%
    mutate(hm = format(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"), "%H:%M")) %>%
    mutate(Color = case_when(
        User == "User1" ~ "red",
        User == "User2" ~ "purple"
    )) %>% arrange(User, TimeStampInMS)


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
    places <- which(df$TimeStampInMS >= df2[i,]$StartTimeStampInMS &
                        df$TimeStampInMS <= df2[i,]$EndTimeStampInMS &
                        df$User == df2[i,]$User)
    if(length(places) > 0){
        df[places,]$Place <- df2[i,]$Name
    }
}



df <- df %>% mutate(ActivityColor = case_when(
    Activity == "WALKING" ~ "blue",
    Activity == "IN_PASSENGER_VEHICLE" ~ "purple",
    Activity == "IN_TRAM" ~ "yellow",
    Activity == "IN_TRAIN" ~ "darkblue",
    Activity == "UNKNOWN_ACTIVITY_TYPE" ~ "grey",
    Activity == "IN_BUS" ~ "orange",
    Activity == "IN_SUBWAY" ~ "black"
))


server <- function(input, output, session) {
    
    output$FirstGKPlot <- renderPlot({
        
        
        
        activity3<- activity2 %>% filter(User %in% input$Users)
        activity2$Weekday <- factor(activity2$Weekday, c("Mon","Tue","Wed", "Thu", "Fri", "Sat", "Sun"))
        if(input$weekday=="day"){
            ggplot(activity2, aes(y=Distance/1000, x=MiddleOfActivity, group=User, color=User, size=DurationInMinutes))+
                geom_point()+
                ylab("Distance in km")+
                xlab("Middle time of activity (hours)")+
                ylim(0,40)}
        
        else{
            ggplot(activity2, aes(y=Distance/1000, x=Weekday, group=User, color=User, size=DurationInMinutes))+
                geom_point()+
                ylab("Distance in km")+
                xlab("weekday of activity")
        }
    })
    
    output$Mapka <- renderLeaflet({
        
        df2 <- df %>% 
            filter(User %in% input$users) %>%
            filter(data == input$DatesMerge &
                       hm > format(as.POSIXct(input$timeS), "%H:%M") &
                       hm < format(as.POSIXct(input$timeE), "%H:%M"))
        
        map3 <- leaflet(df2) %>%
            setView(lng = 21.017532, lat = 52.237049, zoom = 9) %>%
            addAwesomeMarkers(~long,
                              ~lat,
                              icon = makeAwesomeIcon(
                                  icon = ~if_else(is.na(Place), "diamond", "map-marker"),
                                  library = "glyphicon",
                                  markerColor = ~Color,
                                  iconColor = "black"),
                              popup = ~Place)
        nn <- nrow(df2)-1
        if (nn > 0){
            for (i in 1:nn)
                if (df2$User[i] == df2$User[i+1]){
                    map3 <- map3 %>% 
                        addPolylines(lat=c(df2[i,]$lat,df2[i+1,]$lat),
                                     lng=c(df2[i,]$long,df2[i+1,]$long),
                                     color = df2[i,]$ActivityColor,
                                     popup = ~paste("Distance: ", round(raster::pointDistance(c(df2[i,]$long,
                                                                                                df2[i,]$lat),
                                                                                              c(df2[i+1,]$long,
                                                                                                df2[i+1,]$lat),
                                                                                              lonlat = TRUE), 0), "m"))
                }
        }
        
        esri <- c(grep("^OpenStreetMap.Mapnik", providers, value = TRUE),
                  grep("^Esri.WorldImagery", providers, value = TRUE))
        
        for (provider in esri) {
            map3 <- map3 %>% addProviderTiles(provider, group = provider)
        }
        
        
        map3 %>%
            addLayersControl(baseGroups = names(esri),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            # addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
            #            position = "bottomleft") %>%
            htmlwidgets::onRender("
                function(el, x) {
                    var myMap = this;
                    myMap.on('baselayerchange',
                        function (e) {
                            myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                        })
                }") %>%  addLegend(
                    position = "bottomright",
                    colors = unique(df$ActivityColor),
                    labels = unique(df$Activity), opacity = 1,
                    title = "Means of transport"
                )
        
    })
    

    
    output$TypAktywnosci <- plotly::renderPlotly({
        
        df <- activitydf %>% 
            group_by(User, ActivityType) %>% 
            summarise(odl = sum(Distance)/1000, czas = (sum(EndtimeStampInMS)-sum(StartingtimeStampInMS))/(1000*60*60)  ) %>% 
            filter(User == input$whichUser) %>% 
            rename(category = input$kategoria) %>% 
            mutate(ActivityType = fct_reorder(ActivityType, category, .desc = F)) 
        
        p <- ggplot(df, aes(x = ActivityType, y=category) ) +
            geom_col( ) +
            theme_bw()+
            scale_x_discrete(breaks=df$ActivityType,
                             labels=stri_replace_all_fixed(df$ActivityType, '_', ' ')) +
            coord_flip()
        
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
        
        p
    })
}

ui <- fluidPage(
    titlePanel("Page1"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput(
                inputId = "whichUser",
                label = "Choose user:",
                choices = list("User1", "User2"),
                selected = "User1"
            ),
            
            radioButtons("kategoria", 
                         "Select parameter:",
                         choiceNames = c("Distance", "Time"),
                         choiceValues = c("odl", "czas"),
                         selected = "odl"
            )
        ), 
        
        
        
        
        mainPanel(
            shinycssloaders::withSpinner(
                plotly::plotlyOutput("TypAktywnosci")
            ),
            
            
        )
    )
)


ui2 <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("Users", "Users", c("User1", "User2"), c("User1","User2")),
            radioButtons("weekday", "Do you want to see week perspective or day perspective?", c("week", "day"))
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("FirstGKPlot"),
            plotOutput("SecondGKPlot")
        )
    )
)

ui3 <- fluidPage(
    titlePanel("Mapka"),
    sidebarLayout(
        sidebarPanel(
            dateInput("DatesMerge",
                      "Choose date:",
                      min = min(df$data),
                      max = max(df$data),
                      value = median(df$data),
                      format="yyyy-mm-dd"),
            timeInput("timeS", "Start time:", minute.steps = 5),
            timeInput("timeE", "End time:", minute.steps = 5),
            checkboxGroupInput("users", 
                               "Users:", 
                               choices = unique(df$User),
                               selected = 1)
        ),
        
        mainPanel(
            leafletOutput("Mapka")
            #leafletOutput("plot2")
        )
    ),
    useShinydashboard(),
    fluidRow(box(width = 12,
        infoBox("New Orders", 42 * 10, icon = shiny::icon("credit-card")),
        infoBox("progress", 69, icon = shiny::icon("list"), color = "purple"),
        infoBox("papiez", 2137, icon = shiny::icon("thumbs-up", lib = "glyphicon"),
                color = "yellow")
    ))
)

app_ui <- navbarPage(
    title = "Projekt TWD 2",
    tabPanel("Page1", ui),
    tabPanel("Page2", ui2),
    tabPanel("Page3", ui3),
    theme = bslib::bs_theme(bootswatch = "cosmo")
)

shinyApp(app_ui, server)
