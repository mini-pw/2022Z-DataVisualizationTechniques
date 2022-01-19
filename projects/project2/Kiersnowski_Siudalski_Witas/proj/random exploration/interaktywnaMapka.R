library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(leaflet)
library(shinyTime)



df <- read.csv("PointstestData.csv")
df <- df %>% mutate(long = Longitude / 1e7, lat = Latitude / 1e7) %>% 
    mutate(data = as.Date(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"))) %>%
    mutate(hm = format(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"), "%H:%M")) %>%
    mutate(Color = case_when(
        User == "User1" ~ "red",
        User == "User2" ~ "purple"
    ))



ui <- fluidPage(

    titlePanel("Mapka"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("DatesMerge",
                        "Choose date:",
                        min = min(df$data),
                        max = max(df$data),
                        value = median(df$data),
                        timeFormat="%Y-%m-%d"),
            timeInput("timeS", "Start time:", minute.steps = 5),
            timeInput("timeE", "End time:", minute.steps = 5),
            checkboxGroupInput("users", 
                               "Użytkownicy:", 
                               choices = unique(df$User),
                               selected = 1)
        ),

        mainPanel(
           leafletOutput("distPlot")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderLeaflet({
        df2 <- df %>% 
            filter(User %in% input$users) %>%
            filter(data == input$DatesMerge &
                       hm > format(as.POSIXct(input$timeS), "%H:%M") &
                       hm < format(as.POSIXct(input$timeE), "%H:%M"))
        map3 <- leaflet(df2) %>% addTiles() %>% 
            addAwesomeMarkers(~long, ~lat, icon = makeAwesomeIcon(icon = "map-marker", markerColor = ~Color, library = "fa"))
        nn <- nrow(df2)-1
        for (i in 1:nn) 
            map3 <- map3 %>% 
            addPolylines(lat=c(df2[i,]$lat,df2[i+1,]$lat),lng=c(df2[i,]$long,df2[i+1,]$long), color = df2[i,]$Color)
        
        esri <- grep("^Esri", providers, value = TRUE)
        esri <- esri[c(2,5)]
        
        for (provider in esri) {
            map3 <- map3 %>% addProviderTiles(provider, group = provider)
        }
        
        map3 %>%
            addLayersControl(baseGroups = names(esri),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
                       position = "bottomleft") %>%
            htmlwidgets::onRender("
                function(el, x) {
                    var myMap = this;
                    myMap.on('baselayerchange',
                        function (e) {
                            myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                        })
                }")
        
        
        
        
    })
    
}


shinyApp(ui = ui, server = server)
