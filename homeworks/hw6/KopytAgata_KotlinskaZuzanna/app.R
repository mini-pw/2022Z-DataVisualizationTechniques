#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggrepel)  
library(dplyr)
library(bslib)
require(maps)
library(RColorBrewer)

#Obróbka ramki danych
df <- read.csv('complete.csv')

##ramka do pierwszej zakładki
df1 <- df %>% 
    mutate(birth_date =if_else(!is.na(ymd(birth_date)),
                               ymd(birth_date), 
                               ymd(paste(substr(birth_date, 1, 4),'-01-01',sep='')))) %>%
    drop_na(birth_date) %>% 
    mutate(dateAwarded = ymd(dateAwarded)) %>% 
    mutate(age = ifelse(!is.na(trunc((birth_date %--% dateAwarded) / years(1))),
                        trunc((birth_date %--% dateAwarded) / years(1)),
                        awardYear-year(birth_date)))

##ramka do drugiej zakładki
df2 <- df %>% 
    select(awardYear, category, birth_countryNow) 

# Pierwsza zakładka
ui <- fluidPage(
    
    # Kolor sliderów
    # shinyWidgets::chooseSliderSkin(color = '#D0C19F'),
    shinyWidgets::chooseSliderSkin(skin = "Flat",color = 'blue'),
    # shinyWidgets::setSliderColor('#D0C19F',sliderId =c(1,2)),
    
    # Tytuł
    titlePanel("Age of the Nobelists"),
    
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("chosenCategory", "Select category: ", sort(distinct(df1,category)$category)),
            sliderInput("timeRange", 
                        label = "Select period of time: ",min = 1901, max = 2019,
                        value = c(1901, 2019), sep = "")
        ),
        
        
        # Wyświetl
        mainPanel(
            shinycssloaders::withSpinner(plotly::plotlyOutput("agePlot", height =600),
                                         type = getOption("spinner.type", default = 2),
                                         color = getOption("spinner.color", default = '#D0C19F'),
                                         color.background = getOption("spinner.color.background", default ='white'))
        )
    )
)

# Druga zakładka
ui2 <- fluidPage(
    
    #Tytuł
    titlePanel("Number of the Nobelists worldwide"),
    
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("Category", "Select category: ", sort(distinct(df2,category)$category)),
            sliderInput("Range", 
                        label = "Select period of time: ",min = 1901, max = 2019,
                        value = c(1901, 2019), sep = "")
        ),
        
        # Wyświetl
        mainPanel(
            shinycssloaders::withSpinner(plotly::plotlyOutput("mapPlot", width=1200, height=650),
                                         type = getOption("spinner.type", default = 2),
                                         color = getOption("spinner.color", default = '#D0C19F'),
                                         color.background = getOption("spinner.color.background", default ='white'))
        )
    )
)

# Serwer
server <- function(input, output) {
    
    output$agePlot <- plotly::renderPlotly({
        
        p <- df1 %>%filter(category==input$chosenCategory & 
                               awardYear>=input$timeRange[1]&
                               awardYear<=input$timeRange[2]) %>% 
            ggplot(aes(x=awardYear,y=age, color=gender, text = paste("year: ", awardYear,
                                                                     "<br>age: ", age)))+
            theme_minimal()+
            theme(axis.text.x = element_text(angle = 70))+
            scale_color_manual(values = c("male" = "#6AB9A8",
                                          "female"="#E3698A"))+
            scale_x_continuous(breaks = seq(input$timeRange[1],input$timeRange[2],5),
                               limits = c(input$timeRange[1],input$timeRange[2]))+
            scale_y_continuous(breaks = seq(0,100,5),
                               limits =c(0,100), 
                               expand=c(0,0))+
            labs(x = "year of reciving the award", 
                 y = 'age of the nobelist')+
            geom_point(size =3)
        
        plotly::ggplotly(p, tooltip =  'text')
    }) %>% bindCache(input$timeRange, input$chosenCategory)
    
    output$mapPlot <- plotly::renderPlotly({
        
        df2 <- df2 %>% filter(category %in% input$Category, awardYear >=input$Range[1],
                              awardYear<=input$Range[2]) %>% 
            group_by(birth_countryNow) %>% 
            summarise(number = n()) 
        
        country <- map_data("world")
        
        p <- country %>% 
            left_join(df2, by = c("region" = "birth_countryNow")) %>% 
            ggplot(aes(long, lat)) + 
            geom_polygon(aes(group = group, fill = number)) +
            scale_fill_gradient(low = "#a3bfb9", high = "#0a8268", na.value = "grey")
        
        
        plotly::ggplotly(p, tooltip = "all")
    }) %>% bindCache(input$Category, input$Range)
}

app_ui <- navbarPage(
    theme = bs_theme(bootswatch = "lux"), 
    title = "Nobel Prize",
    tabPanel("Nobelists by age", ui, icon = icon("fas fa-baby")),
    tabPanel("Number of the Nobelists worldwide", ui2, icon = icon("fas fa-globe"))
)

# Run the application 
shinyApp(ui = app_ui, server = server)