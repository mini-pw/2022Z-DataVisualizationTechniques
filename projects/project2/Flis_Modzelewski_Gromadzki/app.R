library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(scales)
library(bslib)
library(shiny)
library(wordcloud2)
library(shinyWidgets)
library(shinythemes)
library(fontawesome)
library(shinycssloaders)

source("plot_3.R")

Sys.setlocale("LC_ALL","English")

dfMG <- read.csv("ytMG.csv",encoding = "UTF-8")
dfMF <- read.csv("ytMF.csv",encoding = "UTF-8")
dfTM <- read.csv("ytTM.csv",encoding = "UTF-8")

dfMGw <- read.csv("wyszukiwanieMG.csv",encoding = "UTF-8")
dfMFw <- read.csv("wyszukiwanieMF.csv",encoding = "UTF-8")
dfTMw <- read.csv("wyszukiwanieTM.csv",encoding = "UTF-8")

dffinalMG <- dfMG %>% 
    select(title,subtitles.0.name,time)

dffinalMF <- dfMF %>% 
    select(title,subtitles.0.name,time)

dffinalTM <- dfTM %>% 
    select(title,subtitles.0.name,time)

dffinalMGw <- dfMGw %>% 
    select(title,time)

dffinalMFw <- dfMFw %>% 
    select(title,time)

dffinalTMw <- dfTMw %>% 
    select(title,time)




ui11 <- fluidPage(
    

    tags$head( tags$style(HTML(".fa{font-size: 20px;}"))),
    
    titlePanel("Watching"),
    
    fluidRow(
        column(2,
               wellPanel(
                   dateRangeInput("przedzialMG1",
                                  "From when to when?",
                                  start="2018-06-22",
                                  end="2022-01-01",
                                  min="2018-06-22",
                                  max="2022-01-01"),
                   
                   radioButtons("rodzajMG1",
                                "Interval",
                                c("Hour of the day","Day of the week","Month of the Year"))),
        ),
        column(10,
               shinycssloaders::withSpinner(plotly::plotlyOutput("plot11"),color = "#e95420"), 
        )
    ),
    
    hr(),
    
    
    titlePanel("Searching"),
    
    fluidRow(
        column(2,
               wellPanel(
                   dateRangeInput("przedzialMG1w",
                                  "From when to when?",
                                  start="2015-04-21",
                                  end="2022-01-01",
                                  min="2015-04-21",
                                  max="2022-01-01"),
                   
                   radioButtons("rodzajMG1w",
                                "Interval",
                                c("Hour of the day","Day of the week","Month of the Year"))),
        ),
        column(10,
               shinycssloaders::withSpinner(plotly::plotlyOutput("plot11w"),color = "#e95420"), 
        )
    )
)


ui12 <- fluidPage(
    
    
    titlePanel("Watching"),
    
    fluidRow(
        column(2,
               wellPanel(
                   dateRangeInput("przedzialMF1",
                                  "From when to when?",
                                  start="2019-01-10",
                                  end="2021-12-19",
                                  min="2019-01-10",
                                  max="2021-12-19"),
                   
                   radioButtons("rodzajMF1",
                                "Interval",
                                c("Hour of the day","Day of the week","Month of the Year"))),
        ),
        column(10,
               shinycssloaders::withSpinner(plotly::plotlyOutput("plot12"),color = "#e95420"), 
        )
    ),
    
    hr(),
    
    
    titlePanel("Searching"),
    
    fluidRow(
        column(2,
               wellPanel(
                   dateRangeInput("przedzialMF1w",
                                  "From when to when?",
                                  start="2015-05-31",
                                  end="2021-12-19",
                                  min="2015-05-31",
                                  max="2021-12-19"),
                   
                   radioButtons("rodzajMF1w",
                                "Interval",
                                c("Hour of the day","Day of the week","Month of the Year"))),
        ),
        column(10,
               shinycssloaders::withSpinner(plotly::plotlyOutput("plot12w"),color = "#e95420"), 
        )
    )
)

###
ui13 <- fluidPage(
    
    
    titlePanel("Watching"),
    
    fluidRow(
        column(2,
               wellPanel(
                   dateRangeInput("przedzialTM1",
                                  "From when to when?",
                                  start="2016-02-10",
                                  end="2021-12-14",
                                  max="2021-12-14",
                                  min="2016-02-10"),
                   
                   radioButtons("rodzajTM1",
                                "Interval",
                                c("Hour of the day","Day of the week","Month of the Year"))),
        ),
        column(10,
               shinycssloaders::withSpinner(plotly::plotlyOutput("plot13"),color = "#e95420"), 
        )
    ),
    
    hr(),
    
    
    titlePanel("Searching"),
    
    fluidRow(
        column(2,
               wellPanel(
                   dateRangeInput("przedzialTM1w",
                                  "From when to when?",
                                  start="2017-01-13",
                                  end="2021-12-13",
                                  min="2017-01-13",
                                  max="2021-12-13"),
                   
                   radioButtons("rodzajTM1w",
                                "Interval",
                                c("Hour of the day","Day of the week","Month of the Year"))),
        ),
        column(10,
               shinycssloaders::withSpinner(plotly::plotlyOutput("plot13w"),color = "#e95420"), 
        )
    )
)



ui21 <- fluidPage(
    
    titlePanel("Wordcloud - Alice"),
    tags$head(tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #e95420;
                                                  border-top: 1px solid #e95420 ;
                                                  border-bottom: 1px solid #e95420 ;}"))),
    
    tags$head(tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
              background: #e95420;
              border-top: 1px solid #e95420 ;
              border-bottom: 1px solid #e95420 ;}"))),    
   
    
    fluidRow(
        column(4,
               wellPanel(
                   sliderInput("freqMG2",
                               "Minimal frequency of a word",
                               min=0,
                               max=400,
                               value=80),
                   sliderInput("ileMG2",
                               "Maximum number of words",
                               min=0,
                               max=500,
                               value=100),
                   
                
               ))),
    fluidRow(
        div(style = "height:20px"),
        tags$head(
            tags$style(HTML('div#wcLabel {display: none;}'))
        ),
        wordcloud2Output("plot21"),
    )
)


ui22 <- fluidPage(
    
    
    titlePanel("Wordcloud - Bob"),
    tags$head(tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {
                                                  background: #e95420;
                                                  border-top: 1px solid #e95420 ;
                                                  border-bottom: 1px solid #e95420 ;}"))),
    
    tags$head(tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {
              background: #e95420;
              border-top: 1px solid #e95420 ;
              border-bottom: 1px solid #e95420 ;}"))), 
    
    fluidRow(
        column(4,
               wellPanel(
                   sliderInput("freqMF2",
                               "Minimal frequency of a word",
                               min=0,
                               max=400,
                               value=80),
                   sliderInput("ileMF2",
                               "Maximum number of words",
                               min=0,
                               max=500,
                               value=100),
                   
                   
               ))),
    fluidRow(
        div(style = "height:20px"),
        tags$head(
            tags$style(HTML('div#wcLabel {display: none;}'))
        ),
        wordcloud2Output("plot22"),
    )
)

###
ui23 <- fluidPage(
    
    
    titlePanel("Wordcloud - Charlie"),
    tags$head(tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {
                                                  background: #e95420;
                                                  border-top: 1px solid #e95420 ;
                                                  border-bottom: 1px solid #e95420 ;}"))),
    
    tags$head(tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {
              background: #e95420;
              border-top: 1px solid #e95420 ;
              border-bottom: 1px solid #e95420 ;}"))), 
    
    fluidRow(
        column(4,
               wellPanel(
                   sliderInput("freqTM2",
                               "Minimal frequency of a word",
                               min=0,
                               max=400,
                               value=80),
                   sliderInput("ileTM2",
                               "Maximum number of words",
                               min=0,
                               max=500,
                               value=100),
                   
                   
               ))),
    fluidRow(
        div(style = "height:20px"),
        tags$head(
            tags$style(HTML('div#wcLabel {display: none;}'))
        ),
        wordcloud2Output("plot23"),
    )
)

ui3 <- fluidPage(
    
    
    titlePanel("Favorite singers"),
    
    fluidRow(
        column(4,
               wellPanel(
                   dateRangeInput("przedzial3",
                                  "From when to when?",
                                  start="2018-06-22",
                                  end="2022-01-01",
                                  min="2018-06-22",
                                  max="2022-01-01")
                   
                   
               ),),
        column(8,
               shinycssloaders::withSpinner(plotly::plotlyOutput("plot3"),color = "#e95420"))
    ),
    
    hr(),
    
    
    titlePanel("Watched videos of individual authors"),
    
    
    fluidRow(
        column(4,
               wellPanel(
                   dateInput("kiedy4",
                             "Choose date",
                             min="2018-06-22",
                             max="2022-01-01",
                             value="2022-01-01"),
                   
                   textInput("kto4",
                             "Channel name",
                             "Enter channel name"),
                   
               ),
        ),
        column(8,
               shinycssloaders::withSpinner(htmlOutput("plot4"),color = "#e95420"),
               tags$head(tags$style("#plot4{
                                 font-size: 20px;
                                 }"
               )
               )
        
        )
    )
)



server <- function(input, output) {

    output$plot11 <- plotly::renderPlotly({
        
        if(input$rodzajMG1=="Hour of the day"){
        
        wykres11 <- dffinalMG %>% 
            mutate(data = as_datetime(time)) %>% 
            group_by(hours = format(as.POSIXct(data), format = "%H")) %>% 
            filter(data >= input$przedzialMG1[1] & data <= input$przedzialMG1[2]) %>% 
            summarise(n = n()) %>% 
            ggplot(aes(x = hours, y = n)) +
            geom_col(fill="#e95420") +
            labs(title = paste("At what time did I watch the most?","From",input$przedzialMG1[1], "to", input$przedzialMG1[2]),
                 y = "Number of videos",
                 x = "Hour of the day")+
            theme_light()
        
        }
        
        if(input$rodzajMG1=="Day of the week"){
            wykres11 <- dffinalMG %>% 
                mutate(day=wday(time,label = TRUE,abbr=FALSE),data = as_datetime(time)) %>%
                filter(data >= input$przedzialMG1[1] & data <= input$przedzialMG1[2]) %>%
                group_by(day) %>% 
                summarise(n=n()) %>% 
                mutate(day=factor(day,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
                ggplot(aes(x=day,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which day did I watch the most?","From",input$przedzialMG1[1], "to", input$przedzialMG1[2]),
                     y = "Number of videos",
                     x = "Day of the week")+
                theme_light()
        }
        
        if(input$rodzajMG1=="Month of the Year"){
            wykres11 <- dffinalMG %>% 
                mutate(data = as_datetime(time)) %>% 
                filter(data >= input$przedzialMG1[1] & data <= input$przedzialMG1[2]) %>%
                group_by(mon = months(as.POSIXct(data))) %>% 
                summarise(n=n()) %>% 
                mutate(mon=factor(mon,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))) %>% 
                ggplot(aes(x=mon,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which month did I watch the most?","From",input$przedzialMG1[1], "to", input$przedzialMG1[2]),
                     y = "Number of videos",
                     x = "Month of the Year")+
                theme_light()
        }
        
        
        ggplotly(wykres11)
    })
    
    output$plot11w <- plotly::renderPlotly({
        
        if(input$rodzajMG1w=="Hour of the day"){
            
            wykres11w <- dffinalMGw %>% 
                mutate(data = as_datetime(time)) %>% 
                group_by(hours = format(as.POSIXct(data), format = "%H")) %>% 
                filter(data >= input$przedzialMG1w[1] & data <= input$przedzialMG1w[2]) %>% 
                summarise(n = n()) %>% 
                ggplot(aes(x = hours, y = n)) +
                geom_col(fill="#e95420") +
                labs(title = paste("At what time did I search the most?","From",input$przedzialMG1w[1], "to", input$przedzialMG1w[2]),
                     y = "Number of searches",
                     x = "Hour of the day")+
                theme_light()
            
        }
        
        if(input$rodzajMG1w=="Day of the week"){
            wykres11w <- dffinalMGw %>% 
                mutate(day=wday(time,label = TRUE,abbr=FALSE),data = as_datetime(time)) %>%
                filter(data >= input$przedzialMG1w[1] & data <= input$przedzialMG1w[2]) %>%
                group_by(day) %>% 
                summarise(n=n()) %>% 
                mutate(day=factor(day,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
                ggplot(aes(x=day,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which day did I search the most?","From",input$przedzialMG1w[1], "to", input$przedzialMG1w[2]),
                     y = "Number of searches",
                     x = "Day of the week")+
                theme_light()
        }
        
        if(input$rodzajMG1w=="Month of the Year"){
            wykres11w <- dffinalMGw %>% 
                mutate(data = as_datetime(time)) %>% 
                filter(data >= input$przedzialMG1w[1] & data <= input$przedzialMG1w[2]) %>%
                group_by(mon = months(as.POSIXct(data))) %>% 
                summarise(n=n()) %>% 
                mutate(mon=factor(mon,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))) %>% 
                ggplot(aes(x=mon,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which month did I search the most?","From",input$przedzialMG1w[1], "to", input$przedzialMG1w[2]),
                     y = "Number of searches",
                     x = "Month of the Year")+
                theme_light()
        }
        
        
        ggplotly(wykres11w)
    })
    

    output$plot12 <- plotly::renderPlotly({
        
        if(input$rodzajMF1=="Hour of the day"){
            
            wykres12 <- dffinalMF %>% 
                mutate(data = as_datetime(time)) %>% 
                group_by(hours = format(as.POSIXct(data), format = "%H")) %>% 
                filter(data >= input$przedzialMF1[1] & data <= input$przedzialMF1[2]) %>% 
                summarise(n = n()) %>% 
                ggplot(aes(x = hours, y = n)) +
                geom_col(fill="#e95420") +
                labs(title = paste("At what time did I watch the most?","From",input$przedzialMF1[1], "to", input$przedzialMF1[2]),
                     y = "Number of videos",
                     x = "Hour of the day")+
                theme_light()
            
        }
        
        if(input$rodzajMF1=="Day of the week"){
            wykres12 <- dffinalMF %>% 
                mutate(day=wday(time,label = TRUE,abbr=FALSE),data = as_datetime(time)) %>% 
                filter(data >= input$przedzialMF1[1] & data <= input$przedzialMF1[2]) %>%
                group_by(day) %>% 
                summarise(n=n()) %>% 
                mutate(day=factor(day,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
                ggplot(aes(x=day,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which day did I watch the most?","From",input$przedzialMF1[1], "to", input$przedzialMF1[2]),
                     y = "Number of videos",
                     x = "Day of the week")+
                theme_light()
        }
        
        if(input$rodzajMF1=="Month of the Year"){
            wykres12 <- dffinalMF %>% 
                mutate(data = as_datetime(time)) %>% 
                filter(data >= input$przedzialMF1[1] & data <= input$przedzialMF1[2]) %>%
                group_by(mon = months(as.POSIXct(data))) %>% 
                summarise(n=n()) %>% 
                mutate(mon=factor(mon,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))) %>% 
                ggplot(aes(x=mon,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which month did I watch the most?","From",input$przedzialMF1[1], "to", input$przedzialMF1[2]),
                     y = "Number of videos",
                     x = "Month of the Year")+
                theme_light()
        }
        
        
        ggplotly(wykres12)
    })
    
    
    output$plot12w <- plotly::renderPlotly({
        
        if(input$rodzajMF1w=="Hour of the day"){
            
            wykres12w <- dffinalMFw %>% 
                mutate(data = as_datetime(time)) %>% 
                group_by(hours = format(as.POSIXct(data), format = "%H")) %>% 
                filter(data >= input$przedzialMF1w[1] & data <= input$przedzialMF1w[2]) %>% 
                summarise(n = n()) %>% 
                ggplot(aes(x = hours, y = n)) +
                geom_col(fill="#e95420") +
                labs(title = paste("At what time did I search the most?","From",input$przedzialMF1w[1], "to", input$przedzialMF1w[2]),
                     y = "Number of searches",
                     x = "Hour of the day")+
                theme_light()
            
        }
        
        if(input$rodzajMF1w=="Day of the week"){
            wykres12w <- dffinalMFw %>% 
                mutate(day=wday(time,label = TRUE,abbr=FALSE),data = as_datetime(time)) %>%
                filter(data >= input$przedzialMF1w[1] & data <= input$przedzialMF1w[2]) %>%
                group_by(day) %>% 
                summarise(n=n()) %>% 
                mutate(day=factor(day,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
                ggplot(aes(x=day,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which day did I search the most?","From",input$przedzialMF1w[1], "to", input$przedzialMF1w[2]),
                     y = "Number of searches",
                     x = "Day of the week")+
                theme_light()
        }
        
        if(input$rodzajMF1w=="Month of the Year"){
            wykres12w <- dffinalMFw %>% 
                mutate(data = as_datetime(time)) %>% 
                filter(data >= input$przedzialMG1w[1] & data <= input$przedzialMG1w[2]) %>%
                group_by(mon = months(as.POSIXct(data))) %>% 
                summarise(n=n()) %>% 
                mutate(mon=factor(mon,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))) %>% 
                ggplot(aes(x=mon,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which month did I search the most?","From",input$przedzialMF1w[1], "to", input$przedzialMF1w[2]),
                     y = "Number of searches",
                     x = "Month of the Year")+
                theme_light()
        }
        
        
        ggplotly(wykres12w)
    })
    
    
    output$plot13 <- plotly::renderPlotly({
        
        if(input$rodzajTM1=="Hour of the day"){
            
            wykres13 <- dffinalTM %>% 
                mutate(data = as_datetime(time)) %>% 
                group_by(hours = format(as.POSIXct(data), format = "%H")) %>% 
                filter(data >= input$przedzialTM1[1] & data <= input$przedzialTM1[2]) %>% 
                summarise(n = n()) %>% 
                ggplot(aes(x = hours, y = n)) +
                geom_col(fill="#e95420") +
                labs(title = paste("At what time did I watch the most?","From",input$przedzialTM1[1], "to", input$przedzialTM1[2]),
                     y = "Number of videos",
                     x = "Hour of the day")+
                theme_light()
            
        }
        
        if(input$rodzajTM1=="Day of the week"){
            wykres13 <- dffinalTM %>% 
                mutate(day=wday(time,label = TRUE,abbr=FALSE),data = as_datetime(time)) %>% 
                filter(data >= input$przedzialTM1[1] & data <= input$przedzialTM1[2]) %>%
                group_by(day) %>% 
                summarise(n=n()) %>% 
                mutate(day=factor(day,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
                ggplot(aes(x=day,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which day did I watch the most?","From",input$przedzialTM1[1], "to", input$przedzialTM1[2]),
                     y = "Number of videos",
                     x = "Day of the week")+
                theme_light()
        }
        
        if(input$rodzajTM1=="Month of the Year"){
            wykres13 <- dffinalMF %>% 
                mutate(data = as_datetime(time)) %>% 
                filter(data >= input$przedzialTM1[1] & data <= input$przedzialTM1[2]) %>%
                group_by(mon = months(as.POSIXct(data))) %>% 
                summarise(n=n()) %>% 
                mutate(mon=factor(mon,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))) %>% 
                ggplot(aes(x=mon,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which month did I watch the most?","From",input$przedzialTM1[1], "to", input$przedzialTM1[2]),
                     y = "Number of videos",
                     x = "Month of the Year")+
                theme_light()
        }
        
        
        ggplotly(wykres13)
    })
    
    
    output$plot13w <- plotly::renderPlotly({
        
        if(input$rodzajTM1w=="Hour of the day"){
            
            wykres13w <- dffinalTMw %>% 
                mutate(data = as_datetime(time)) %>% 
                group_by(hours = format(as.POSIXct(data), format = "%H")) %>% 
                filter(data >= input$przedzialTM1w[1] & data <= input$przedzialTM1w[2]) %>% 
                summarise(n = n()) %>% 
                ggplot(aes(x = hours, y = n)) +
                geom_col(fill="#e95420") +
                labs(title = paste("At what time did I search the most?","From",input$przedzialTM1w[1], "to", input$przedzialTM1w[2]),
                     y = "Number of searches",
                     x = "Hour of the day")+
                theme_light()
            
        }
        
        if(input$rodzajTM1w=="Day of the week"){
            wykres13w <- dffinalTMw %>% 
                mutate(day=wday(time,label = TRUE,abbr=FALSE),data = as_datetime(time)) %>%
                filter(data >= input$przedzialTM1w[1] & data <= input$przedzialTM1w[2]) %>%
                group_by(day) %>% 
                summarise(n=n()) %>% 
                mutate(day=factor(day,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
                ggplot(aes(x=day,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which day did I search the most?","From",input$przedzialTM1w[1], "to", input$przedzialTM1w[2]),
                     y = "Number of searches",
                     x = "Day of the week")+
                theme_light()
        }
        
        if(input$rodzajTM1w=="Month of the Year"){
            wykres13w <- dffinalTMw %>% 
                mutate(data = as_datetime(time)) %>% 
                filter(data >= input$przedzialTM1w[1] & data <= input$przedzialTM1w[2]) %>%
                group_by(mon = months(as.POSIXct(data))) %>% 
                summarise(n=n()) %>% 
                mutate(mon=factor(mon,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))) %>% 
                ggplot(aes(x=mon,y=n))+
                geom_col(fill="#e95420")+
                labs(title = paste("Which month did I search the most?","From",input$przedzialTM1w[1], "to", input$przedzialTM1w[2]),
                     y = "Number of searches",
                     x = "Month of the Year")+
                theme_light()
        }
        
        
        ggplotly(wykres13w)
    })

    
    output$plot21 <- renderWordcloud2({
        
        wykres21 <- dffinalMG %>% 
            mutate(data = as_datetime(time)) %>% 
            group_by(subtitles.0.name) %>% 
            filter(subtitles.0.name!="") %>% 
            summarise(freq = n()) %>% 
            filter(freq>=input$freqMG2) %>% 
            arrange(desc(freq)) %>% 
            rename(word=subtitles.0.name) %>% 
            head(input$ileMG2)

        wordcloud2(wykres21,size = 0.75)
        
        
        
    }) 
    
    
    output$plot22 <- renderWordcloud2({
        
        wykres22 <- dffinalMF %>% 
            mutate(data = as_datetime(time)) %>% 
            group_by(subtitles.0.name) %>% 
            filter(subtitles.0.name!="") %>% 
            summarise(freq = n()) %>% 
            filter(freq>=input$freqMF2) %>%
            arrange(desc(freq)) %>% 
            rename(word=subtitles.0.name) %>% 
            head(input$ileMF2)
        
        wordcloud2(wykres22,size = 0.75)
        
        
    }) 
    
    output$plot23 <- renderWordcloud2({
        
        wykres23 <- dffinalTM %>% 
            mutate(data = as_datetime(time)) %>% 
            group_by(subtitles.0.name) %>% 
            filter(subtitles.0.name!="") %>% 
            summarise(freq = n()) %>% 
            filter(freq>=input$freqTM2) %>%
            arrange(desc(freq)) %>% 
            rename(word=subtitles.0.name) %>% 
            head(input$ileTM2)
        
        wordcloud2(wykres23,size = .75)
        
        
    })
    
    output$plot3 <- plotly::renderPlotly({
        
        top_singers_plot(dffinalMG, dffinalMF, dffinalTM, input$przedzial3[1], input$przedzial3[2])
    })
    
    
    output$plot4 <- renderUI({
        
        wykres4v1MG <- dffinalMG %>% 
            mutate(data = as_date(time)) %>% 
            filter(data==input$kiedy4) %>% 
            summarise(n=n())
        
        wykres4v2MG <- dffinalMG %>% 
            mutate(data = as_date(time)) %>% 
            filter(subtitles.0.name!="") %>% 
            filter(data==input$kiedy4,subtitles.0.name==input$kto4) %>% 
            summarise(n=n())
        
        wykres42v3MG <- dffinalMG %>% 
            mutate(data=as_date(time)) %>% 
            filter(subtitles.0.name==input$kto4) %>% 
            arrange(desc(data)) %>% 
            head(1)
        
        wykres4v1MF <- dffinalMF %>% 
            mutate(data = as_date(time)) %>% 
            filter(data==input$kiedy4) %>% 
            summarise(n=n())
        
        wykres4v2MF <- dffinalMF %>% 
            mutate(data = as_date(time)) %>% 
            filter(subtitles.0.name!="") %>% 
            filter(data==input$kiedy4,subtitles.0.name==input$kto4) %>% 
            summarise(n=n())
        
        wykres42v3MF <- dffinalMF %>% 
            mutate(data=as_date(time)) %>% 
            filter(subtitles.0.name==input$kto4) %>% 
            arrange(desc(data)) %>% 
            head(1)
        
        wykres4v1TM <- dffinalTM %>% 
            mutate(data = as_date(time)) %>% 
            filter(data==input$kiedy4) %>% 
            summarise(n=n())
        
        wykres4v2TM <- dffinalTM %>% 
            mutate(data = as_date(time)) %>% 
            filter(subtitles.0.name!="") %>% 
            filter(data==input$kiedy4,subtitles.0.name==input$kto4) %>% 
            summarise(n=n())
        
        wykres42v3TM <- dffinalTM %>% 
            mutate(data=as_date(time)) %>% 
            filter(subtitles.0.name==input$kto4) %>% 
            arrange(desc(data)) %>% 
            head(1)
        
        
            br <- ""
            str0 <- "Please enter channel name to see results"
            str1 <- "Person 1:"
            str2 <- paste("On ",input$kiedy4," I watched ",wykres4v1MG$n," videos, ",wykres4v2MG$n," from author ",input$kto4,".",sep="")
            str25 <- paste("The last time I saw this author's video was on ",wykres42v3MG$data,sep="")
            str3 <- "Person 2:"
            str4 <- paste("On ",input$kiedy4," I watched ",wykres4v1MF$n," videos, ",wykres4v2MF$n," from author ",input$kto4,".",sep="")
            str45 <- paste("The last time I saw this author's video was on ",wykres42v3MF$data,sep="")
            str5 <- "Person 3:"
            str6 <- paste("On ",input$kiedy4," I watched ",wykres4v1TM$n," videos, ",wykres4v2TM$n," from author ",input$kto4,".",sep="")
            str65 <- paste("The last time I saw this author's video was on ",wykres42v3TM$data,sep="")
            str7 <- "I've never seen this author's video"
    
            
            if(dim(wykres42v3MG)[1]==0)
                str25 <- str7
            
            if(dim(wykres42v3MF)[1]==0)
                str45 <- str7
            
            if(dim(wykres42v3TM)[1]==0)
                str65 <- str7
            
            
            
            if(input$kto4=="Enter channel name" || input$kto4==""){
                HTML(paste(str0))
            }
            else{
            HTML(paste(str1, str2, str25, br, str3, str4, str45, br, str5, str6, str65, sep = '<br/>'))}
        
        
        
    })
    
}

uiv1 <- fluidPage(navbarPage(
    title="Projekt JA",
    tabPanel("Alice",ui11,icon = icon("female")),
    tabPanel("Bob",ui12, icon = icon("male")),
    tabPanel("Charlie",ui13, icon = icon("male")),
    theme=shinytheme("united")
))

uiv2 <- fluidPage(navbarPage(
    title="Projekt JA",
    tabPanel("Alice",ui21,icon = icon("female")),
    tabPanel("Bob",ui22,icon = icon("male")),
    tabPanel("Charlie",ui23,icon = icon("male")),
    theme=shinytheme("united")
))



ui <- fluidPage(navlistPanel(
    tabPanel("Plots",uiv1,icon = icon("chart-bar")),
    tabPanel("Wordcloud",uiv2,icon = icon("cloud")),
    tabPanel("Analytics",ui3,icon = icon("server")),
    widths = c(2,10)
))


shinyApp(ui, server)