#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
# library(gganimate)

library(lubridate)
library(readr)
library(tidyverse)
library(scales)
library(dashboardthemes)
library(shinydashboard)
library(ggthemes)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)


#________________________________GLOBAL DATA____________________________________

#Viewing Activities 
AgataViewingActivity <- 
    read.csv("dane/AgataViewingActivity.csv", encoding='UTF-8')
AdamViewingActivity <- 
    read.csv("dane/AdamViewingActivity.csv", encoding='UTF-8')
WojtekViewingActivity <- 
  read.csv("dane/WojtekViewingActivity.csv", encoding='UTF-8')

# wczytaj ramke z filmami z IMDB
#   data <- read_tsv("dane/title.basics.tsv",na = "\\N", quote = '')

# kaggle
kaggle <- read.csv("dane/netflix_titles.csv",encoding= "UTF-8")

# kolorki
netflix_colors <- c("#0973e6","#e5b209",'#e50915','#073b4c','#2abd77',"#edb879","#e07b39","#80391e","#cce7e8","#b97455")

netflix_theme <- theme(
    plot.background = element_rect(fill = "#141414", colour = NA),
    panel.background = element_rect(fill = "#141414", colour = NA),
    axis.text = element_text(colour = "linen"),
    axis.title = element_text(colour = "linen"),
    plot.title = element_text(colour = "linen", hjust=0, size=15),
    plot.subtitle = element_text(colour = "linen", hjust=0),
    panel.grid = element_line(color = '#2B2B2B'),
    legend.background = element_rect(fill = "#141414", colour = "linen", size =1),
    legend.text = element_text(colour = "linen"),
    legend.title = element_text(colour = "linen"),
    legend.key = element_rect(fill = "#141414", colour = NA)
)

#konta
accounts = c("Adam", "Agata", "Wojtek")

chosenProfile <-  "XXX"


customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#ffffff"
  ,primaryFontColor = "blue"
  ,infoFontColor = "white"
  ,successFontColor = "white"
  ,warningFontColor = "white"
  ,dangerFontColor = "white"
  ,bodyBackColor = "#141414"
  
  ### header
  ,logoBackColor = "#141414"
  
  ,headerButtonBackColor = "#141414"
  ,headerButtonIconColor = "white"
  ,headerButtonBackColorHover = "white"
  ,headerButtonIconColorHover = "#141414"
  
  ,headerBackColor = "#141414"
  ,headerBoxShadowColor = "#141414"
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#141414"
    ,colorMiddle = "#141414"
    ,colorEnd = "#141414"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "1px 0px 0px"
  ,sidebarShadowColor = "white"
  
  ,sidebarUserTextColor = "blue"
  
  ,sidebarSearchBackColor = "white"
  ,sidebarSearchIconColor = "white"
  ,sidebarSearchBorderColor = "white"
  
  ,sidebarTabTextColor = "blue"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "white"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "white"
    ,colorMiddle = "white"
    ,colorEnd = "white"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "white"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "#141414"
    ,colorMiddle = "#151515"
    ,colorEnd = "#161616"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "green"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "white"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "white"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "white"
  ,boxTitleSize = 16
  ,boxDefaultColor = "white"
  ,boxPrimaryColor = "white"
  ,boxInfoColor = "white"
  ,boxSuccessColor = "white"
  ,boxWarningColor = "white"
  ,boxDangerColor = "white"
  
  ,tabBoxTabColor = "white"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "blue"
  ,tabBoxTabTextColorSelected = "white"
  ,tabBoxBackColor = "white"
  ,tabBoxHighlightColor = "white"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "white"
  ,buttonTextColor = "blue"
  ,buttonBorderColor = "white"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "white"
  ,buttonTextColorHover = "white"
  ,buttonBorderColorHover = "white"
  
  ,textboxBackColor = "#161616"
  ,textboxBorderColor = "white"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "white"
  ,textboxBorderColorSelect = "white"
  

  
  ### tables
  ,tableBackColor = "white"
  ,tableBorderColor = "white"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


#_______________________________________________________________________________

localMaxima <- function(x) {
    
    y <- diff(c(-.Machine$integer.max, x)) > 0L
    rle(y)$lengths
    y <- cumsum(rle(y)$lengths)
    y <- y[seq.int(1L, length(y), 2L)]
    if (x[[1]] == x[[2]]) {
        y <- y[-1]
    }
    y
}

closest <-function(d,loc.max){
    #znajduje wartości w d$x najbliższe wartościom z loc.max
    v=c()
    for(i in c(1:length(loc.max))){
        v=append(v,d$y[which.min(abs(d$x-loc.max[i]))])
        
    }
    v
}

chooseAccountViewingActivity <- function(input){
    if (input == "Adam"){
        return(AdamViewingActivity)
    }
        
    if (input == "Agata"){
        return(AgataViewingActivity)
    }
    #add Wojtek
    if (input == "Wojtek"){
      return(WojtekViewingActivity)
    }
}


chooseProfile <- function(input){
    if (input == "Adam"){
        return ("Adam")
    }
    
    if (input == "Agata"){
        return("Pasożyt")
    }
    #add Wojtek
    if (input == "Wojtek"){
      return("Wojtek")
    }
}


dailyActivityDataFrameGeneratorFunction <- function(chosenAccountInput){
    
    # Activity in 2021
    if ( options()$stringsAsFactors )
        options(stringsAsFactors=FALSE)
    
  
    print ("_____ProfileName_____ dailyActivityDFGenerator")
    profile = chooseProfile(chosenAccountInput)
    print(profile)
    
    print ("_____ViewingActivity_____ dailyActivityDFGenerator")
    ViewingActivity <- chooseAccountViewingActivity(chosenAccountInput)
    print(head(ViewingActivity))

    print(paste(chosenAccountInput, "- daily activity dataframe generator debug (chosenAccountInput)"))
    
    df <- ViewingActivity %>%
        filter(Profile.Name==profile) %>% # dodałam filtrowanie po profilu
        mutate(MonthDay = format(as.POSIXct(Start.Time), format = "%Y-%m-%d")) %>%
        mutate(Year = format(as.POSIXct(Start.Time), format = "%Y")) %>%
        mutate(Hour = format(as.POSIXct(Start.Time), format = "%H:%M:%S")) %>%
        mutate(Hourfrac = sapply(strsplit(Hour,":"),
                                 function(x) {
                                     x <- as.numeric(x)
                                     x[1]+x[2]/60
                                 })
        )
    
   
    df <- df %>% 
        mutate(Hourfrac = as.double(Hourfrac))
     
    #str(df)
    
    return(df)
}


denistyGeneratorFunction <- function(dfInput){
    print (paste(length(dfInput), "- density debug"))
    print (dfInput)
    print (dfInput$Hourfrac)
    d <- density(x = dfInput$Hourfrac, bw = "nrd", adjust=0.7)
    return(d)
}



mostWatchedShowDataFrameGeneratorFunction <- function(chosenAccountInput){
    
    
    profile = chooseProfile(chosenAccountInput)
    ViewingActivity <- chooseAccountViewingActivity(chosenAccountInput)
    
    df <- ViewingActivity %>%
        filter(Profile.Name==profile) %>%
        mutate(Year = format(as.POSIXct(Start.Time), format = "%Y"))%>%
        mutate(Minutes = format(Duration, format = "%H:%M:%S")) %>% 
        mutate(Minutesfrac = sapply(strsplit(Minutes,":"),
                                    function(x) {
                                        x <- as.numeric(x)
                                        60*x[1]+x[2]+x[3]/60
                                    })
        )
    
    
    # df <- df %>%
    #     filter(Year==2021 & Supplemental.Video.Type=="") %>%
    #     mutate(TitleShort = sub("\\:.*", "", Title)) %>%
    #     group_by(Profile.Name,TitleShort) %>%
    #     summarise(suma = sum(Minutesfrac)) 
    
    return (df)
}




    
ui <- dashboardPage(
    
    # Application title
    dashboardHeader(
        #title= customLogo
        title = tags$img(
          src='https://images.ctfassets.net/4cd45et68cgf/7LrExJ6PAj6MSIPkDyCO86/542b1dfabbf3959908f69be546879952/Netflix-Brand-Logo.png?w=684&h=456', 
          height = '70', 
          width ='70')
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                selectInput(
                    inputId = "ProfileSelector",
                    label = "Choose Profile: ", 
                    choices = accounts,
                    selected = "Agata"
                )
            ),
            
            menuItem(
              selectInput(
                "year",
                "Select Year: ",
                c(2021,2020,"All time"))
            ),
            
            
              setSliderColor("#db0000",1),
              sliderInput("shows",
                          "Choose the number of TV Shows: ",
                          min = 1,
                          max = 10,
                          value = 5,
                          pre = "TV Shows: ")
            
            
            
        )
        
        
        
    ),
        
        
    
    
    dashboardBody(
        
         customTheme,
            
           
            
  
        fluidRow(
         
          column(
            shiny::markdown("###  Netflix daily activity \n #### by hour"),
            withSpinner(
              plotOutput("dailyActivityPlotOutput"),
              type = 4, color = "white"
            ),
            offset = 0.3,
            width = 12
          )
          
        
            
        ),
        
        
        
        
        fluidRow(
          
          column(
            shiny::markdown("###  Your Top favourite shows \n #### determined by minutes watched"),
            withSpinner(
              plotOutput("mostWatchedShowPlotOutput"),
              type = 4, color = "white"
            ), 
            offset = 0.3,
            width = 12
          )
          
          
          
        ),
        
        fluidRow(
          
          column(
            shiny::markdown("###  Your Top 3 movies or episodes \n #### determined by minutes watched"),
            withSpinner(
              plotOutput("podiumPlotOutput"),
              type = 4, color = "white"
            ) , 
            offset = 0.3,
            width = 12
          )
          
          
          
        ),
        
        
        fluidRow(
          
          column(
            shiny::markdown("###  Your monthly activity \n #### measured by watched minutes"),
            withSpinner(
              plotlyOutput("monthlyActivityPlotOutput"),
              type = 4, color = "white"
            )  , 
            offset = 0.3,
            width = 12
          )
          
          
          
        ),
        
        
        
    )
    
)


dailyActivityPlotGeneratorFunction <- function(dailyActivity, loc.max, d)
{
    dailyActivityPlot <- dailyActivity %>%
        ggplot(aes(x=Hourfrac))+
        netflix_theme+
        theme(panel.grid.minor = element_blank())+
        geom_density(colour = netflix_colors[1], size =2)+
        geom_vline(xintercept=loc.max, col="#e50914")+
        annotate("text",
                 x=loc.max,
                 #labels zagmatwane, bo zmienia godzinę ułamek na format godzinowy
                 label=sapply(as.character(loc.max),
                              function(x){a <- strsplit(x,"\\.")
                              sapply(a, 
                                     function(y){paste(y[1],
                                                       (round(as.numeric(paste(0,y[2], sep="."))*6/10, digits = 2)*100)%%100, 
                                                       sep=":")})
                              }),
                 # wyświetlamy labels przy maximach
                 y=closest(d,loc.max),
                 colour="#e50914",
                 vjust=-0.5, 
                 hjust=0.4) +
        labs(
             x = "Hour",
             # zmieniłam density na frequency bo jakoś bardziej mi pasuje do godzin, 
             # ale dziwnie trochę częstość jako ułamek
             y="Frequency")+
        scale_x_continuous(breaks = seq(0,24,2))
    
}




mostWatchedShowPlotGeneratorFunction <- function(df,nr){
    mostWatchedShow <- df %>% 
        arrange(-suma) %>%
        top_n(nr)
    
    
    mostWatchedShowPlot <- mostWatchedShow %>% 
        ggplot(aes(x=suma,y=reorder(TitleShort,suma),fill=reorder(TitleShort,-suma)))+
        netflix_theme+
        scale_fill_manual(values = rep("#2abd77", 10))+
        scale_x_continuous(expand = expansion(mult = c(0, .05)))+
        geom_col(show.legend = FALSE)+
        geom_text(aes(label =round(suma, digits=0)),
                  colour="linen", 
                  angle=-90,
                  vjust = -0.5)+
        labs(
             x = "minutes watched",
             y="")
    
    return(mostWatchedShowPlot)
}

podiumPlotGeneratorFunction <- function(chosenAccountInput){
    
    profile = chooseProfile(chosenAccountInput)
    ViewingActivity <- chooseAccountViewingActivity(chosenAccountInput)
    
    if ( options()$stringsAsFactors )
        options(stringsAsFactors=FALSE)
    
    if ( options()$stringsAsFactors )
      options(stringsAsFactors=FALSE)
    
    df <- ViewingActivity %>%
      filter(Profile.Name==profile) %>%
      mutate(Year = format(as.POSIXct(Start.Time), format = "%Y"))%>%
      mutate(Minutes = format(Duration, format = "%H:%M:%S")) %>%
      mutate(Minutesfrac = sapply(strsplit(Minutes,":"),
                                  function(x) {
                                    x <- as.numeric(x)
                                    60*x[1]+x[2]+x[3]/60
                                  }))
    return (df)
}


monthlyActivityPlotFunction <- function(df){
    
  monthActivity <-df %>% 
    add_row(month=c(1:12)[!(c(1:12) %in% df$month)], minPerMonth=0) %>% 
    arrange(month)
  
  monthActivity$y =rep(c(1,2,3,4),each=3)
  monthActivity <- monthActivity%>% 
    mutate(colors = replace(rep(netflix_colors[1:4],each=3),minPerMonth==max(minPerMonth),"#D4B972"))
  
  TotalTimeSpend <- df %>% 
    summarise(totalTime = seconds_to_period(sum(minPerMonth*60)))
  
  monthlyActivityPlot <- monthActivity %>% ggplot(aes(x=month, y =y, text = paste(
    "Minutes watched in ", month.name[monthActivity$month]," :\n",minPerMonth
  )))+
    netflix_theme+
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y= element_blank(),
          legend.position = "none" #c(0.88,0.2)
    )+
    labs(
         x="",
         y="")+
    geom_point(aes(size=minPerMonth, fill=format(y),color=format(month)), shape=21, stroke = 1.5)+
    guides(fill = "none", color = "none",size=guide_legend(title="minutes",override.aes=list(colour="linen")))+
    scale_fill_manual(values = alpha(netflix_colors,0.6))+
    scale_color_manual(values = monthActivity$colors)+
    scale_y_continuous(breaks=seq(0,4,1))+
    scale_size(range = c(0, 10))+
    scale_x_continuous(breaks = seq(1,12,1), labels = month.abb[monthActivity$month])+
    coord_cartesian(ylim=c(0.8,4.2), xlim=c(1,15), clip = "off")+
    annotate("text",
             x = 14,y = 4.1,
             label = paste("Total time spend on Netflix:","\n" ,TotalTimeSpend$totalTime[1]),
             hjust=0,
             color= netflix_colors[5])+
    annotate("text",
             x =14 ,y = 3, 
             label = "This color shows \n the most active \n month",
             hjust=0,
             colour = "#D4B972")
    # 
     return (monthlyActivityPlot)
}





server <- function(input, output) {
    
    output$dailyActivityPlotOutput <- renderPlot({
        
        print("_______Profile Name - density_______")
        print(input$ProfileSelector)
        
        
        df <- dailyActivityDataFrameGeneratorFunction(input$ProfileSelector)
        
        if (input$year != "All time"){
        df <- df %>% filter(Year==input$year)
        }
        
        print("_______TABLE_______")
        print (head(df, 4))
        
        
        print(paste(input$ProfileSelector, "- input Account debug"))
        d <- denistyGeneratorFunction(df)
        
        loc.max <- d$x[localMaxima(d$y)]
        
        dailyActivity <- df
        
        dailyActivityPlot <- dailyActivityPlotGeneratorFunction(dailyActivity, loc.max, d)
        
        dailyActivityPlot
        
     
        
        
    })
    
    
    
    if ( options()$stringsAsFactors )
        options(stringsAsFactors=FALSE)
    
    output$mostWatchedShowPlotOutput <- renderPlot({
        df1 <- mostWatchedShowDataFrameGeneratorFunction(chosenAccountInput = input$ProfileSelector)
        
        if (input$year != "All time"){
          df1 <- df1 %>% filter(Year==input$year)
        }
        
        df1 <- df1 %>%
          filter(Supplemental.Video.Type=="") %>%
          mutate(TitleShort = sub("\\:.*", "", Title)) %>%
          group_by(Profile.Name,TitleShort) %>%
          summarise(suma = sum(Minutesfrac)) 
        mostWatchedShowPlot <- mostWatchedShowPlotGeneratorFunction(df1,input$shows)
        
       mostWatchedShowPlot
        
        
    })
    
    
    output$podiumPlotOutput <- renderPlot({
        
        chosenProfile <- input$ProfileSelector
        
        
        df <- podiumPlotGeneratorFunction(chosenProfile)
        
        if (input$year != "All time"){
          df <- df %>% filter(Year==input$year)
        }
        
        
        
        mostWatchedMovie_frameB <- df %>%
          filter(Supplemental.Video.Type=="") %>%
          group_by(Profile.Name,Title) %>%
          summarise(suma = sum(Minutesfrac))%>%
          arrange(-suma) %>%
          top_n(3) %>% 
          mutate(order= c(2,1,3)) %>% 
          mutate(place = c(100,75,60))
        mostWatchedMovie <- mostWatchedMovie_frameB %>%
          mutate(Title = sapply(Title,
                                function(x) {
                                  paste(strwrap(x, width=30),  collapse="\n")}))
        
        regularPodiumPlot <- mostWatchedMovie %>%
          ggplot(aes(y=place,x=reorder(Title,order),fill=reorder(Title,-order)))+
          netflix_theme+
          theme(axis.text.y =element_blank(),
                axis.text.x =element_blank(),
                axis.ticks.y =element_blank(),
                axis.ticks.x=element_blank(),
                panel.grid = element_blank(),
                plot.title = element_text( size=30))+
          scale_fill_manual(values = netflix_colors)+
          scale_y_continuous(expand = expansion(mult = c(0.3, 0.5)))+
          geom_col(show.legend = FALSE, width = 1)+
          geom_text(aes(label =reorder(Title,order)),
                    colour="linen",
                    vjust=-0.1,
                    size =8)+
          geom_text(aes(label =format(round(suma, digits=0))),
                    colour="linen",
                    position = position_stack(vjust = .5),
                    size=15)+
          labs(
               y = "",
               x="")
        
        regularPodiumPlot
        
    
    })
    
    
    output$monthlyActivityPlotOutput <- renderPlotly({
        chosenProfile <- input$ProfileSelector
        
        profile = chooseProfile(chosenProfile)
        ViewingActivity <- chooseAccountViewingActivity(chosenProfile)
        
        df <- ViewingActivity %>%
          filter(Profile.Name==profile) %>% 
          mutate(year = year(Start.Time)) 
        
        if (input$year != "All time"){
          df <- df %>% filter(year==input$year)
        }
        
          df <- df %>% 
          mutate(month = month(Start.Time)) %>%
          mutate(minutes =hour(hms(Duration))*60+ minute(hms(Duration))+second(hms(Duration))/60) %>% 
          group_by(month) %>%
          summarise(minPerMonth=sum(minutes))
        
        
        monthlyActivityPlot <- monthlyActivityPlotFunction(df)
        ggplotly( monthlyActivityPlot, tooltip = "text")
    })
        
    
}


# Run the application 

shinyApp(ui = ui, server = server)
