library(shiny)
library(wordcloud2)
library(shinycssloaders)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library("DT")


kotletzdupy <- shinyDashboardThemeDIY(
  appFontFamily = "Arial",
  appFontColor = "#e4e6eb",
  ,primaryFontColor = "#e4e6eb"
  ,infoFontColor = "#e4e6eb"
  ,successFontColor = "#e4e6eb"
  ,warningFontColor = "#e4e6eb"
  ,dangerFontColor = "#e4e6eb"
  ,bodyBackColor = "#1c1e21"
  
  ### header
  ,logoBackColor = "#1c1e21"
  
  ,headerButtonBackColor = "#1c1e21"
  ,headerButtonIconColor = "#e4e6eb"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "#1c1e21"
  ,headerBoxShadowColor = "#e4e6eb"
  ,headerBoxShadowSize = "1px 1px 1px"
  
  ### sidebar
  ,sidebarBackColor = "#1c1e21"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "1px 1px 1px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "#e4e6eb"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "#e4e6eb"
  
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  
  ,sidebarTabBackColorHover = "#e4e6eb"
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
  
  
  
  
)


znajomi <-fluidPage(
    fluidRow(
      titlePanel("Roczne przyrosty znajomych"),
          plotly::plotlyOutput("znajomi")
    )
)

wiadomosci <- fluidPage(
    
    splitLayout(
        column(12,
        sidebarLayout(
            sidebarPanel(
                tags$style(".panel {background-color: #00000000; 
                                      border-width: 0px;}"),
                class = 'panel',
                radioButtons(
                    "lenDaysOrHours",
                    "Podziel na: ",
                    choiceNames = c("Dni tygodnia", "Godziny"),
                    choiceValues = c("dt", 'h')
                )
            ),
            mainPanel(
                plotly::plotlyOutput("wiadLen")
            )
        )),
        column(12, 
        sidebarLayout(
            sidebarPanel(
                tags$style(".panel {background-color: #00000000; 
                                      border-width: 0px;}"),
                class = 'panel',
                radioButtons(
                    "countDaysOrHours",
                    "Podziel na: ",
                    choiceNames = c("Dni tygodnia", "Godziny"),
                    choiceValues = c("dt", 'h'),
                    selected = 'dt'
                )
            ),
            mainPanel(
                plotly::plotlyOutput("wiadCount")
            )
        ))
    ),
    
    fluidRow(
        
        titlePanel("Chmura słów z wiadomości na Messengerze"),
        
        sidebarLayout(
            sidebarPanel(
                tags$style(".panel {background-color: #00000000; 
                                      border-width: 0px;}"),
                class = 'panel',
                setSliderColor(c("#358AF8","#358AF8","#358AF8"),c(1,2,3)),
                
                sliderInput("year",
                            "Rok: ",
                            min = 2015,
                            max = 2021,
                            value = 2021,
                            sep = ''),
                sliderInput("word_count",
                            "Maksymalna liczba słów:",
                            min = 50,
                            value = 200,
                            max = 250,
                            step = 10),
                sliderInput("min_freq",
                            "Minimalna liczba wystąpień słowa:",
                            min = 1,
                            max = 10,
                            value = 5)
            ),
            
            mainPanel(
                wordcloud2Output("cloud")
            )
        )
    )
)

reakcje <- fluidPage(
  fluidRow(
    titlePanel("Wystąpienia poszczególnych reakcji"),
      plotly::plotlyOutput("plot2")
  )
)

zainteresowania <- fluidPage(
    
    fluidRow(
        titlePanel("Nasze zainteresowania wywnioskowane przez Facebooka"),
        dataTableOutput("topicsTable")
    ),
    fluidRow(
      tags$style(".panel {background-color: #00000000; 
                                    border-width: 0px;}"),
      class = 'panel',
      setSliderColor("#358AF8", 1),
      sliderInput("topicsCount",
                  "Ile tematow: ",
                  min = 1,
                  max = 10,
                  value = 5,
                  sep = '',
                  width = '100%')
    )
)

shinyUI(
  dashboardPage(
    dashboardHeader(title = span("Facebook",style = "color:#e4e6eb")),

    
    dashboardSidebar(
      sidebarMenu(
        id = 'tabs',
        menuItem("Znajomi", tabName = 'znajomi', icon = icon("users")),
        menuItem("Wiadomosci", tabName = 'wiadomosci', icon = icon("comment-dots")),
        menuItem("Reakcje", tabName = 'reakcje', icon = icon("heart")),
        menuItem("Zainteresowania", tabName = 'zainteresowania', icon = icon("table-tennis")),
        tags$style(".item {color:black}"),
        menuItem(selectInput("user",
                             "Wybierz osobę: ",
                             c("Grzegorz Zbrzeżny", 
                               "Antoni Zajko", 
                               "Mikołaj Roguski"),
                             selected = 'Antoni Zajko')
        )
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = 'znajomi', znajomi),
        tabItem(tabName = 'wiadomosci', wiadomosci),
        tabItem(tabName = 'reakcje', reakcje),
        tabItem(tabName = 'zainteresowania', zainteresowania)
      )
    ),
    
   kotletzdupy
  )
)