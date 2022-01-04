
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(tidyr)
library(dashboardthemes)



ui <- dashboardPage(

    dashboardHeader(
        title = "Nobel Price Laureates"
    ),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Line plot",tabName = "tab1",icon = icon("chart-line"), startExpanded = T,stayExpanded = T,
                 selectInput("select",
                             "By:",
                             choices = list("Country" = "country","Continent" = "continent","Category" = "category")),
                 sliderInput("range", "Range:",
                             min = 1901, max = 2019,
                             sep = "",
                             step = 1,
                             value = c(1901,  2019))
                 )
      ),
      sidebarMenu( #robie dwa sidaberMenu zamiast dwoch menuItem, zeby mozna bylo miec rozwiniete je oba na raz
        menuItem("Map plot",tabName = "tab2",icon = icon("globe-americas"), startExpanded = T,
                 radioButtons("by2", "By:", choices = list("Gender:" = "gender","Category" = "category" )),
                 checkboxGroupInput("next_box", "Gender:",
                                    choices = list("Male" = "male","Female" = "female"),selected = "male"),
                 radioButtons("map_type", "Map type:", choices = list("Natural" = "natural earth",
                                                                      "Globe" = "orthographic" ))
        )
      )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark" #problem jest z tym, ze slabe sa kolory w selectInput, jest to napisane tez na githubie dashboardsthemes
      ),
      fluidRow(
        textOutput("text1"),
        tags$head(tags$style("#text1{color: white;
                                 font-size: 30px;
                                 text-align: center;
                                 }"
        )),
        plotlyOutput("dispPlotly1",height = "600px")
      ),
      hr(),
      fluidRow(

        textOutput("text2"),
        tags$head(tags$style("#text2{color: white;
                                 font-size: 30px;
                                 text-align: center;
                                 }"
        )),
        plotlyOutput("dispPlotly2",
                     width = "auto",
                     height = "600px")
      ) 
    )
)