library(shiny)
library(dplyr)
library(plotly)

df <- read.csv('data/data.csv') %>% filter(birth_continent != '')

shinyUI(fillPage(
  
  title = "Nobel prizes dashboard",
  lang = "en",

  tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
            visibility: hidden !important;
    }'))),

  #Title
  fluidRow(titlePanel(h1("Noble prizes",
                         align = 'center',
                         style = "margin-bottom: 25px"))),

  #Slider
  fluidRow(div(sliderInput("decade",
                           "Decade:",
                           min = 1910,
                           step  = 10,
                           width = "60%",
                           animate = T,
                           max = 2010,
                           value = 1990,
                           sep = ''),
               align = 'center',
               style = "font-size: 20px")),
  
  #plots
  fluidRow(
      splitLayout(
           plotlyOutput("plot2"),
           column(12, 
               plotlyOutput("plot1"),
               div(
                 align = "center",
                 checkboxGroupInput(
                   "checkbox",
                   "Continents",
                   unique(df$birth_continent),
                   selected = c("Europe", "Asia", "North America"),
                   inline = TRUE
                 )
               )
               ),
           
          )
      )
  
))
