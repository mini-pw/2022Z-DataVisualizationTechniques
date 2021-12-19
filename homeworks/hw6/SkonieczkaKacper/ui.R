#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(PogromcyDanych)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "Noblowski analizator"
    ),
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        menuItem(text = "Analiza wieku", tabName = "A_wieku", icon = icon("user-clock")),
        menuItem(text ="Analiza kontynentów", tabName = "A_kontynentow", icon= icon("globe-europe"))
      )
    ),
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "A_wieku", fluidRow(plotlyOutput("distPlot"),
                                                column(3,
                                                     selectInput('listaRozwijana',
                                                                 "Wybierz dziedzinę nagrody Nobla:",
                                                                  choices = c("Chemia",
                                                                              "Ekonomia",
                                                                              "Fizjologia lub medycyna",
                                                                              "Fizyka",
                                                                              "Literatura",
                                                                              "Literatura",
                                                                              "Pokojowa"),
                                                                 selected = c("C"))
                                               
                                    
                                               
        )
      )),
      
      tabItem(tabName = "A_kontynentow",  fluidRow(plotlyOutput("distPlot2"),
              column(3, 
                                                checkboxGroupInput("checkGroup", 
                                                                   h3("Wybierz kontynenty:"), 
                                                                   choices = list("Europa" , 
                                                                                  "Ameryka Północna", 
                                                                                  "Ameryka Południowa",
                                                                                  "Azja",
                                                                                  "Australia i Oceania",
                                                                                  "Afryka"),
                                                                   selected = "Europa")))
              )
      )

   
    )
  )

)
