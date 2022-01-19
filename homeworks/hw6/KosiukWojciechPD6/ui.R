#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinycssloaders)

if ( options()$stringsAsFactors )
    options(stringsAsFactors=FALSE)
my_data <- read.csv("complete.csv")


shinyUI(dashboardPage(
    dashboardHeader(title = "Noblists' Data"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Nobel prizes by country", tabName = "menu1", icon = icon("medal")),
            menuItem("Words Appearance", tabName = "menu2", icon = icon("sort-amount-up")),
            menuItem("Age of Nobel Winners",tabName = "menu3",icon = icon("child"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "menu1", 
                    fluidPage(
                    h2("Number of Nobel Prize winners by their birth country"),
                    fluidRow(
                        column(11,align="center",
                               wellPanel(
                                   selectInput("country", "Select country: ", arrange(count(group_by(my_data,birth_country)),-n)[-6,]$birth_country),
                                   checkboxInput("showNoblists", "Show Noblists", value = TRUE)
                               ) 
                            )
                        ),
                    fluidRow(
                        box(status="warning", align="center",
                            width=11, shinycssloaders::withSpinner(plotlyOutput("distPlot"),
                                                                   type=5,color="orange"))
                    ),
                    fluidRow(
                        box( status = "warning", color="orange",
                        width=11,
                        h3("Noblists' List"),
                        dataTableOutput("table1"))
                    )
                )
            ),
            
            
            tabItem(tabName = "menu2",
                    fluidPage(
                    h2("Words appearance in Motivation for Nobel Prize"),
                    fluidRow(
                        column(11, align="center",
                               wellPanel(
                                   sliderInput("bins",
                                               "Minimum word length:",
                                               min = 1,
                                               max = 20,
                                               value = 3,
                                               pre = "Word length: ")        
                               )
                        )
                    ),
                    fluidRow(
                        box(status="warning",
                            width=11,shinycssloaders::withSpinner(plotlyOutput("distPlot2"),
                                                                  type=5,color="orange"))
                        )
                    )
                ),
            
            tabItem(tabName = "menu3",
                    fluidPage(
                    h3("Age of Nobel Prize Winners by category"),
                    
                    fluidRow(
                        box(status="warning",
                            width=11,shinycssloaders::withSpinner(plotlyOutput("distPlot3"),
                                                                  color="orange"))
                    ),
                    fluidRow(
                            column(width=11,
                            wellPanel(
                            h3("Select categories: "),
                            checkboxInput("ch1", "Economic Sciences", value = TRUE),
                            checkboxInput("ch2", "Physics", value = TRUE),
                            checkboxInput("ch3", "Chemistry", value = F),
                            checkboxInput("ch4", "Peace", value = F),
                            checkboxInput("ch5", "Physiology or Medicine", value = F),
                            checkboxInput("ch6", "Literature", value = F)
                            )
                        )
                    )
                )
            )
        )
    )
                    
))
