library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(shinycssloaders)

df2 <- read.csv("complete.csv")


shinyUI(fluidPage(

    
    titlePanel("Nobel Prize Data"),

    
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Plot 1: Period length (in years):",
                        min = 1,
                        max = 120,
                        value = 15),
            
            selectInput("category",
                        "Plot 2: Choose category:",
                        choices = c("Economic Sciences", "Physics", "Chemistry", "Peace", "Physiology or Medicine", "Literature", "All categories")
            )
        ),

        
        mainPanel(
            
            shiny::markdown("### Plot1: Contriubution of Men and Women \n #### in Nobel Prizes in given timeframe"),
            withSpinner(
                plotly::plotlyOutput("distPlot"),
                type = 4, color = "black"
            ),
            shiny::markdown("\n### Plot2: Prize amout distribution \n #### in chosen category per continent; adjusted for inflation"),
            withSpinner(
                plotly::plotlyOutput("distPlot2"),
                type = 4, color = "black"
                
            )
        )
    )
))
