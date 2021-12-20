#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(shinycssloaders)

df2 <- read.csv("complete.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Nobel Prize Data"),

    # Sidebar with a slider input for number of bins
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

        # Show a plot of the generated distribution
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
