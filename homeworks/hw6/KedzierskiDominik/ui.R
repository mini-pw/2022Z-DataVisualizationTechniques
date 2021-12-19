library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinythemes)
library(rsconnect)
library(plotly)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Continent"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "category1",
        label = "Category:",
        choices = c("Physics", "Chemistry", "Economic Sciences",
                    "Peace", "Physiology or Medicine", "Literature"),
        selected = "Physics"
      )
    ),
    mainPanel(
      plotlyOutput("plot1"),
    )
  )
)


ui2 <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Age"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "category2",
        label = "Category:",
        choices = c("Physics", "Chemistry", "Economic Sciences",
                    "Peace", "Physiology or Medicine", "Literature"),
        selected = "Physics"
      ),
      sliderInput("year1", label = h3("Year"), min = 1900, 
                  max = 2019, value = c(1900, 2019))
    ),
    mainPanel(
      plotlyOutput("plot2"),
    )
  )
)


ui3 <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Prize"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "category3",
        label = "Category:",
        choices = c("Physics", "Chemistry", "Economic Sciences",
                    "Peace", "Physiology or Medicine", "Literature"),
        selected = "Physics"
      ),
      radioButtons("adjust", label = h3("Adjusted"),
                   choices = list("Yes" = "prizeAmountAdjusted", "No" = "prizeAmount"), 
                   selected = "prizeAmountAdjusted"),
      sliderInput("year2", label = h3("Year"), min = 1900, 
                  max = 2019, value = c(1900, 2019))
    ),
    mainPanel(
      plotlyOutput("plot3"),
    )
  )
)




app_ui <- navbarPage(
  title = "Nobel Prize People",
  tabPanel("Continent", ui, icon = icon("map")),
  tabPanel("Age", ui2, icon = icon("users")),
  tabPanel("Prize", ui3, icon = icon("coins")),
  theme = bslib::bs_theme(bootswatch = "cyborg"),
)