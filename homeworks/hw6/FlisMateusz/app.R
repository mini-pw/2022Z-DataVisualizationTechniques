library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)


data <- read.csv("complete.csv")


ui <- fluidPage(

    titlePanel("Nobel Prize Winners And Their Adjusted Prizes"),

    sidebarLayout(
        
        sidebarPanel(
            
            selectInput("category",
                        "Choose category:",
                        choices = sort(unique(data$category)),
                        selected = "Chemistry"),
            
            sliderInput("years",
                        "Choose years",
                        min = 1901,
                        max = 2019,
                        value = c(1901,2019)),
            
            checkboxInput("smooth",
                        "Show trend line",
                        value = FALSE)
        ),
        
        mainPanel(
           
           plotly::plotlyOutput("distPlot")
           
        )
    )
)

####################################

ui2 <- fluidPage(
    
    titlePanel("Nobel Prize Winners Devided by Countries of their Birth"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("country",
                        "Choose a country:",
                        choices = sort(unique(data$birth_country)),
                        selected = "Poland")
        ),
        
        mainPanel(
            plotly::plotlyOutput("distPlot2")
        )
    )
    
    
)

####################################

server <- function(input, output) {
  
    
    options(scipen=10000)
    

    output$distPlot <- plotly::renderPlotly({
        
        data1 <- data %>%
            filter(awardYear > input$years[1] & awardYear < input$years[2]) %>% 
            filter(gender!="") %>% 
            filter(category==input$category) 
        
        
        p <- data1 %>% 
            ggplot(aes(x = awardYear, y = prizeAmountAdjusted)) +
            geom_point(aes(color = gender, alpha = 0.5), position=position_jitter(h = 2, w = 0.5))+
            labs(color = "Gender",
                 alpha = "",
                 y = "",
                 x = "Year") +
            ylim(0,13000000) +
            theme_bw()
        
        if (input$smooth) {
            p <- p + geom_smooth(se = FALSE)
        }
        
        plotly::ggplotly(p)
        
    })
    
    
    output$distPlot2 <- plotly::renderPlotly({
        
        data2 <- data %>% 
            filter(birth_country==input$country) 
        
        p2 <- data2 %>% 
            ggplot(aes(y = category, color = gender, x = awardYear)) +
            geom_point(position=position_jitter(h = 0.1)) +
            theme_bw() +
            theme(panel.grid = element_blank()) +
            labs(y = "",
                 x = "Year",
                 color = "Gender")
        
        plotly::ggplotly(p2)
    })
    
    
}

######################################

app_ui <- navbarPage(
    title = "Data analysis: Nobel Prize Winners",
    tabPanel("Prize Received", ui, icon = icon("money")),
    tabPanel("Country Representation", ui2, icon = icon("flag")),
    theme = bslib::bs_theme(bootswatch = "cosmo")
)


shinyApp(ui = app_ui, server = server)
