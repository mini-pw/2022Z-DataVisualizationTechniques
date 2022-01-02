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
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(stringr)
# Define UI for application that draws a histogram

data<- read.csv('complete.csv');

ui <- fluidPage(

    # Application title
    titlePanel("Nobel prize payouts"),

    # Sidebar with a slider input for number of bins 
    
    
    
    sidebarLayout(
        sidebarPanel(
            inputPanel(
               selectInput(
                   inputId = "Categories",
                   label = "Choose categories to sum:",
                   choices = unique(data$category),
                   selected = "Physics",
                   multiple = T
               ),
               checkboxInput(
                   inputId = "divide",
                   label = "Distingiush categories",
                   value = F
               ),
               sliderInput(
                   inputId = "years",
                   label = "Choose years of interest",
                   min = 1900,
                   max = 2020,
                   value = c(1900,2020),
                   sep = ""
               )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           withSpinner(plotlyOutput("payoutByYear"))
        )
    )
)

ui2 <- fluidPage(
    titlePanel("Names and surnames of laureates"),
    sidebarLayout(
        sidebarPanel(
            inputPanel(
                selectInput(
                    inputId = "nos",
                    label = "Name or surname",
                    choices = c("First name","Last name"),
                    selected = "First name"
                ),
                sliderInput(
                    inputId = "skipnum",
                    label = "Skip counts lower than:",
                    min = 0,
                    max = 5,
                    step = 1,
                    value = 0
                ),
                checkboxInput(
                    inputId = "skipspace",
                    label = "Skip empty",
                    value = T
                )
            )
        )
    ,
    mainPanel(
        withSpinner(plotlyOutput("histogramByNameOrSurname"))
    ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$histogramByNameOrSurname <- renderPlotly({
        dataSplitNames <- data %>% mutate(firstName = str_extract(fullName, '^\\w*(\\.)?(?=\\W)'), lastName = str_extract(fullName,'(?<=\\W)\\w*$')) %>% select(firstName,lastName);
        if (input$skipspace == T){
            dataSplitNames <- dataSplitNames %>% filter(lastName != "");
        }
        if (input$nos == "First name"){
        p <- plot_ly(dataSplitNames  %>% count(firstName, name = "count") %>% filter(count >= input$skipnum ),
                    x = ~firstName,
                    y = ~count,
                    type = "bar"
                    )
        }
        else {
            p <- plot_ly(dataSplitNames  %>% count(lastName, name = "count") %>% filter(count >= input$skipnum),
                         x = ~lastName,
                         y = ~count,
                         type = "bar"
            )
        }
        p <- p %>% layout(title = paste("number of nobel laureates by their ",input$nos),
                          xaxis = list(title = input$nos),
                          yaxis = list(title = "count"))
                        
    }
        
    )

    output$payoutByYear <- renderPlotly({
        # generate bins based on input$bins from ui.R
        selectCategory <- function(category){
            for (cat1 in input$Categories){
                if (cat1 == category){
                    TRUE
                }
                
            }
            FALSE
                
            
        }
        
        if (input$divide){
            dataByYearByCategory <- data %>%
                group_by(awardYear,category) %>%
                summarize(sum = sum(prizeAmountAdjusted)) %>%
                filter(category %in% input$Categories);
            p <- plot_ly(dataByYearByCategory,
                         x = ~awardYear,
                         y = ~sum,
                         color = ~category,
                         type = "bar") %>% 
                layout(barmode = "relative")
            
        }else{
        dataByYearByCategory <- data %>%
            group_by(awardYear,category) %>%
            summarize(sum = sum(prizeAmountAdjusted)) %>%
            filter(category %in% input$Categories) %>%
            ungroup() %>%
            group_by(awardYear) %>%
            summarise(sum = sum(sum));
        
        p <- plot_ly(dataByYearByCategory,
                x = ~awardYear,
                y = ~sum,
                type = "bar")
        }
        p<- p %>%
            layout(
                title = "Summaric payout by year of :\n",
                xaxis = list(range = input$years,
                             title="Year"),
                yaxis = list(title="Summaric payout")
            )
    })
}
add_ui <- navbarPage(
    title = "Analysis of nobel prize cash prizes",
    tabPanel("Summaric payout of categories",ui),
    tabPanel("Names and surnames of laureates",ui2),
    theme = bslib::bs_theme(bootswatch = "cosmo")
)
# Run the application 
shinyApp(add_ui,  server)
