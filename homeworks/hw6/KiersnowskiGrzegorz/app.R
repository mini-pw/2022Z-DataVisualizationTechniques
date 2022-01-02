#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(shinyWidgets)
df<- data.frame(read.csv('complete.csv'))

# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundColor(rgb(0.65, 1, 0.4)),

    # Application title
    titlePanel("Noble awards data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          tags$style(".well {background-color:#FF33A6;}"),
          selectInput("Kategoria", "Wybierz Kategorie", unique(df$category), 'Physics'),
          
          checkboxInput("liniaTrendu", "Linia trendu", FALSE),
          
          h1("Ciekawe co robia te slidery...."),
          sliderInput("R", "number 1", 0,1,0.2),
          sliderInput("G", "number 2", 0,1,0.2),
          sliderInput("B", "number 3", 0,1,0.2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("YearAmmount")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$distPlot <- renderPlot({
        p<-df[df$category==input$Kategoria,] %>% 
        ggplot(aes(x = prizeAmount))+
        geom_histogram(fill = rgb(input$R, input$G, input$B))+
        scale_x_continuous(labels = comma)+
        ggtitle("Histogram UwU")+
          theme(panel.background = element_rect(fill = rgb(1-input$R, 1-input$G, 1-input$B),
                                                colour = "lightblue"))
  
        p
    })
    output$YearAmmount <- renderPlot({
      p<- df[df$category==input$Kategoria,] %>% 
        ggplot(aes(y = prizeAmount, x = awardYear))+
        geom_point(color = rgb(input$G, input$B, input$R))+
        scale_y_continuous(labels = comma)+
        ggtitle("Fajnie ci naukowcy zarabiaja")+
        theme(panel.background = element_rect(fill = rgb(1-input$G, 1-input$B, 1-input$R),
                                              colour = "lightblue"))
      
      if (input$liniaTrendu) {
        p <- p + geom_smooth(se = FALSE, color = rgb(input$B, input$R, input$G))
      }
      p
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
