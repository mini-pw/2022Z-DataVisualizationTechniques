library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinythemes)
library(rsconnect)
library(plotly)


df <- read.csv('data/complete.csv')




server <- function(input, output, session) {
  df <- read.csv('data/complete.csv')
  df <- df[,c("awardYear", "category", "prizeAmount", "prizeAmountAdjusted", 
              "birth_date", "birth_countryNow", "birth_continent")]
  df <- filter(df, birth_date != "")
  df$birth_date <- substring(df$birth_date, first=1, last=4)
  df$birth_date <- df$awardYear- as.integer(df$birth_date)
  output$plot1 <- renderPlotly({
    p <- ggplot(data=df[df$category %in% input$category1,] %>% 
                  group_by(birth_continent) %>% 
                  summarise(n = n()), 
                aes(x=birth_continent, y=n))+
      labs(x = "Continent", y = "Number of Noblist",
           title = "Number of Noblist per Continent")+
      theme_gray()+
      geom_bar(stat="identity",fill = "#FFFFFF")+
      theme(panel.background = element_rect(fill = '#303030', colour = '#303030'))
    
    
    ggplotly(p)
  })
  
  output$plot2 <- renderPlotly({
    p <- ggplot(data=df[df$category %in% input$category2,], 
                aes(x=awardYear, y=birth_date,color = category))+
      labs(x = "Year", y = "Age",
           title = "Age of nobel laureates")+
      theme_gray()+
      geom_point()+
      xlim(input$year1)+
      theme(panel.background = element_rect(fill = '#303030', colour = '#303030'))
    
    
    ggplotly(p)
  })
  
  
  output$plot3 <- renderPlotly({
    p <- ggplot(data=df[df$category %in% input$category3,])+
      labs(x = "Year", y = "Prize",
           title = "The amount of the nobel prize")+
      theme_gray()
    if (input$adjust == "prizeAmountAdjusted"){
      p = p + geom_point(aes(x=awardYear, 
                             y=prizeAmountAdjusted, 
                             color = category))+
        theme(panel.background = element_rect(fill = '#303030', colour = '#303030'))+
        xlim(input$year2)
    } else {
      p = p + geom_point(aes(x=awardYear, 
                             y=prizeAmount, 
                             color = category))+
        theme(panel.background = element_rect(fill = '#303030', colour = '#303030'))+
        xlim(input$year2)
    }
    
    ggplotly(p)
  })
}