#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$distPlot <- renderPlotly({
    
    
    data <- read.csv("complete.csv")
    
    data %>% 
      mutate(Wiek =as.numeric(awardYear) - as.numeric(substring(birth_date,1, 4))) %>% 
      select(Wiek, gender, category, awardYear) %>% 
      filter(!is.na(Wiek)) ->
      data
    
    data[data$category == "Chemistry",]$category <- "Chemia"
    data[data$category == "Economic Sciences",]$category <- "Ekonomia"
    data[data$category == "Literature",]$category <- "Literatura"
    data[data$category == "Peace",]$category <- "Pokojowa"
    data[data$category == "Physics",]$category <- "Fizyka"
    data[data$category == "Physiology or Medicine",]$category <- "Fizjologia lub medycyna"
    
    
    femaleData <- filter(data, gender == "female")
    maleData <- filter(data, gender == "male")
    
    
    m <- list(
      t = 80,
      pad = 4
    )
    
    femaleData <- filter(femaleData, category == input$listaRozwijana)
    maleData <- filter(maleData, category == input$listaRozwijana)
    
    
    plot_ly() %>% 
      add_trace(data = maleData,
                type = "histogram",
                x = ~Wiek,
                name = "mężczyźni",
                nbinsx = 20) %>% 
      add_trace(data = femaleData,
                type = "histogram",
                x = ~Wiek,
                name = "kobiety",
                nbinsx = 20) %>% 
      layout(
        barmode="stack",
        bargap=0.1,
        yaxis = list(title = "Liczba nagrodzonych osób", range = list(0,41)),
        xaxis = list(range = list(0,100),  dtick = 5),
        title = list(text ="Liczba osób, które otrzymały nagrodę Nobla będąc w danym wieku", 
                     y = 0.97,
                     font = list(size = 25)),
        legend=list(title=list(text='<b> Płeć </b>')),
        margin = m)
  })
  
  
  output$distPlot2 <- renderPlotly({
    
    
    data2 <- read.csv("complete.csv")
    
    data2 %>% 
      select(category, birth_continent) ->
      data2
    
    data2[data2$category == "Chemistry",]$category <- "Chemia"
    data2[data2$category == "Economic Sciences",]$category <- "Ekonomia"
    data2[data2$category == "Literature",]$category <- "Literatura"
    data2[data2$category == "Peace",]$category <- "Pokojowa"
    data2[data2$category == "Physics",]$category <- "Fizyka"
    data2[data2$category == "Physiology or Medicine",]$category <- "Fizjologia lub medycyna"
    
    data2[data2$birth_continent == "Europe",]$birth_continent <- "Europa"
    data2[data2$birth_continent == "Oceania",]$birth_continent <- "Australia i Oceania"
    data2[data2$birth_continent == "North America",]$birth_continent <- "Ameryka Północna"
    data2[data2$birth_continent == "South America",]$birth_continent <- "Ameryka Południowa"
    data2[data2$birth_continent == "Asia",]$birth_continent <- "Azja"
    data2[data2$birth_continent == "Africa",]$birth_continent <- "Afryka"
    
    m <- list(
      t = 80,
      pad = 4
    )
    
    data2 <- filter(data2, birth_continent %in% input$checkGroup) 
    
    dataAdd <- data.frame(category = c("Chemia",
                                        "Ekonomia",
                                        "Literatura",
                                        "Pokojowa",
                                        "Fizyka",
                                        "Fizjologia lub medycyna"),
                                         liczba = 0)

    
    data2 <- data2 %>% 
      group_by(category) %>% 
      summarise(liczba = n()) 
    
    data2 <- rbind(data2,dataAdd)
    
    data2 <- data2 %>% 
      group_by(category) %>% 
      summarise(liczba = sum(liczba)) 
    

    

    
    plot_ly() %>% 
      add_trace(data = data2,
                type = "bar",
                x = ~category,
                y = ~liczba) %>% 
      layout(
        yaxis = list(title = "Liczba nagrodzonych osób",  range = list(0,250)),
        xaxis = list(title = "Kategorie"),
        title = list(text ="Liczba osób z wybranych kontynentów, które otrzymały nagrodę Nobla", 
                     y = 0.97,
                     font = list(size = 25)),
        
        margin = m)

  })

  
  
  
  outputOptions(output, "distPlot", suspendWhenHidden = FALSE)
  
  observe({
    updateTabItems(session, "tabs", selected = "report_1")
  })
})
