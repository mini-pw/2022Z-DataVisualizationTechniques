library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
frame <- read.csv("complete.csv")
df <- read.csv("complete.csv")
server <- function(input, output,session) {
    
    
    
    df$birth_date <- as.Date(df$birth_date, format = "%Y-%m-%d")
    df <- df%>%
        mutate(birth = as.numeric(format(birth_date, format = "%Y")))%>%
        drop_na(birth, awardYear)%>%
        mutate(age = awardYear - birth)
    
    
    output$plot <- renderPlot({
        df1 <- df[df$category %in% input$category,]%>%
            group_by(awardYear)%>%
            mutate(mean = mean(age))
        
        
        ggplot(df1, aes(x= awardYear, y = mean))+
            geom_line(color="#f8766d")+
            geom_point(color="#f8766d")+
            labs(title = "Mean age of laureates while receiving awards ",
                 x = "Award year", y = "Age", color = "category")+
            xlim(input$slider)+
            theme(text = element_text(size = 15))+
            theme(plot.title = element_text(size = 20))
    })
    
    output$plot2 <- renderPlot({
        
        df2 <- frame[frame$birth_continent %in% input$continent,]%>%
            group_by(category, gender)%>%
            summarize(number = n())%>%
            ungroup()
        
        ggplot(df2,aes(x = category, y = number, fill = factor(gender, levels=c("male","female"))))+
            geom_bar(stat='identity')+
            scale_fill_discrete(name = "Laureates", labels = c("male", "female" ))+
            labs(title = "Nobel Laureates")+
            theme(axis.text.x = element_text(angle = 30, size = 11, hjust=0.7))+
            theme(text = element_text(size = 15))+
            theme(plot.title = element_text(size = 20))
    })
}

ui <- shinyUI(fluidPage(
    titlePanel("Nobel Laureates Age"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider", label = "Select range of years", min = 1900, 
                        max = 2020, value = c(1900, 2020)),
            checkboxGroupInput("category", 
                               "Select category:",
                               c("Peace" = "Peace",
                                 "Literature" = "Literature",
                                 "Physics" = "Physics",
                                 "Economic Sciences" = "Economic Sciences",
                                 "Physiology or Medicine"= "Physiology or Medicine",
                                 "Chemistry" = "Chemistry"), 
                               selected = "Peace")),
        
        mainPanel(
            plotOutput("plot"))
    )))


ui2 <- shinyUI(fluidPage(
    titlePanel("Nobel laureates"),
    sidebarLayout(
        sidebarPanel(
            selectInput("continent",
                        "Select continent:",
                        unique(frame$birth_continent))
        ),
        mainPanel(
            plotOutput("plot2")
        )
    )
))    

app_ui <- navbarPage(
    title = "Nobel Prize",
    tabPanel("Nobel Laureates Age", ui),
    tabPanel("Nobel laureates", ui2, icon = icon("database")))

shinyApp(app_ui, server)
