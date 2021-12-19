library(ggplot2)
library(dplyr)
library(stringi)
library(shiny)

nobel <- read.csv("complete.csv")



# Define UI for application that draws a histogram
ui1 <- fluidPage(

    # Application title
    titlePanel("At what age Nobel Prize winners receive their prize?"),

    # Sidebar with a slider input for number of bins 

    sidebarPanel(
        actionButton("box", "Boxplot"),
        actionButton("violin", "Violin plot"),
        selectInput("continent", "Continent: ", c("All continents", unique(nobel$birth_continent)))
    ),
    mainPanel(
        plotOutput("distPlot1")
    )
)

ui2 <- fluidPage(
    
    # Application title
    titlePanel("Which continent has the most Nobel prizes"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Years: ",
                        min = min(nobel$awardYear),
                        max = max(nobel$awardYear),
                        value = c(min(nobel$awardYear), max(nobel$awardYear))),
            checkboxInput("chemistry", "Chemistry", TRUE),
            checkboxInput("economy", "Economic Sciences", TRUE),
            checkboxInput("literature", "Literature", TRUE),
            checkboxInput("peace", "Peace", TRUE),
            checkboxInput("physic", "Physics", TRUE),
            checkboxInput("medicine", "Physiology or Medinice", TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    aaa <-  reactiveValues(data = NULL)
    
    observeEvent(input$box, {
        aaa$data <- geom_boxplot()
    })
    
    observeEvent(input$violin, {
        aaa$data <- geom_violin()
    })
    
    colors <- data.frame(continent = c("Africa", "Asia", "Europe", 
                                       "North America", "Oceania", "South America"),
                         color = c("#F8766D", "#B79F00" , "#00BA38", "#00e699", 
                                   "#619CFF", "#F564E3"))
    
    output$distPlot1 <- renderPlot({
        tmp <- nobel %>% 
            select(category, birth_continent, awardYear, birth_date) %>% 
            filter(birth_continent != "")
        tmp$birth_date <- stri_extract_first_regex(tmp$birth_date, '[0-9]+')
        tmp$birth_date <- as.integer(tmp$birth_date)
        
        color <- colors$color
        if(input$continent != "All continents"){
            tmp <- tmp %>% 
                filter(birth_continent == input$continent)
            color <- colors %>% filter(continent == input$continent)
            color <- color$color
        }
        
        plot1 <- tmp %>% 
            mutate(age = awardYear - birth_date) %>% 
            group_by(birth_continent) %>% 
            ggplot(aes(x = category, y = age, fill = birth_continent)) 
        
        if(!is.null(color)){
            plot1 <- plot1 + scale_fill_manual(values = color)
        }
        
        if(!is.null(aaa$data)){
            plot1 <- plot1 + aaa$data
        }
        
        plot1 + 
            labs(title = "Distribution of Nobel Prize winners age",
                 subtitle = paste("For", input$continent),
                 fill = "Legend") +
            xlab("Category") +
            ylab("Winner's age")
        
    })
    
    output$distPlot2 <- renderPlot({
        col <- data.frame(color = colors$color, 
                          category = c("Chemistry", "Economic Sciences", "Literature", 
                                       "Peace", "Physics", "Physiology or Medicine"))
        tmp <- nobel %>% 
            filter(awardYear >= input$years[1], awardYear <= input$years[2])  %>% 
            filter(birth_continent != "")
        if(!input$chemistry){
            tmp <- tmp %>%  filter(category != "Chemistry")
            col <- col %>%  filter(category != "Chemistry")
        }
        if(!input$economy){
            tmp <- tmp %>%  filter(category != "Economic Sciences")
            col <- col %>%  filter(category != "Economic Sciences")
        }
        if(!input$medicine){
            tmp <- tmp %>%  filter(category != "Physiology or Medicine")
            col <- col %>%  filter(category != "Physiology or Medicine")
        }
        if(!input$literature){
            tmp <- tmp %>%  filter(category != "Literature")
            col <- col %>%  filter(category != "Literature")
        }
        if(!input$physic){
            tmp <- tmp %>%  filter(category != "Physics")
            col <- col %>%  filter(category != "Physics")
        }
        if(!input$peace){
            tmp <- tmp %>%  filter(category != "Peace")
            col <- col %>%  filter(category != "Peace")
        }
        tmp %>% 
            group_by(category) %>% 
            ggplot(aes(x = birth_continent, fill = category)) +
            geom_bar() +
            scale_fill_manual(values = col$color) + 
            labs(title = "Number of Nobel prizes won", 
                 subtitle = paste("Between years", input$years[1], "and", input$years[2]),
                 fill = "Legend") +
            xlab("Birth continent") + 
            ylab("Number of prizes")
    })
}

ui <- navbarPage(
    title = "Data analysis: complete",
    tabPanel("Plot 1", ui1),
    tabPanel("Plot 2", ui2))

# Run the application 
shinyApp(ui = ui, server = server)

