library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

df <- read.csv('complete.csv')

server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        df1 <- df %>%
            mutate(age = awardYear - as.numeric(substr(birth_date, 1, 4))) %>%
            filter(!is.na(age)) %>%
            filter(category == input$category)
        
        if (!input$male) {
            df1 <- df1 %>%
                filter(gender != "male")
        }
        
        if (!input$female) {
            df1 <- df1 %>%
                filter(gender != "female")
        }
        
        ggplot(df1, aes(x = age)) + 
            geom_histogram(color="black", fill="orange", bins = 19) + 
            theme_classic() + 
            scale_y_continuous(expand = c(0,0)) + 
            scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
            labs(title = "Age of Nobel Prizes winners at the time of receival") +
            theme(plot.title = element_text(size=18))
    })
    
    
    output$plot2 <- renderPlot({
        df2 <- df %>%
            filter(birth_countryNow != "", category == input$category2) %>%
            group_by(birth_countryNow) %>%
            summarise(n = n()) %>%
            arrange(-n) %>%
            mutate(birth_countryNow = forcats::fct_reorder(birth_countryNow, n)) %>%
            head(input$number)

        ggplot(df2, aes(x = n, y = birth_countryNow)) +
            geom_col(color = 'black', fill = 'orange') +
            scale_x_continuous(expand = c(0,0)) +
            theme_classic() +
            labs(title = 'Birth countries of Nobel Prizes winners',
                 y = "",
                 x = "count")+
            theme(plot.title = element_text(size=18))
    })
}

ui1 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("category",
                        "Category",
                        sort(unique(df$category))),
            checkboxInput("male",
                          "Male",
                          TRUE),
            checkboxInput("female",
                          "Female",
                          TRUE)

        ),
        mainPanel(
           plotOutput("plot1")
        )
    ),
    theme = shinytheme("simplex")
)

ui2 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("category2",
                        "Category",
                        sort(unique(df$category))),
            selectInput("number",
                        "How many countries to show?",
                        1:20,
                        selected = 10)
        ),
        mainPanel(
            plotOutput("plot2")
        )
    ),
    theme = shinytheme("simplex")
)

app_ui <- navbarPage(
    title = "Nobel Prizes analysis",
    tabPanel(title = "Chart 1: Age",
             ui1),
    tabPanel(title = "Chart 2: Countries",
             ui2)
)


shinyApp(ui = app_ui, 
         server = server)

