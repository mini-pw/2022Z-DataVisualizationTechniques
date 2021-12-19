library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)

df <- read.csv('complete.csv')

df1 <- df %>% 
    select(category, birth_date, dateAwarded) %>% 
    filter(birth_date != "", dateAwarded != "") %>% 
    mutate(age = as.numeric(as.Date(as.character(dateAwarded), format="%Y-%m-%d") 
                            - as.Date(as.character(birth_date), format="%Y-%m-%d"))%/%365) %>% 
    filter(!is.na(age))

df2 <- df %>% select('awardYear', 'category', 'gender') %>% 
    filter(awardYear %in% 1990:2018) %>%
    filter(gender != "") %>% 
    arrange(awardYear)

df2 <- df2 %>% group_by(gender) %>% 
    group_by(category, awardYear, gender) %>% 
    summarise(nn = n())

df2 <- df2 %>% mutate(n_male = ifelse(gender == 'male', nn, 0), 
                      n_female = ifelse(gender == 'female', nn, 0)) %>% 
    select(-gender, -nn)


ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "category", label = "Select category", choices = unique(df$category)),
            markdown("## Age means of laureates"),
            shiny::tableOutput('ageTable'),
            width = 3
            
        ),
        
        mainPanel(
            plotlyOutput("p1"),
            width = 9
        )
    )
)

ui2 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            shinyWidgets::sliderTextInput('rok',
                                          'Select year',
                                          choices = unique(df2$awardYear))
        ),
        
        mainPanel(
            plotlyOutput("p2")
        )
    )
    
    
)


app_ui <- navbarPage(
    
    title = "Data analysis of dataset regarding Noble Prize laureates",
    tabPanel("Analysis by age", ui),
    tabPanel("Analysis by sex", ui2),
    theme = bslib::bs_theme(bootswatch = "yeti"),
    footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Bujakowski Hubert & Jakubowski Wiktor © 2021 Copyright
                </p>
                </footer>
                "))



server <- function(input, output) {
    
    output$p1 <- renderPlotly({
        
        p1 <- df1 %>% filter(category == input$category) %>% 
            plot_ly(x = ~age, name = "Age", type='histogram', nbinsx = 40) %>% 
            layout(title = "Age distribution of Noble Prize laureates at the time of awarding by category",
                   showlegend = FALSE, 
                   xaxis = list(title = 'Age when awarded'),
                   yaxis = list(title = 'Number of awarded'))
        p1 <- config(p1, staticPlot = TRUE)
        
    })
    output$ageTable <- shiny::renderTable({table <- df1 %>% 
        group_by(category) %>% 
        mutate(mean = mean(age)) %>% 
        select(category, mean) %>% 
        unique() %>% 
        arrange(mean)
    table
    })
    
    
    output$p2 <- renderPlotly({
        p2 <- df2 %>% filter(awardYear == input$rok) %>% 
            plot_ly(
                x = ~category,
                y = ~n_male,
                name = 'male',
                type = 'bar',
                marker = list(color = "#3376FF")
            ) %>% add_trace(
                y = ~n_female,
                name = 'female',
                marker = list(color = "#FF69B4")
            ) %>% layout(
                legend = list(title = list(text = "<b> Gender </b>")),
                barmode = 'group',
                title = "Number of laureates awarded in 1990-2018 by category",
                yaxis = list(title = "Number of laureates",
                             range = list(0, 4)),
                
                xaxis = list(title = "Category")
            )
        p2 <- config(p2, staticPlot = TRUE)
        p2
        
    })
}

shinyApp(app_ui, server)

