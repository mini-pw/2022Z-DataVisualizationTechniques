library(shiny)

library(ggplot2)
library(dplyr)

df <- readr::read_csv("complete.csv")

## Server
server <- function(input, output) {
  # Plot 1 - liczba nagród Nobla po kraju urodzenia laureatów
  
  output$plot1 <- renderPlot({
    tmp <- df %>%
      group_by(birth_country, category) %>%
      count() %>%
      filter(birth_country == input$country)
    
    ggplot(tmp, aes(x = category, y = n)) +
      geom_col() +
      scale_y_continuous(breaks = seq(0, max(tmp$n), 1),
                         expand = c(0, 0)) +
      labs(
        x = "Category",
        y = "Number",
        title = paste(
          "Number of Nobel prize awards by category of laureates born in",
          input$country
        )
      ) +
      theme(text = element_text(size = 18))
  })
  
  # Plot 2 - liczba nagród Nobla przyznanych między określnymi latami
  
  output$plot2 <- renderPlot({
    tmp <- df %>%
      filter(awardYear >= input$year[1] &
               awardYear <= input$year[2]) %>%
      group_by(category) %>%
      count()
    
    
    ggplot(tmp, aes(x = category, y = n)) +
      geom_col() +
      labs(
        x = "Category",
        y = "Number",
        title = paste(
          "Number of Nobel prize awards by category between",
          input$year[1],
          "and",
          input$year[2]
        )
      ) +
      theme(text = element_text(size = 18)) +
      scale_y_continuous(expand = c(0, 0))
  })
  
}

## Zakładka 1
ui1 <- fluidPage(sidebarLayout(
  sidebarPanel(selectInput(
    "country", "Country",
    sort(unique(df$birth_country)),
    "Poland"
  ),
  width = 3),
  
  mainPanel(plotOutput("plot1"),
            width = 9),
  fluid = FALSE,
))

## Zakładka 2
ui2 <- fluidPage(fluidRow(mainPanel(plotOutput("plot2"),
                                    width = '90%')),
                 
                 fluidRow(
                   sliderInput(
                     "year",
                     label = h3("Select years"),
                     min = min(df$awardYear),
                     max = max(df$awardYear),
                     value = c(min(df$awardYear), max(df$awardYear)),
                     step = 1,
                     sep = NULL,
                     width = '100%'
                   )
                 ))

##
app_ui <-
  navbarPage(title = "HW6 - number of Nobel prize awards by:",
             tabPanel("Birth country", ui1),
             tabPanel("Year", ui2), )

# Run the application
shinyApp(ui = app_ui, server = server)
