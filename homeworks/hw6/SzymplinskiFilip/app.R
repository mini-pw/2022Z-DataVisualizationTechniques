# Necessary libraries
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
require(maps)


nobel_df <- read.csv("complete.csv") # Data set about Nobel Prize laureates

# First page
ui1 <- fluidPage(
  
  titlePanel("Age of Nobel Prize laureates at the time of winning the award"),
  fluidRow(
    column(width = 3,
           selectInput("continentInput",
                  "Select continent of birth",
                  choices = c("All",
                              unique(as.character((nobel_df %>% filter(birth_continent != ""))$birth_continent))) ))
  ),
  fluidRow(
    column(width = 7, offset = 2,
           plotOutput("plot1_boxplot"),
           sliderInput(inputId = "years_interval",
                       label = "Range of years",
                       min = min(nobel_df$awardYear),
                       max = max(nobel_df$awardYear),
                       value = c(min(nobel_df$awardYear), max(nobel_df$awardYear)), sep = "",
                       width = "100%"
           ))
))


# Second page
ui2 <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  titlePanel("No. of Nobel Prize laureates in each European country by age"),
  fluidRow(
    column(width = 3,
           selectInput("ageInput",
                       "Select age interval",
                       choices = c("All", "10-19", "20-29", "30-39",
                                   "40-49", "50-59", "60-69", "70-79",
                                   "80-89", "90-99") ))
  ),
  fluidRow(
    column(width = 9, offset = 3,
           plotOutput("plot2_map",
                      800, 600))
))


final_ui <- navbarPage(
  h3("Simple analyze of the Nobel Prize Dataset"),
    tabPanel("First Plot", ui1),
    tabPanel("Second Plot", ui2)
)


server <- function(input, output) {
  
  output$plot1_boxplot <- renderPlot({
    
    nobel_df_boxplot <- if(input$continentInput != "All"){
      nobel_df %>% filter(birth_continent == input$continentInput)
    } else {
      nobel_df
    }
    
    nobel_df_boxplot <- nobel_df_boxplot %>%
      filter(awardYear >= input$years_interval[1] &
               awardYear <= input$years_interval[2]) %>% 
      mutate(age = as.numeric(as.Date(as.character(dateAwarded), format="%Y-%m-%d") 
             - as.Date(as.character(birth_date), format="%Y-%m-%d"))%/%365) %>% 
      filter(!is.na(age))
    
    ggplot(nobel_df_boxplot, aes(x = category, y = age)) +
      geom_boxplot(
        color = "#004b6f",
        outlier.color = "#b70000",
        outlier.shape = 1
      ) +
      scale_y_continuous(breaks = seq(20, 100, by = 20), limits = c(20, 100)) +
      theme_bw() +  
      labs(x = "Category of the Nobel Prize",
           y = "Age")+
      theme(
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted"),
        panel.background = element_rect(color = "gray", fill = "#fafafa"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
  })
  
  
  output$plot2_map <- renderPlot({
    
    countries <- unique(as.character((nobel_df %>% filter(birth_continent == "Europe"))$birth_countryNow))
    countries <- countries[countries != "Russia"]
    countries[countries == "Norway"] <- "Norway(?!:Svalbard)"
    countries[countries == "United Kingdom"] <- "UK"
    europe_map <- map_data("world", region = countries)
    
    nobel_df_map <- nobel_df %>% 
      filter(birth_continent == "Europe") %>% 
      mutate(age = as.numeric(as.Date(as.character(dateAwarded), format="%Y-%m-%d") 
      - as.Date(as.character(birth_date), format="%Y-%m-%d"))%/%365) %>% 
      mutate(age_interval = (age + 10)-(age%%10))
    
    nobel_df_map <- if(input$ageInput == "All"){
      nobel_df_map
    } else if(input$ageInput == "10-19"){
      nobel_df_map %>% filter(age_interval == 20)
    } else if(input$ageInput == "20-29"){
      nobel_df_map %>% filter(age_interval == 30)
    } else if(input$ageInput == "30-39"){
      nobel_df_map %>% filter(age_interval == 40)
    } else if(input$ageInput == "40-49"){
      nobel_df_map %>% filter(age_interval == 50)
    } else if(input$ageInput == "50-59"){
      nobel_df_map %>% filter(age_interval == 60)
    } else if(input$ageInput == "60-69"){
      nobel_df_map %>% filter(age_interval == 70)
    } else if(input$ageInput == "70-79"){
      nobel_df_map %>% filter(age_interval == 80)
    } else if(input$ageInput == "80-89"){
      nobel_df_map %>% filter(age_interval == 90)
    } else{
      nobel_df_map %>% filter(age_interval == 100)
    }
    
    nobel_df_map <- nobel_df_map %>% 
      group_by(birth_countryNow) %>% 
      summarise(count_laureates = n()) %>% 
      ungroup()
    
    levels(nobel_df_map$birth_countryNow)[78] <- "UK"
    
      europe_map %>% 
      left_join(nobel_df_map, by = c("region" = "birth_countryNow")) %>% 
      ggplot(aes(long, lat)) +
      geom_polygon(aes(group = group, fill = count_laureates)) +
      scale_fill_gradient2(low = "#00acff", high = "#004b6f", na.value = "#adadad") +
      borders(region = countries, size = 0.15, colour = "black") +
      theme_bw() + 
      theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
      ) +
      labs(
        title = "",
        fill = "",
        x = "",
        y = ""
      )

    
  })
  
}

app <- shinyApp(final_ui, server)