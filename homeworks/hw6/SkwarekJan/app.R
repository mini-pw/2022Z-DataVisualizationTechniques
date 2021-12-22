library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)

df <- read.csv(file = "complete.csv")

server <- shinyServer(function(input, output) {
  df_sub1 <- df %>% 
    mutate(gender_new = case_when(gender == "male" ~ "Male",
                                   gender == "female" ~ "Female",
                                   TRUE ~ "Other")) 
  df_sub2 <- df %>% 
    mutate(birth_continent_new = case_when(birth_continent == "North America" ~ "North America",
                                  birth_continent == "Europe" ~ "Europe",
                                  TRUE ~ "Other")) 
  
  output$graph1 <- renderPlot({
    
    df_subset1 <- df_sub1[df_sub1$gender_new %in% input$gender,]
    
    ggplot(
      df_subset1, 
      aes(x = awardYear, y = prizeAmount)
    ) + 
      geom_point(aes(color = gender_new)) + 
      geom_label_repel(
        data = df_subset1[df_subset1$name == input$character1,],
        aes(label = name),
        min.segment.length = 0,
      ) +
      theme_bw() + 
      labs(title = "Relation of award year and prize amount of Noble winners" , x = "Award year", y = "Prize amount", color = "Gender")
  }, width = 600, height = 600)
  
  output$graph2 <- renderPlot({
    
    df_subset2 <- df_sub2[df_sub2$birth_continent_new %in% input$birth_continent,]
    
    ggplot(
      df_subset2, 
      aes(x = awardYear, y = prizeAmountAdjusted)
    ) + 
      geom_point(aes(color = birth_continent_new)) + 
      geom_label_repel(
        data = df_subset2[df_subset2$name == input$character2,],
        aes(label = name),
        min.segment.length = 0,
      ) +
      theme_bw() + 
      labs(title = "Relation of award year and adjusted prize amount of Noble winners" , x = "Award year", y = "Adjusted prize amount", color = "Birth continent")
  }, width = 600, height = 600)
  
})

ui1 <- shinyUI(fluidPage(
  titlePanel("Noblist character"),
  sidebarLayout(
    sidebarPanel(
      selectInput("character1",
                  "Select character:",
                  unique(df$name)),
      checkboxGroupInput("gender", 
                         "Select gender:",
                         c("Male" = "Male",
                           "Female" = "Female",
                           "Other" = "Other"))
      
    ),
    mainPanel(
      plotOutput("graph1")
    )
  )
))

ui2 <- shinyUI(fluidPage(
  titlePanel("Noblist character"),
  sidebarLayout(
    sidebarPanel(
      selectInput("character2",
                  "Select character:",
                  unique(df$name)),
      checkboxGroupInput("birth_continent", 
                         "Select gender:",
                         c("North America" = "North America",
                           "Europe" = "Europe",
                           "Other" = "Other"))
      
    ),
    mainPanel(
      plotOutput("graph2")
    )
  )
))


app_ui <- navbarPage(
  title = "Nobel Prizes analysis",
  tabPanel(title = "Chart 1: gender",
           ui1),
  tabPanel(title = "Chart 2: birth continent",
           ui2)
)


app <- shinyApp(app_ui, server)

app

