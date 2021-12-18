library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)

server <- shinyServer(function(input, output) {
  starwars_sub <- starwars %>% 
    mutate(species_new = case_when(species == "Human" ~ "Human",
                                   species == "Droid" ~ "Droid",
                                   TRUE ~ "Other")) 
  
  output$graph <- renderPlot({
    
    starwars_subset <- starwars_sub[starwars_sub$species_new %in% input$species,]
    
    ggplot(
      starwars_subset, 
      aes(x = height, y = mass)
    ) + 
      geom_point(aes(color = species_new)) + 
      geom_label_repel(
        data = starwars_subset[starwars_subset$name == input$character,],
        aes(label = name),
        min.segment.length = 0,
      ) +
      theme_bw() + 
      labs(title = "Relation of height and mass of starwars characters" , x = "Height", y = "Mass", color = "Species")
  }, width = 600, height = 600)
  
})

ui <- shinyUI(fluidPage(
    titlePanel("Starwars character"),
    sidebarLayout(
      sidebarPanel(
        selectInput("character",
                    "Select character:",
                    unique(starwars$name)),
        checkboxGroupInput("species", 
                           "Select species:",
                           c("Human" = "Human",
                             "Droid" = "Droid",
                             "Other" = "Other"))
        # selected = c("Human", "Droid", "Other"))
      ),
      mainPanel(
        plotOutput("graph")
      )
    )
))

app <- shinyApp(ui, server)

app