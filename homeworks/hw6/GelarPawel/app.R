library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(patchwork)

df <- read.csv("data/complete.csv") %>% filter(ind_or_org == "Individual") %>%  mutate(age = awardYear - as.numeric(substr(birth_date,1,4)))
ids <- df$id
ui <- fluidPage(

    titlePanel("Individual Nobel Prize laureates"),

    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("gender",
                        "Choose gender:",
                        c("male", "female"),
                        selected = c("male", "female")),
            checkboxGroupInput("category",
                               "Choose categories:",
                               c("Physiology or Medicine" ,"Physics", "Peace", "Literature", "Economic Sciences",  "Chemistry"),
                               selected = c("Economic Sciences", "Physics", "Chemistry", "Peace", "Physiology or Medicine", "Literature")),
            sliderInput(inputId = "date", 
                        label = "choose date range", 
                        min = 1901, 
                        max = 2019, 
                        value = c(1901, 2019),
                        step = 1,
                        ticks = F,
                        sep = ""),
            textOutput("number")
        ),

        mainPanel(
           girafeOutput("girafePlot"),
           h5("You can select one laureate, to view detailed information about his award(s), or more, to view summary"),
           actionButton("reset", label = "Reset selection"),
           actionButton("select_all", label = "Select all"),
           htmlOutput("info"),
           plotOutput("infoPlot")
        )
    )
)



server <- shinyServer(function(input, output, session) {
    output$number <- renderText({
       number <- df %>% filter(gender %in% input$gender, between(awardYear, input$date[1], input$date[2]), category %in% input$category) %>% count() %>% pull
       paste(number, "matching entries")
    })
    output$info <- renderUI({
      if(length(selected_ids()) == 1)
      {
        choosen <- df %>% filter(id %in% selected_ids())
        text <- paste(choosen$name[1], "<br>")
        awards <- paste( 
                        choosen$prizeStatus, 
                        choosen$categoryFullName, 
                        "in", choosen$awardYear, choosen$motivation, 
                        "at the age of", choosen$age, 
                        collapse = "<br>")
        HTML(paste(text, awards))
      }
    })
    
    output$infoPlot <- renderPlot({
      if (length(selected_ids()) >1)
      {
        filtered <- df %>% filter(id %in% selected_ids(), gender %in% input$gender, between(awardYear, input$date[1], input$date[2]), category %in% input$category) %>% distinct(id, .keep_all = T)
        ageplot <- filtered %>% ggplot(aes(y = age)) + geom_boxplot() + 
          scale_x_continuous(NULL, NULL, NULL) + scale_y_continuous(name = "Age", limits = c(0,100))
        genderplot <- filtered %>% ggplot(aes(x = gender, fill = gender)) + geom_bar() + 
          scale_fill_manual(values = c("male" = "brown", "female" = "pink"), guide = NULL) + scale_x_discrete(name = NULL) + scale_y_continuous(name = "Count")
        continentplot <- filtered %>% ggplot(aes(x = birth_continent, fill = birth_continent)) + geom_bar() + 
          scale_x_discrete(name = "Birth continent") + scale_y_continuous(name = "Count") +
          scale_fill_manual(values = c("Africa" = "#FF0000", "Asia" = "#FFDB00", "Europe" = "#49FF00", "North America" = "#00FF92", "Oceania" = "#0092FF", "South America" = "#FF00DB"), guide = NULL)
        (ageplot + genderplot + continentplot) & theme_minimal() & plot_annotation(subtitle = "Distribiutions of ages, genders and continents of origin of selected Noble prize laureates")
      }
    })
    observeEvent(input$reset, {
      session$sendCustomMessage(type = 'girafePlot_set', message = character(0))
    })
    observeEvent(input$select_all, {
      session$sendCustomMessage(type = 'girafePlot_set', message = ids)
    })
    
    output$girafePlot <- renderGirafe({
        
        g_plot <- df %>% filter(gender %in% input$gender, between(awardYear, input$date[1], input$date[2]), category %in% input$category) %>% 
        ggplot(aes(x = awardYear, y = category, color = birth_continent, tooltip = name, data_id = id)) + 
        geom_point_interactive(position = position_dodge(width = 0.5)) +
          scale_x_continuous(name = "Year of reciving the award" ,breaks = seq(1900,2020, 10), minor_breaks = 1901:2019) +
          scale_y_discrete(name = "Category") +
          scale_color_manual(name = "Birth continent", values = c("Africa" = "#FF0000", "Asia" = "#FFDB00", "Europe" = "#49FF00", "North America" = "#00FF92", "Oceania" = "#0092FF", "South America" = "#FF00DB")) +
          theme_minimal()
          girafe(ggobj = g_plot, width = 9, options = list(opts_selection(type = "multiple")))
    })
    
    selected_ids <- reactive({
      input$girafePlot_selected
    })
})

shinyApp(ui = ui, server = server)
