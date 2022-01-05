server <- function(input, output) {
  
  output$countries_plot <- renderPlot({
    nobel_df %>% 
      filter(category %in% input$category) %>% 
      filter(birth_countryNow != "") %>% 
      group_by(birth_countryNow) %>% 
      summarise(count = n()) %>% 
      mutate(birth_countryNow = fct_reorder(birth_countryNow, count)) %>% 
      arrange(-count) %>% 
      head(input$top_n) %>% 
      ggplot() + 
      geom_col(aes(x = count, y = birth_countryNow)) + 
      scale_x_continuous(expand = expansion(mult = c(0, .1))) + 
      labs(
        x = "Number of Nobel Prize Laureates", 
        y = ""
      ) + 
      theme_bw()
  }) %>% bindCache(list(input$category, input$top_n))
  
  output$continents_plot <- plotly::renderPlotly({
    df <- nobel_df %>% 
      filter(birth_continent %in% input$continent) %>% 
      filter(birth_continent != "") %>% 
      group_by(birth_continent, category) %>% 
      summarise(count = n()) 
    
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'markers',
      fill = 'toself'
    ) 
    
    for (i in 1:length(input$continent)) {
      fig <- fig %>% 
        add_trace(
          data = df %>% filter(birth_continent == input$continent[i]),
          r = ~count,
          theta = ~category,
          hovertemplate = paste("Number of Laureates: %{r}<br>Category: %{theta}"),
          name = input$continent[i]
        )
    }

    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, max(df$count))
          )
        ),
        showlegend = T
      ) %>% 
      config(displayModeBar = FALSE)
    
    fig
  }) %>% bindCache(input$continent)
  
  
  output$cities_plot <- leaflet::renderLeaflet({
    pal <- colorFactor(plasma(length(unique(nobel_df$category))), domain = unique(nobel_df$category))
    
    df <- birthPlace_df %>% filter(category %in% input$map_categories)
    
    content <- lapply(seq(nrow(df)), function(i) {
      as.character(tagList(
        tags$style(
          ".leaflet-tooltip{ width: 350px; white-space: normal; }"
        ),
        tags$h4("Name:", df$name[i]), 
        tags$b("Award Year: "), as.character(df$awardYear[i]), tags$br(),
        tags$b("Birth City: "), as.character(df$birth_cityNow[i]), tags$br(),
        tags$b("Birth Country: "), as.character(df$birth_countryNow[i]), tags$br(),
        tags$b("Category: "), as.character(df$category[i]), tags$br(),
        tags$b("Motivation: "), as.character(df$motivation[i])
      ))
    })
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        data = df,
        lng = ~lon, 
        lat = ~lat,
        radius = 4,
        color = ~pal(category),
        stroke = FALSE, 
        fillOpacity = 0.8, 
        label = lapply(content, htmltools::HTML)
      ) %>%
      addLegend(pal = pal, values = unique(nobel_df$category), opacity = 1)
   
  
  }) %>% bindCache(input$map_categories)
}



