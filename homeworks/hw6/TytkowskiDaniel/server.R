server <- function(input, output, session){
  
  output$plot1 <- plotly::renderPlotly({
    df2 <- df %>% 
      filter(category == input$kategoria) %>% 
      filter(birth_country != "") %>% 
      group_by(birth_countryNow) %>% 
      summarise(Count = n()) %>% 
      arrange(desc(Count)) %>% 
      head(input$ilosc)
    
    fig <- plot_ly(showlegend = FALSE, data = df2, x = ~birth_countryNow, y = ~Count, type = "bar") %>% 
      layout(title = paste("5 most frequently awarded countries in ", input$kategoria,"\n (only individual candidates)"),
             yaxis = list(title = "Count"),
             xaxis = list(title = "Country"))
    
  })
  
  output$plot2 <- renderPlot({
    
    fig2 <- ggplot(df %>% 
                     filter(birth_continent == input$kontynent) %>% 
                     group_by(birth_continent, category, gender) %>% 
                     summarise(Count = n())
                   , aes(x = category, fill = gender, y = Count))+
      geom_col()+
      labs(title = paste("Count of awarded Nobel prizes in",input$kontynent),
           x = "Category",
           y = "Count")+
      theme(title = element_text(size = 15))
    
    plot(fig2)
  })
  
  output$plot3 <- renderPlot({
    dftest <-df %>% 
      filter(birth_countryNow != "") %>%
      filter(category %in% input$kategoriaMapa) %>% 
      filter(gender %in% input$plec) %>% 
      group_by(birth_countryNow) %>% 
      summarise(Count = n())
    
    country <- map_data("world")
    country %>% 
      left_join(dftest, by =c("region" = "birth_countryNow")) %>% 
      ggplot(aes(long, lat)) + 
      geom_polygon(aes(group = group, fill = Count))+
      labs(x = element_blank(), y = element_blank(), title = "Most awarded countries by categories\n(birth countries of individual laureats)")+
      theme(axis.text = element_blank(), title = element_text(size = 15))
    
    
    
  })
  
}