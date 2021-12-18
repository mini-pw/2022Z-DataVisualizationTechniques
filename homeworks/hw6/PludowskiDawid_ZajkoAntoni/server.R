library(shiny)

source('prizes_count_over_decades.R')
source('box_prizes_over_continents.R')

shinyServer(function(input, output, session) {

    observeEvent(input$decade,{
      updateSliderInput(session,'decade',
                        label = input$decade)
    }) 
    
    output$plot1 <- renderPlotly({
        decade <- input$decade
        continent <- input$checkbox
        draw_box_prizes_over_continents(continent, decade)
    })
    
    output$plot2 <- renderPlotly({
        decade <- input$decade
        draw_prizes_count_over_decades(decade)
    })

})
