library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(rsconnect)
shinyServer(function(input, output) {

    df <- read.csv("./Data/complete.csv") %>%
        na.omit() %>%
        select(where(is.numeric))
    df2 <- read.csv("./Data/complete.csv") %>%
        na.omit() %>%
        arrange(birth_country)
    output$chart <- renderPlotly({
        col_x <- sym(input$dropdownX)
        col_y <- sym(input$dropdownY)
        
        p <- ggplot(df, aes(x = !!col_x, y = !!col_y)) +
            geom_point() +
            labs(title = input$inputTitle)
        ggplotly(p)
    })
    output$countries <- renderPlotly({
        df_temp <- df2 %>%
            filter(birth_country == input$country)
        if(input$category != "all"){
            df_temp <- df_temp %>%
                filter(category == input$category)
            ggplotly(ggplot(df_temp,aes(x = input$category, y = nrow(df_temp))) +
                         geom_col() +
                         labs(title=paste("Nr of winners of",input$category,"Nobel prize from",input$country)) +
                         ylab("Nr of winners") +
                         xlab("category") +
                         ylim(0,nrow(df_temp) + 3) +
                         theme_bw())
        } else{
            df_temp <- df_temp %>%
                group_by(category) %>%
                tally()
            ggplotly(ggplot(df_temp,aes(x = category, y = n)) +
                         geom_col() +
                         labs(title=paste("Nr of winners of Nobel prize from",input$country,"by each category")) +
                         ylab("Nr of winners") +
                         ylim(0,ifelse(max(df_temp$n) > 10,max(df_temp$n) + 1, 11)) +
                         theme_bw())
        }
        
    })

})
