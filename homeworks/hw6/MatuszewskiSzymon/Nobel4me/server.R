#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

nobel = read.csv("nobel.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$fig1 <- renderPlot({
        
        data <- nobel %>% 
            filter(category == input$categories & birth_cityNow != "") %>% 
            group_by(birth_countryNow) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            head(5)
        fig <- ggplot(data,aes(x = birth_countryNow,y = count)) +
            geom_bar(stat = "identity",color = "black",fill = "#A9E2B9") +
            labs(
                x = "Country",
                y = "Number of Nobel Awards"
            ) +
            theme_bw() +
            theme(
                axis.title = element_text(size = 17),
                axis.text = element_text(size = 15)
            ) + 
            scale_y_continuous(expand = c(0.001,0),limits = c(0,data$count[1] + 5))
        
        if(input$buttons1 == "Yes"){
            fig <- fig + 
                geom_text(stat = "identity", aes(x = birth_countryNow,y = count,label = count),vjust = -0.25,size = 8)
        }
        fig
    })
    
    output$fig2 <- renderPlot({
        data <- nobel %>% 
            filter(birth_countryNow == input$countries) %>% 
            group_by(category) %>% 
            summarise(count = n())
        fig <- ggplot(data,aes(x = category,y = count)) +
            geom_bar(stat = "identity",color = "black", fill = "#ABCDEF") +
            labs(
                x = "Category",
                y = "Number of Nobel Awards"
            ) +
            theme_bw() +
            theme(
                axis.title = element_text(size = 17),
                axis.text = element_text(size = 15)
            ) + 
            scale_y_continuous(expand = c(0.001,0),limits = c(0,data$count[1] + 5))
        
        if(input$buttons2 == "Yes"){
            fig <- fig + 
                geom_text(stat = "identity", aes(x = category,y = count,label = count),vjust = -0.25,size = 8)
        }
        fig
    })
    
    accumulate_by <- function(dat, var) {
        var <- lazyeval::f_eval(var, dat)
        lvls <- plotly:::getLevels(var)
        dats <- lapply(seq_along(lvls), function(x) {
            cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
        })
        dplyr::bind_rows(dats)
    }
    
    df <- nobel %>% 
        filter(category != "Economic Sciences") %>% 
        group_by(awardYear,category) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        group_by(category) %>% 
        mutate(cumsum = cumsum(count))
    
    df <- df %>% accumulate_by(~awardYear)
    
    output$fig3 <- renderPlotly({
        
        plot_ly(
            data = df, 
            x = ~awardYear, 
            y = ~cumsum,
            frame = ~frame,
            color = ~category,
            colors = "Set1",
            mode = "lines",
            text = paste0("Year: ", df$awardYear, "<br>Cumsum: ", df$cumsum, "<br>",df$category),
            hoverinfo = 'text'
        ) %>%
            layout(
                title = "Cumulative sum of Nobel awards by category", 
                xaxis = list(title = 'Year'), 
                yaxis = list(title = 'Cumulative sum'),
                legend = list(
                    x = 1, y = 0.7,
                    title = list(text = "Legend")
                )
            ) %>% 
            add_lines(x = df$awardYear, y = df$cumsum) %>%
            plotly::config(displayModeBar = FALSE) %>% 
            animation_opts(10) %>%
            animation_button() %>%
            animation_slider(currentvalue = list(prefix = "YEAR: ", font = list(color="red")))
        
    })


})
