#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(plotly)
library(textstem)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    options(warn=-1)
    output$distPlot <- renderPlotly({
        
        namesNobel <- function(country, years){
            stri <- c()
            for (i in 1:length(years)){
                names <- my_data %>%
                    filter(birth_country==country & awardYear==years[i]) %>%
                    select(name) %>%
                    unlist()
                stri[i] <- ""
                for (n in names){
                    stri[i] <- paste(stri[i],n,sep="<br>")
                }
            }
            stri
        }
        
        if ( options()$stringsAsFactors )
            options(stringsAsFactors=FALSE)
        my_data <- read.csv("complete.csv")
        
        
        df <- my_data %>%
            select(awardYear,birth_country) %>%
            filter(birth_country == input$country) %>%
            group_by(birth_country,awardYear) %>%
            count() %>%
            ungroup()
        
        
        df <- df %>%
            arrange(birth_country,awardYear)%>%
            group_by(birth_country)%>%
            mutate(suma=cumsum(n))
        
        df <- df %>%
            mutate(tekst = namesNobel(birth_country,awardYear))
        
        df %>%
            plot_ly(
                x = ~awardYear,
                y = ~suma,
                
                type = 'scatter',
                mode = 'lines+markers',
                hovertemplate = ifelse(rep(input$showNoblists,length(df$tekst)), paste('%{x}',paste("Noblists:<br>",df$tekst,"<extra></extra>")),
                                                              paste(input$country,'<br>%{x}<br><extra></extra>'))
                
                
            ) %>%
            layout(
                title = paste("Birth Country: ",input$country),
                    
                bgcolor = "#E2E2E2",
                xaxis = list(
                    title = "Award Year"
                ),
                yaxis = list(
                    title = "Number of Nobel Prizes"
                ),
                images = list(source = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Nobel_logo.svg/1200px-Nobel_logo.svg.png",
                              xref = "paper",
                              yref = "paper",
                              x= 0.1,
                              y= 0.95,
                              sizex = 0.2,
                              sizey = 0.2,
                              opacity = 0.6
                )
            ) %>% 
            config(displayModeBar = F)
        
        
    })
    
    
    output$table1 <- renderDataTable({
        
        if ( options()$stringsAsFactors )
            options(stringsAsFactors=FALSE)
        my_data <- read.csv("complete.csv")
        
        my_data %>%
            filter(birth_country==input$country) %>%
            select(name,awardYear,category) %>%
            arrange(-awardYear,name)
    })
    
    output$distPlot2 <- renderPlotly({
        
        if ( options()$stringsAsFactors )
            options(stringsAsFactors=FALSE)
        my_data <- read.csv("complete.csv")
        
        words <- c()
        
        for (i in seq(1,length(my_data$motivation),1)){
            words <- append(words,strsplit(my_data$motivation[[i]]," "))
        }
        
        words <- unlist(words)
        words <- lemmatize_words(words)
        
        df <- as.data.frame(table(words)) %>%
            arrange(desc(Freq))
        
        df$words = as.character(df$words)
        df %>%
            filter(nchar(words,type="chars")>=input$bins)%>%
            head(5) %>%
            mutate(words = forcats::fct_reorder(words,Freq))%>%
            plot_ly(x=~words,y=~Freq,type="bar") %>%
            layout(
                title = "Words appearance in Nobel Prize motivation",
                xaxis = list(
                    title = paste("Minimum word length: ",input$bins)
                ),
                yaxis = list(title="Frequency")
            )  %>% 
            config(displayModeBar = F)
        })
    
    output$distPlot3 <- renderPlotly({
        
        if ( options()$stringsAsFactors )
            options(stringsAsFactors=FALSE)
        my_data <- read.csv("complete.csv")
        
        df <- my_data %>% 
            filter(birth_date != "" & dateAwarded != "") 
        
        df$birth_date = as.numeric(substr(df$birth_date,0,4))
        
        df$age = df$awardYear - df$birth_date
        
        categories <- unique(my_data$category)
        loginp <- c(input$ch1,input$ch2,input$ch3,input$ch4,input$ch5,input$ch6)
        
        df %>%
            filter(category %in% categories[loginp])%>%
            ggplot(aes(x=age,fill=category))+
            geom_density(alpha=0.5)+
            labs(title = "Distribution of Age",
                 x="Age",
                 y ="Density" )+
            guides(fill=guide_legend(title="Category"))
        
    })
        
        
})

