library(shiny)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

DF<- read.csv("complete.csv")

DF<- DF %>% 
    select(awardYear,category,givenName,birth_continent,birth_countryNow,gender)

shinyServer(function(input, output) {
    
    output$plotgraph1 <-renderPlot({
        
        DF1<- DF %>% 
            filter(birth_continent==input$cont) %>% 
            filter(category %in% input$categ) %>% 
            group_by(birth_countryNow) %>%
            summarise(n=n()) %>% 
            arrange(-n) %>% 
            head(6)
        
        ggplot(data = DF1,aes(x = birth_countryNow, y = n)) + 
            geom_col(fill="gold") +
            labs(title = "The most awarded countries",
                 y = "Number of winners",
                 x = "Country")+
            coord_flip()
    })
    
    output$plotgraph2 <-renderPlot({
        if(!is.null(input$gend)){
            DF2<- DF %>% 
                filter(gender %in% input$gend) %>% 
                group_by(givenName) %>% 
                summarise(n = n())
            set.seed(120)
            wordcloud(words = DF2$givenName, freq = DF2$n, min.freq = 1,
                      max.words=100, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))
        }
    })
})