
library(shiny)
library(dplyr)
library(ggplot2)

Nobel_df<-read.csv("complete.csv")

server<-function(input, output) {
    
    output$plot1 <- renderPlot({
        tmp<-Nobel_df %>% 
            filter(birth_continent==input$kontynent & category==input$dziedzina) %>% 
            group_by(gender) %>% 
            summarise(liczba=n())
        ggplot(data=tmp,aes(x=gender,y=liczba))+
            geom_col()+
            labs(
                x="płeć"
            )
        
    })
    output$plot2<-renderPlot({
        tmp2<- Nobel_df[!duplicated(Nobel_df$awardYear),]
        tmp2<-Nobel_df %>% 
            filter(category=="Chemistry" & awardYear >= input$DatesMerge[1] & awardYear <= input$DatesMerge[2])  #dla wszystkich dziedzin nagrody sa jednakowe w danym roku
        ggplot(data=tmp2,aes(x=awardYear,y=prizeAmountAdjusted))+
            geom_line()+
            labs(
                title="Porównanie wysokości nagród w przeliczeniu na dzisiejsze korony",
                subtitle="Lata 1901-2019",
                x="Rok",
                y="Liczba koron"
            )
    })
}
