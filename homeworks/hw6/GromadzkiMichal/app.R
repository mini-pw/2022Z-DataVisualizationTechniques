library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(scales)
library(bslib)

df <- read.csv("complete.csv")

#server
server <- function(input, output, session) {
    
    output$plot1 <- plotly::renderPlotly({

        df1 <- df %>% 
            filter(category==input$kategoria) %>% 
            group_by(birth_countryNow) %>% 
            summarise(suma=sum(prizeAmountAdjusted)) %>% 
            arrange(desc(suma)) %>% 
            head(10)
        
        
        p<- ggplot(df1,aes(x=fct_reorder(birth_countryNow,suma),y=suma))+
            geom_col()+
            labs(
                title=paste("Sum of prize money by birth country of the winner in",input$kategoria),
                y="Sum of prize money",
                x="Birth country"
            )+
            theme_light()+
            scale_y_continuous(labels = comma)+
            scale_x_discrete(guide=guide_axis(n = 2))
        
        ggplotly(p)
    })
    
    output$plot2 <- plotly::renderPlotly({
        
        df2 <- df %>% 
            filter(birth_continent==input$kontynent) %>% 
            group_by(category) %>% 
            summarise(n=n()) %>% 
            arrange(desc(n))
        
        
        p2 <- ggplot(df2,aes(x=fct_reorder(category,n),y=n))+
            geom_col()+
            labs(
                title=paste("Number of awards by category in",input$kontynent),
                y="Number of awards",
                x="Category"
            )+
            theme_light()+
            scale_x_discrete(guide=guide_axis(n = 2))
        
        plotly::ggplotly(p2)
    })
}

#Zakladka 1
ui <- fluidPage(
    
    titlePanel("Wykres 1."),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "kategoria",
                label = "Category:",
                choices = c("Economic Sciences","Physics","Chemistry", "Peace","Physiology or Medicine", "Literature")
            ),
            width = 3
        ),
        mainPanel(
            plotly::plotlyOutput("plot1"),
            width = 9
        )
    )
)

#Zakladka 2
ui2 <- fluidPage(
    titlePanel("Wykres 2."),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "kontynent",
                label = "Continent: ",
                choices = c("North America", "Europe", "Asia", "Africa","South America", "Oceania")
            ),
            width = 3
        ),
        mainPanel(
            plotly::plotlyOutput("plot2"),
            width=9
        )
    )
)

#Polaczenie zakladek
app_ui <- navbarPage(
    title = "hw6 - Michal Gromadzki",
    tabPanel("Wykres 1.", ui),
    tabPanel("Wykres 2.", ui2),
    theme=bs_theme(bootswatch = "cosmo"))

shinyApp(app_ui, server)
