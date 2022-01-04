library(ggplot2)
library(dplyr)
library(shinyWidgets)
dane <- read.csv(file = "complete.csv")
dane <- dane %>% filter(gender == "male" | gender == "female")
dane2 <- dane %>% group_by(birth_countryNow, birth_continent) %>% summarise(n = n())
server <- shinyServer(function(input, output) {
    
    output$plot1 <- renderPlot({
        
        if (input$malo & !input$wiecej & !input$duzo) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria & dane$prizeAmountAdjusted <= 3000000 ,], 
                aes(x = awardYear, y = prizeAmountAdjusted)
                ) +
                    geom_point(aes(color = gender))+ 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        if (!input$malo & input$wiecej & !input$duzo) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria & dane$prizeAmountAdjusted > 3000000 & dane$prizeAmountAdjusted <= 6000000,], 
                aes(x = awardYear, y = prizeAmountAdjusted)
            ) +
                geom_point(aes(color = gender))+ 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        if (!input$malo & !input$wiecej & input$duzo) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria & dane$prizeAmountAdjusted > 6000000,], 
                aes(x = awardYear, y = prizeAmountAdjusted)
            ) +
                geom_point(aes(color = gender))+ 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        if (input$malo & input$wiecej & !input$duzo ) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria & dane$prizeAmountAdjusted <= 6000000,], 
                aes(x = awardYear, y = prizeAmountAdjusted)
            ) +
                geom_point(aes(color = gender))+ 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        if (input$malo & !input$wiecej & input$duzo ) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria & (dane$prizeAmountAdjusted <= 3000000 | dane$prizeAmountAdjusted > 6000000),], 
                aes(x = awardYear, y = prizeAmountAdjusted)
            ) +
                geom_point(aes(color = gender))+ 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        if (!input$malo & input$wiecej & input$duzo ) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria & dane$prizeAmountAdjusted > 3000000,], 
                aes(x = awardYear, y = prizeAmountAdjusted)
            ) +
                geom_point(aes(color = gender))+ 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        if (input$malo & input$wiecej & input$duzo ) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria,], 
                aes(x = awardYear, y = prizeAmountAdjusted)
            ) +
                geom_point(aes(color = gender)) + 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        if (!input$malo & !input$wiecej & !input$duzo ) {
            p1 <- ggplot(
                dane[dane$category == input$kategoria & dane$prizeAmountAdjusted > 60000000,], 
                aes(x = awardYear, y = prizeAmountAdjusted)
            ) +
                geom_point(aes(color = gender))+ 
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                xlab("Rok")+
                ylab("Wartosc nagrody")+
                scale_color_discrete(name = "Plec")
        }
        
        p1
    })
    
    output$plot2 <- renderPlot({
        p2 <- ggplot(dane2[dane2$birth_continent == input$kontynent,], aes(n, birth_countryNow))+
            geom_col(fill = "#FFDB6D")
        
        if(input$choice=="Posortuj slupki rosnaco"){
            p2 <- ggplot(dane2[dane2$birth_continent == input$kontynent,], aes(n, reorder(birth_countryNow, -n)))+
                geom_col(fill = "#FFDB6D")+
                xlab("ilosc nagrod")+
                ylab("kraj")
        }
        
        if(input$choice=="Posortuj slupki malejaco"){
            p2 <- ggplot(dane2[dane2$birth_continent == input$kontynent,], aes(n, reorder(birth_countryNow, n)))+
                geom_col(fill = "#FFDB6D")+
                xlab("ilosc nagrod")+
                ylab("kraj")
        }
        p2
    
    })
    
    
})

 
ui <- shinyUI(fluidPage(
    titlePanel("Nagrody na przestrzeni lat"),
    sidebarLayout(
        sidebarPanel(
            
            selectInput("kategoria", "Wybierz kategorie", unique(dane$category)),
            
            prettyCheckbox(inputId = "malo", label = "Nagroda mniejsza niz 3000000",
                           shape = "round", outline = TRUE, status = "info"),
            prettyCheckbox(inputId = "wiecej", label = "Nagroda wieksza niz 3000000, ale mniejsza niz 6000000",
                           shape = "round", outline = TRUE, status = "info"),
            prettyCheckbox(inputId = "duzo", label = "Nagroda mniejsza niz 3000000",
                           shape = "round", outline = TRUE, status = "info"),
            
        ),
        mainPanel(
            plotOutput("plot1")
        )
    )
))

ui2 <- fluidPage(
    titlePanel("Ilosc laureatow z danych panstw"),
    sidebarLayout(
        sidebarPanel(
            selectInput("kontynent", "Wybierz kontynent", unique(dane$birth_continent)), 
            
            radioButtons(inputId="choice", label="sortowanie slupkow", 
                         choices=c("Posortuj slupki rosnaco","Posortuj slupki malejaco"))
            
          ),
        
        mainPanel(
            plotOutput("plot2")
        )
    )
)


app_ui <- navbarPage(
    title = "Nagrody Nobla",
    tabPanel("Nagrody pieniezne", ui, icon = icon("dollar-sign")),
    tabPanel("Pochodzenie laureatow", ui2, icon = icon("flag")))

shinyApp(app_ui, server)

#app <- shinyApp(ui, server)

#app
