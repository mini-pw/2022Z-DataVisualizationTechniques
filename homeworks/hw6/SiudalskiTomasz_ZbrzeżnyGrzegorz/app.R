library(shiny)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(memoise)
library(stringi)

df <- read.csv("nobel.csv") %>% 
    mutate(death_country = replace(death_country, death_country == "", "Brak danych/Wciąż żyje"))

countries <- df %>% select(birth_country) %>% distinct()

categories <- unique(df$category)

getTermMatrix <- memoise(function(chosenCategory) {
    
    text <- df %>% filter(category == chosenCategory) %>% 
        select(motivation) %>% unlist() %>%
        stri_paste(collapse=' ')
    
    myCorpus = VCorpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "the", "and", "but"))
    
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
})

ui <- fluidPage(
    
    titlePanel("Wykres punktowy"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "krajPochodzenia",
                        label = "Wybierz kraj:",
                        choices = countries$birth_country),
            radioButtons(inputId = "kolor",
                         label = "Wybierz po czym kolorować: ",
                         choices = c("Płec" = "gender", "Status nagrody" = "prizeStatus", "Kraj śmierci" = "death_country"))
        ),
        
        mainPanel(
            plotly::plotlyOutput("plot1", height = 600),
            width = 8,
        )
    )
)

ui2 <- fluidPage(
    titlePanel("Chmura słów z motywacji laureatów"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("selection", "Wybierz kategorię:",
                        choices = categories),
            actionButton("update", "Zmień"),
            hr(),
            sliderInput("freq",
                        "Minimalna częstość:",
                        min = 1,  max = 15, value = 10),
            sliderInput("max",
                        "Maksymalna liczba słów:",
                        min = 1,  max = 50,  value = 20)
        ),
        
        mainPanel(
            plotOutput("plot", height = 600)
        )
    )
)

server <- function(input, output, session) {
    
    output$plot1 <- plotly::renderPlotly({
        p <- ggplot(df %>% filter(birth_country == input$krajPochodzenia), aes_string(x = "category", y = "awardYear", color = input$kolor, label = "name")) + 
            geom_point() + scale_y_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, by = 10)) + xlab("Kategoria") +
            ylab("Rok nadania nagrody") + theme(axis.text.x = element_text(angle = 45))
        plotly::ggplotly(p)
    })
    
    terms <- reactive({
        input$update
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix(input$selection)
            })
        })
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        v <- terms()
        wordcloud_rep(names(v), v, scale=c(4,0.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))
    })
}


app_ui <- navbarPage(
    title = "Praca Domowa 06",
    tabPanel("Wykres 1", ui),
    tabPanel("Wykres 2", ui2),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Zbrzeżny&Siudalski Company © 2021 Copyright
                </p>
                </footer>
                "))

shinyApp(ui = app_ui, server = server)
