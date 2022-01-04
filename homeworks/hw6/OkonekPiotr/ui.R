
library(shiny)
library(dplyr)
library(ggplot2)

Nobel_df<-read.csv("complete.csv")


ui1<-fluidPage(
    
    
    titlePanel("Płeć zwycięzców Nagrody Nobla z poszczególnych kontynentów"),
    
    
    sidebarLayout(
        sidebarPanel(
            selectInput("kontynent", "Wybierz kontynent", unique(Nobel_df$birth_continent),selected = "Europe"),
            selectInput("dziedzina","Wybierz dziedzinę",unique(Nobel_df$category,selected="Peace"))
        ),
        
        
        
        mainPanel(
            plotOutput("plot1")
        )
    )
)
ui2 <- fluidPage(
    titlePanel("Wysokości nagród"),
    
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("DatesMerge",
                        "Rok otrzymania nagrody:",
                        min = 1901,
                        max = 2019,
                        value = c(1923,1961),
                        sep=" ")
            
        ),
        mainPanel(
            plotOutput("plot2")
        )
    )
)

app_ui <- navbarPage(
    title = "Analiza danych: Nagroda Nobla",
    tabPanel("Mężczyźni vs Kobiety", ui1),
    tabPanel("Wysokości nagród", ui2),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)
