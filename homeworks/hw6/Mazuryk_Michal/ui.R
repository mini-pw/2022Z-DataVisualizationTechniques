library(shiny)
DF<- read.csv("complete.csv")
shinyUI(
    fluidPage(
        
        titlePanel("Countries with the most winners"),
        
        sidebarLayout(
            sidebarPanel(
                
                selectInput("cont", "Select continent", unique(DF$birth_continent),selected = "Europe"),
                checkboxGroupInput("categ", 
                                   "Select category:",
                                   c("Economic Sciences" = "Economic Sciences",
                                     "Physics" = "Physics",
                                     "Chemistry" = "Chemistry",
                                     "Peace" = "Peace",
                                     "Physiology or Medicine" = "Physiology or Medicine",
                                     "Literature" = "Literature"),
                                   selected = c("Economic Sciences","Physics",
                                                "Chemistry","Peace","Physiology or Medicine","Literature"))
  
            ),
            
            mainPanel(plotOutput("plotgraph1"))),
        
        titlePanel("Most popular names of the winners"),
        
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput("gend", 
                                   "Select gender:",
                                   c("male" = "male",
                                     "female" = "female"),
                                   selected = c("male","female")
                )),
            
            mainPanel(plotOutput("plotgraph2"))
        )
        
    )
)

