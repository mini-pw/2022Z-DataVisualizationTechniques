library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjqui)
library(plotly)

df <- read.csv("complete.csv", encoding="UTF-8")

ui1 <- fluidPage(
    fluidRow(
        wellPanel(
            checkboxInput(
                inputId="alive",
                label="Laureates alive as of 2019",
                value=FALSE
            ),
            
            orderInput("source", "Available continents", items=c("North America",
                                                       "Asia", "Europe", 
                                                       "Africa",
                                                       "South America",
                                                       "Oceania"),
                       connect="dest"),
            orderInput("dest", "Selected continents", items=NULL,
                       placeholder="Drag items here...", connect="source"),
            width = 5
        ),
        
        # Show a plot of the generated distribution
        column(
           plotly::plotlyOutput("plot1"),
           width=7
        )
    )
)

options(scipen = 999)
ui2 <- fluidPage(
    titlePanel("Ditribututions of age and money awards of Nobel prizes"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(
                inputId = "Continent",
                label = "Select continent",
                choices = c("North America","Europe","Asia","Africa",
                            "South America","Oceania" ),
                selected = "Europe"
            ),
            selectInput(
                inputId = "Category",
                label = "Select category",
                choices = unique(df$category),
                selected = "Peace"
            ),
            selectInput(
                inputId = "Yaxis",
                label = "What variable do you want to check?",
                choices = c("Money Prize Amount", "Age")
            )
        ),
        mainPanel(
            plotly::plotlyOutput("plot2")
        )
    )
)

app_ui <- navbarPage(
    title="Noble Prizes Analysis",
    tabPanel("Chart 1", ui1),
    tabPanel("Chart 2", ui2),
    theme = bslib::bs_theme(bootswatch = "cosmo")
)

server <- function(input, output) {

    output$plot1 <- plotly::renderPlotly({
        # Alive?
        df1 <- df
        title = "Number of Noble prize laureates"
        if(input$alive == TRUE) {
            df1 <- df %>%
                filter(death_date == "")
            title = "Number of alive Noble prize laureates"
        }
        # Birth now
        # By country
        # Filter out continets 
        p <- ggplot(df1 %>% filter(birth_continent %in% input$dest)) + 
            geom_bar(aes(x=birth_continent), fill="blue", alpha=0.8) +
            theme_bw() + 
            scale_y_continuous(expand=expansion(mult= c(0, 0.1))) +
            labs(
                title=title,
                x="Continent",
                y="Number of laureates"
            )
        
        plotly::ggplotly(p) %>% 
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>% 
            config(displayModeBar = FALSE)
    })
    output$plot2 <- renderPlotly({
        #choose continents and category
        df1 <- df %>% filter(birth_continent != "", category == input$Category)
        df1 <- df1 %>%  filter(birth_continent %in% input$Continent)
        if(input$Yaxis == "Money Prize Amount"){
        plot <- ggplot(df1, aes(x = birth_continent, y = prizeAmountAdjusted)) +
            geom_boxplot(fill = "navyblue") +
            theme_bw() +
            labs(title = "Distribution of money awards of Nobel prizes",
                 x = "Continent", y = "Prize (inflacy - adjusted)")
        }
        else{
            df1 <- df1 %>% 
                filter(birth_date != "", awardYear != "")
            #extract year from birth_date
            df1$birth_date <- as.numeric(stringr::word(df1$birth_date,1,sep = "-"))
            #Calculate age when awarded Nobel prize (based on years, not specific dates)
            df1 <- df1 %>% 
                mutate(Age = awardYear - birth_date)
            plot <- ggplot(df1, aes(x = birth_continent, y = Age)) +
                geom_boxplot(fill = "navyblue") +
                theme_bw() +
                labs(title = "Distribution of age of Nobel prizes awardees",
                     x = "Continent", y = "Age")
        }
        ggplotly(plot)
    })
}

shinyApp(ui = app_ui, server = server)

