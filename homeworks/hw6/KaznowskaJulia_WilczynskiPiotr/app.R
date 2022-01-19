# import libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)

# import data
#setwd(".")
df <- read.csv("complete.csv")

# prepare data
df <- df %>% filter(ind_or_org == "Individual")
df <- df %>% mutate(birthYear = as.numeric(substr(df$birth_date,0,4)),
                    age = awardYear - birthYear) %>% 
    arrange(category, decreasing = T)

# generate plots
server <- function(input, output, session) {
    
    # plot - Page 1
    output$plot1 <- renderPlotly({
        
        # color data frame
        catCols <- data.frame(category = unique(df$category),
                              color = c("#F8766D","#B79F00",
                                        "#00BA38","#00BFC4",
                                        "#619CFF","#F564E3")) %>% 
            arrange(category)
        
        # Nobel Prize winners modified data frame
        dftemp <- as.data.frame(df %>% filter(birth_continent == input$continent,
                                category %in% input$categories))
        
        # removing redundant colors
        dfcheck <- dftemp %>% group_by(category) %>% summarise(cnt = n())
        catCols <- catCols %>% filter(category %in% dfcheck$category)
        
        # create empty plot (case when there is no records of Nobel Prize
        # winners with the criteria selected)
        if(dim(dftemp)[[1]] == 0){
            
            p <- ggplot(data.frame()) +
                    geom_point() +
                    theme_bw() +
                    ylim(2250000, 12500000) +
                    labs(x = "Age at the time of receiving the prize ",
                         y = "The amount of the prize [SEK]",
                         title = "The age and amount of money received by Nobel laureates") +
                    theme(
                        legend.position = "none"
                    ) +
                    scale_x_continuous(limits = c(15, 100), breaks = seq(15, 101, 5))
            
            p <- ggplotly(p)
            
        } else {
        # create plot in every other case    
            p <- ggplot(dftemp %>% arrange(category, decreasing = T),
                        aes(x = age, y = prizeAmountAdjusted,
                            text = paste(paste("Name: ", knownName),
                                         paste("Age:", age),
                                         paste("Prize:", prizeAmountAdjusted),
                                         sep = "\n"),
                            color = category)) +
                geom_point(size = 2) +
                theme_bw() +
                ylim(2250000, 12500000) +
                labs(x = "Age at the time of receiving the prize ",
                     y = "The amount of the prize [SEK]",
                     title = "The age and amount of money received by Nobel laureates") +
                theme(
                    legend.position = "none"
                ) +
                scale_color_manual(values = 
                                       (catCols %>% filter(category %in% input$categories) %>% 
                                            arrange(category))$color) +
                scale_x_continuous(limits = c(15, 100), breaks = seq(15, 101, 5))
            
            p <- ggplotly(
                p,
                tooltip = "text"
            )
            
        }
        
        p <- p %>% config(displayModeBar = FALSE)
        
    })
    
    # plot - Page 2
    output$plot2 <- renderPlotly({
        
        # prepare data
        df1 <- df %>% 
            select(awardYear, category, birth_continent, gender, birth_date) %>% 
            mutate(birth_date = as.numeric(substr(birth_date, 1, 4))) %>% 
            mutate(age = awardYear - birth_date) %>% 
            filter(awardYear >= input$years[1] & awardYear <= input$years[2]) %>% 
            filter(if_all(everything(), ~ !is.na(.x)))
        
        if(input$gender != "All"){
            df1 <- df1 %>% 
                filter(gender == tolower(input$gender))
        }
        if(input$category != "All"){
            df1 <- df1 %>% 
                filter(category == input$category)
        }
        
        # create empty plot (case when there is no records of Nobel Prize
        # winners with the criteria selected)
        if(dim(df1)[[1]] == 0){
            print("abc")
            boxplt <- ggplot(data.frame()) +
                geom_point() +
                labs(title = "Age distribution of Nobel Prize laureates",
                     x = "Continent", y = "Age") +
                ylim(0, 100) +
                theme_bw()
        } else{
        # create plot in every other case
            boxplt <- ggplot(df1, aes(x = birth_continent, y = age)) +
                geom_boxplot() +
                labs(title = "Age distribution of Nobel Prize laureates",
                     x = "Continent", y = "Age") +
                ylim(0, 100) +
                theme_bw()
        }
        
        boxplt <- ggplotly(boxplt)
        
        boxplt <- boxplt %>% config(displayModeBar = FALSE)
        
    })
}

# Page 1
ui1 <- fluidPage(
    
    titlePanel("The age and amount of money by continent"),
    
    # layout
    sidebarLayout(
        sidebarPanel(
            # side bar contents
            selectInput(
                inputId = "continent",
                label = "Continent:",
                choices = unique(df$birth_continent)
            ),
            checkboxGroupInput(
                inputId = "categories",
                label = "Categories:",
                selected = unique(df$category),
                inline = FALSE,
                choiceNames = list(
                     tags$div("Chemistry", icon("circle", style = "color:#F8766D; font-size: 0.73em;")),
                     tags$div("Economic Sciences", icon("circle", style = "color:#B79F00; font-size: 0.73em;")),
                     tags$div("Literature", icon("circle", style = "color:#00BA38; font-size: 0.73em;")),
                     tags$div("Peace", icon("circle", style = "color:#00BFC4; font-size: 0.73em;")),
                     tags$div("Physics", icon("circle", style = "color:#619CFF; font-size: 0.73em;")),
                     tags$div("Physiology or Medicine", icon("circle", style = "color:#F564E3; font-size: 0.73em;"))),
                choiceValues = unique(df$category)
            ),
            width = 3
        ),
        mainPanel(
            # main panel contents
            list(tags$head(tags$style("body {background-color: #B6C1C3; }"))),
            plotlyOutput("plot1")
        )
    )
)

# Page 2
ui2 <- fluidPage(
    
    titlePanel("Age distribution by continent"),
    
    # layout
    sidebarLayout(
        sidebarPanel(
            # side bar contents
            selectInput(inputId = "category",
                        label = "Category:",
                        choices = c("All", unique(df$category))),
            sliderInput(inputId = "years", label = "Timeframe:",
                        min = min(df$awardYear), max = max(df$awardYear),
                        value = c(1970,2000)),
            radioButtons(inputId = "gender", label = "Gender:",
                         choices = c("All", "Female", "Male"))
        ),
        
        # main panel contents
        mainPanel(
            list(tags$head(tags$style("body {background-color: #B6C1C3; }"))),
            plotly::plotlyOutput("plot2")
        )
    )
    
)

# navigation bar, theme, footer
app_ui <- navbarPage(
    title = "Nobel Prizes",
    tabPanel("Age and Money", ui1),
    tabPanel("Age distribution", ui2),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Data available on the website: 
                  <a class='text-dark' href='https://www.kaggle.com/imdevskp/nobel-prize'>www.kaggle.com</a>
                </p>
                </footer>
                "))

shinyApp(app_ui, server)
