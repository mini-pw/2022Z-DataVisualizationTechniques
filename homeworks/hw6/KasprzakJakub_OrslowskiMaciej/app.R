library(shiny)
library(tidyverse)

path_to_nobel_df <- "data/complete.csv"  # change me
nobel_df <- read_csv(path_to_nobel_df)

genders <- nobel_df %>% 
    filter(!is.na(gender)) %>% 
    select(awardYear, category, gender) %>% 
    mutate(awards_male = ifelse(gender == "male", 1, 0),
           awards_female = ifelse(gender == "female", 1, 0))

countries <- 
    nobel_df %>% 
    select("birth_country") %>% 
    filter(birth_country != "") %>% 
    group_by(birth_country) %>% 
    count() %>% 
    arrange(n) %>% 
    filter(n >= 3)

aggregate_genders_df <- function(awardCategory) {
    if (awardCategory != "all") {
        df2 <- genders %>% 
            filter(category == awardCategory)
    } else {
        df2 <- genders
    }
    df2 %>% 
        group_by(awardYear) %>%
        summarise(awards_male = sum(awards_male),
                  awards_female = sum(awards_female)) %>% 
        mutate(awards_male = cumsum(awards_male),
               awards_female = cumsum(awards_female))
}

wrap_with_spinner <- function(obj) {
    shinycssloaders::withSpinner(
        obj,
        type = 3, color = "#3E3F3A",
        color.background = "white"
    )
}

server <- function(input, output) {

    output$genders_plot <- plotly::renderPlotly({
        df <- aggregate_genders_df(input$category) %>% 
            rename(Male = awards_male, Female = awards_female) %>% 
            pivot_longer(!awardYear,
                         names_to = "gender", 
                         values_to = "total_awards")
        plotly::plot_ly(
            data = df,
            x = ~gender,
            y = ~total_awards,
            frame = ~awardYear,
            type = "bar",
            marker = list(color = "#3E3F3A")
        ) %>% plotly::layout(
            xaxis = list(
                title = ""
            ),
            yaxis = list(
                title = "Total number of awards"
            )
        ) %>% plotly::config(displayModeBar = FALSE) %>% 
            plotly::animation_slider(
                currentvalue = list(
                    prefix = "Year: ",
                    font = list(color = "black")
                )
            )
    })
    
    output$genders_table <- DT::renderDataTable({
        aggregate_genders_df(input$category) %>% 
            rename(Year = awardYear,
                   `Total number of awards for women` = awards_female,
                   `Total number of awards for men` = awards_male)
    })
    
    output$countries_plot <- renderPlot({
        
        df <- countries %>%
            filter(n >= input$range[1] & n <= input$range[2])
        
        ggplot(df, aes(x = birth_country, y = n)) +
            geom_col(fill = "#3E3F3A") +
            geom_text(aes(label = n), vjust = -0.5) +
            scale_x_discrete(guide = guide_axis(title = "Country name", angle = 90)) +
            ylab("Number of awards won") +
            theme_bw(base_size = 14)
    })
    
    output$countries_table <- DT::renderDataTable({
        df <- countries %>%
            filter(n >= input$range[1] & n <= input$range[2]) %>% 
            arrange(-n) %>% 
            rename(`Number of awards won` = n,
                   `Country name` = birth_country)
    })
}

ui1 <- fluidPage(
    titlePanel("Total number of Nobel Prize Awards by gender"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "category",
                label = "Category",
                choices = c("all", nobel_df$category)
            )
        ),
        mainPanel(
            wrap_with_spinner(
                plotly::plotlyOutput("genders_plot")
            ),
            markdown('# Table'),
            wrap_with_spinner(
                DT::dataTableOutput('genders_table')
            )
        )
    )
)

ui2 <- fluidPage(
    # zmiana koloru slidera
    tags$style(type = "text/css", HTML("
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
            background: #3E3F3A;
            border-top: 1px solid #3E3F3A ;
            border-bottom: 1px solid #3E3F3A ;
        }")),
    
    titlePanel("An interactive graph: which countries' citizens won a number of Nobel awards in a set range?"),
    
    sidebarLayout(
        sidebarPanel(
            shinyWidgets::sliderTextInput("range",
                                          "Range of the number of awards won:",
                                          choices = unique(countries$n),
                                          selected = c(11, 77)
            )
        ),
        
        mainPanel(
            wrap_with_spinner(
                plotOutput("countries_plot")
            ),
            markdown('# Table'),
            wrap_with_spinner(
                DT::dataTableOutput("countries_table")
            )
        )
    )
)

ui_final <- navbarPage(
    title = "Data analysis: Nobel Prizes",
    tabPanel("Awards by gender", ui1),
    tabPanel("Awards by country", ui2),
    theme = bslib::bs_theme(bootswatch = "sandstone")
)

shinyApp(ui_final, server)
