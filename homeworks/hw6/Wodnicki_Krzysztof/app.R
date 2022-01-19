library(shiny)
library(shinyBS)
library(ggplot2)
library(dplyr)

full_data <- read.csv2("./complete.csv", sep=",") %>% 
  filter(gender!="")

filter_data <- full_data %>%
  group_by(birth_countryNow) %>%
  summarize(TotalAwards=n()) %>%
  right_join(full_data, by=c("birth_countryNow")) %>% 
  filter(TotalAwards >= 10)

ui <- fluidPage(

    titlePanel("Nobel Prize Award Winners by Country"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "category",
                        label = "Select cathegory:",
                        choices = unique(full_data$category)),
            textInput(inputId = "country",
                          label = "Highlight country:",
                          value = "",
                          placeholder = "Which country do you want to be marked?"),
            checkboxInput(inputId = "filter",
                          label = "Filter Countries",
                          value = FALSE),
            bsTooltip(id = "filter",
                      title = "Remove countries which have less total award winners than 10 from the plot",
                      placement = "bottom",
                      trigger = "hover")
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        if (input$filter)
          df <- filter_data %>% 
            filter(category == input$category)
        else 
          df <- full_data %>% 
            filter(category == input$category)
        ggplot(df, aes(x = birth_countryNow, fill = (birth_countryNow==input$country))) +
          geom_bar(color = "black") +
          theme(legend.position = "none") +
          scale_x_discrete(guide = guide_axis(angle=45)) +
          scale_fill_manual(values = c("#2d46b9", "#f037a5")) +
          labs(
            x = "Birth Country (Now)",
            y = "Award Winners Count"
          )
    })
}

shinyApp(ui = ui, server = server)
