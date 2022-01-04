library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(rsconnect)
df <- read.csv("./Data/complete.csv") %>%
    na.omit() %>%
    select(where(is.numeric))
df2 <- read.csv("./Data/complete.csv") %>%
    na.omit() %>%
    arrange(birth_country)
ui2 <- fluidPage(
    titlePanel("Winners by category and country"),
    sidebarLayout(
        sidebarPanel(
            sliderTextInput("country","Choose country",choices = unique(df2$birth_country),selected = "Poland"),
            selectInput("category","Choose category",choices = c("all",unique(df2$category)),selected = "all")
        ),
        mainPanel(
            plotlyOutput("countries"),
        )
        
    )
)
ui1 <- fluidPage(
    titlePanel('Various plots about Nobel prize winners'),
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "inputTitle", label = "Plot title:", value = "Nobel prize visualization"),
            varSelectInput(inputId = "dropdownX", label = "X-axis", data = df),
            varSelectInput(inputId = "dropdownY", label = "Y-axis", data = df)
        ),
        mainPanel(
            plotlyOutput(outputId = "chart")
        )
    )
)
shinyUI(
    navbarPage(title = "Nobel prizes analysis", tabPanel("Various plots about numerical data",ui1), tabPanel("Winners by category and country",ui2))
    )
