#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)

nobel = read.csv("nobel.csv")

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        skin = "black",
    dashboardHeader(title = "Nobel4me"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Top Nobel Awards", tabName = "topAwards", icon = icon("trophy")),
            menuItem("Nobel Award Trend", tabName = "awardTrend", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
       .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
        tabItems(
            # First tab content
            tabItem(tabName = "topAwards",
                    h2("Top Nobel Awards"),
                    fluidRow(
                        column(
                            width = 6,
                            box(selectInput("categories",
                                            label = "Choose category:",
                                            sort(unique(nobel$category))),
                                radioButtons("buttons1",
                                             "Display real values?",
                                             choices = c("No","Yes")),
                                width = NULL,
                                status = "danger"),
                            box(plotOutput("fig1"),
                                width = NULL,
                                status = "warning",
                                title = "Top 5 countries",
                                solidHeader = TRUE),
                            box(title = "Description",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,
                                "In this section you are able to check top 5 countries with the biggest number
                                of Nobel awards in each category. By choosing 'Yes' in display option you will
                                see exact values for each column.")
                        ),
                        column(
                            width = 6,
                            box(selectInput("countries",
                                            label = "Choose country:",
                                            sort(unique(nobel$birth_countryNow))[2:length(unique(nobel$birth_countryNow))]),
                                radioButtons("buttons2",
                                             "Display real values?",
                                             choices = c("No","Yes")),
                                width = NULL,
                                status = "danger"),
                            box(plotOutput("fig2"),
                                width = NULL,
                                status = "warning",
                                title = "Awards by country",
                                solidHeader = TRUE),
                            box(title = "Description",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,
                                "In this section you are able to check how many and of which categories chosen
                                country has Nobel awards. Once again, By choosing 'Yes' in display option you will
                                see exact values for each column.")
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "awardTrend",
                    h2("Nobel Award Trend"),
                    fluidRow(
                        column(width = 12,
                            box(plotlyOutput("fig3"),
                                width = NULL,
                                status = "warning",
                                title = "Worldwide Nobel Award Trend",
                                solidHeader = TRUE),
                            box(title = "Description",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,"In this section you are able to check trends of giving 
                                the Nobel awards away. If you want to know specific value of exact point on the plot 
                                just point it with cursor. You can start the animation with 'Play' button or use the slider.
                                WARNING: It takes a while for the plot to load itself, be patient.")
                        ))
                    )
            )
        )
    )
)
