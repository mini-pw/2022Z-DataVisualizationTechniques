library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)

source("scripts/prepare_data_filip.R")

df_jedrek <- read.csv("data/apps_j.csv", encoding="UTF-8")
df_filip <- read.csv("data/apps_f.csv", encoding="UTF-8")
df_malwina <- read.csv("data/apps_m.csv", encoding="UTF-8")

df_jedrek <- prepare_jedrek_df(df_jedrek)
df_filip <- prepare_filip_df(df_filip)
df_malwina <- prepare_malwina_df(df_malwina)

apps_jedrek <- df_jedrek %>% group_by(App.name) %>% summarise(sum = sum(duration_minutes)) %>% arrange(desc(sum)) %>% head(10)
apps_jedrek <- as.character(apps_jedrek$App.name)

apps_filip <- df_filip %>% group_by(App.name) %>% summarise(sum = sum(duration_minutes)) %>% arrange(desc(sum)) %>% head(10)
apps_filip <- as.character(apps_filip$App.name)

apps_malwina <- df_malwina %>% group_by(App.name) %>% summarise(sum = sum(duration_minutes)) %>% arrange(desc(sum)) %>% head(10)
apps_malwina <- as.character(apps_malwina$App.name)

apps_all <- unique(c(apps_malwina, apps_filip, apps_jedrek))

min_date <- max(c(min(df_jedrek$Data), min(df_filip$Data), min(df_malwina$Data)))
max_date <- min(c(max(df_jedrek$Data), max(df_filip$Data), max(df_malwina$Data)))
# min_date_str <- stri_datetime_format(min_date, stri_datetime_fstr("%d %B %Y"), locale="en_US")
# max_date_str <- stri_datetime_format(max_date, stri_datetime_fstr("%d %B %Y"), locale="en_US")

shinyUI(tagList(dashboardPage(
    skin = "red",
    dashboardHeader(title = "Phone activity data", 
                    tags$li(a(href = 'https://ww2.mini.pw.edu.pl/',
                              img(src = 'https://konkurs.mini.pw.edu.pl/sites/default/files/m_logo_120x120_2.png',
                                  title = "Logo MiNI", height = "40px"),
                              style = "padding-top:5px; padding-bottom:5px;"),
                            class = "dropdown")),
    
    
    dashboardSidebar(
        sidebarMenu(
          menuItem("Introduction", 
                   tabName = "intro", 
                   icon = icon("info-circle")),
            menuItem("Daytime activity",
                     tabName = "menu_daytime",
                     icon = icon("mobile-alt")
                     ),
            menuItem("Total time in apps",
                     tabName = "menu_radars",
                     icon = icon("mobile-alt")
            ),
            menuItem("Summary of 6 apps",
                     tabName = "menu_summary",
                     icon = icon("mobile-alt")
            ),
            menuItem("Change of average activity time",
                     tabName = "average_activity_change",
                     icon = icon("mobile-alt")
            ),
            menuItem("Average activity time in apps",
                     tabName = "average_activity",
                     icon = icon("mobile-alt")
            )
        )
    ),
    
    dashboardBody(
        tabItems(
          tabItem(tabName = "intro",
                  fluidPage(
                    fluidRow(
                      column(width = 2),
                      column(
                        width = 8,
                        h1("Welcome!", align = "center"),
                        h4("This app is created by Jędrzej Sokołowski, Filip Szympliński and Malwina Wojewoda.
                                   We are Data Science students at Warsaw Univeristy of Technology.
                                   This is our project for Data Visualization Techniques course.", align = "center"),
                        h4("In recent years there has been a massive upsurge in 
                                   social media usage. The more popular social media
                                   outlets get, the more time people will devote to them.
                                   Thus, we wanted to check how much time each of us is
                                   actually spending online and whether there is any 
                                   visible difference in social media consumption. 
                                   See our results by clicking on the tabs on the side", align = "center")
                      ),
                      column(width = 2)
                    ))),
            tabItem(tabName = "menu_daytime",
                    fluidPage(
                        h1("Daytime activity", align = "center"),
						h4("Here you can analyse phone activity of each person throughout the day.
						   You can choose a specific part of the day or simply showcase all
						   the activity on the selected date.", align = "center"),
                        br(),
                        
                        fluidRow(
                            column(
                                width = 3,
                                box(width = "100%", 
                                    background = 'red',
                                    selectInput(
                                        inputId = "pora_dnia",
                                        label = "Time of day:",
                                        choices = c("Entire day" = "all", "Evening" = "Wieczor", "Middle of the day" = "Srodek", "Morning" = "Rano"),
                                        selected = "Cały dzień"),
                                    dateInput(
                                        inputId = "Data",
                                        label = "Date:",
                                        format = "yyyy-mm-dd",
                                        value = "2021-12-11",
                                        min = min_date,
                                        max = max_date),
                                    selectInput(
                                        inputId = "osoba",
                                        label = "Whose data you want to see?",
                                        choices = c("Jędrek" = "j", "Filip" = "f", "Malwina" = "m"),
                                        selected = "Jędrek")
                                )
                            ), 
                            column(
                                width = 9,
                                box(width = "100%", 
                                    status = "danger", 
                                    solidHeader = TRUE,
                                    shinycssloaders::withSpinner(plotly::plotlyOutput("plot1"), type = 2, color.background = "#FF5454",
                                                                 color = "#FF5454")
                                )
                            ), 
                            br()
                        )
                    )
            ), 
            tabItem(tabName = "menu_radars",
                    fluidPage(
                        h1("Total time in applications", align = "center"),
                        h4("What you can see is an overview of our social media
                        usage in minutes. The graph is adjustable, you can pick 
                        specific people's data, apps and timespan you want to 
                        look into.", align = "center"),
                        fluidRow(column(10), 
                                 column(2, 
                                        selectInput(
                                            inputId = "typWykresu",
                                            label = "Select type of plot:",
                                            choices = c("Radar chart" = "radar", "Barplot" = "barplot"),
                                            selected = "Radar chart"
                                        )
                                 )),
                        fluidRow(column(1), 
                                 column(10, sliderInput("Date",
                                                        "Select range of dates:",
                                                        min = as.Date("2021-12-09","%Y-%m-%d"),
                                                        max = as.Date("2022-01-15","%Y-%m-%d"),
                                                        value=c(as.Date("2021-12-09"), as.Date("2022-01-15")),
                                                        timeFormat="%Y-%m-%d", 
                                                        width = "100%")),
                                 column(1)
                        ),
                        br(),
                        tags$style(HTML(".js-irs-0 .irs-bar {
                                        border-top-color: #d01010;
                                        border-bottom-color: #d01010;
                                        } 
                                        .js-irs-0 .irs-bar-edge {
                                        border-color: #d01010;
                                        }
                                        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                        background: #a00;
                                        }
                                        .js-irs-0 .irs-to,.js-irs-0 .irs-from {
                                        background: #a00}
                                        ")),
                        fluidRow(
                            column(4,
                                   box(width = "100%", 
                                       status = "danger", 
                                       solidHeader = TRUE,
                                       shinycssloaders::withSpinner(plotly::plotlyOutput("plotMalwina"), 
                                                                type = 2, 
                                                                color.background = "#FF5454",
                                                                color = "#FF5454")
                                   )
                            ), 
                            column(4,
                                   box(width = "100%", 
                                       status = "danger", 
                                       solidHeader = TRUE,
                                       shinycssloaders::withSpinner(plotly::plotlyOutput("plotJedrek"), 
                                                                type = 2, 
                                                                color.background = "#FF5454",
                                                                color = "#FF5454")
                                   )
                            ),
                            column(4, 
                                   box(width = "100%", 
                                       status = "danger", 
                                       solidHeader = TRUE,
                                       shinycssloaders::withSpinner(plotly::plotlyOutput("plotFilip"), 
                                                                type = 2, 
                                                                color.background = "#FF5454",
                                                                color = "#FF5454")
                                   )
                            )
                        ),
                        fluidRow(
                            column(2,
                                   box(width = "100%", 
                                       background = 'red',
                                       p("Select whose data you want to see:"),
                                       checkboxInput('Malwina', label = "Malwina", TRUE),
                                       checkboxInput('Jedrek', label = "Jędrek", TRUE),
                                       checkboxInput('Filip', label = "Filip", TRUE)
                                   )
                            ), 
                            column(8, align="center",
                                   box(width = "100%", 
                                       status = "danger", 
                                       solidHeader = TRUE,
                                       shinycssloaders::withSpinner(plotly::plotlyOutput("plotMain"), 
                                                                type = 2, 
                                                                color.background = "#FF5454",
                                                                color = "#FF5454")
                                   )
                            ), 
                            column(2, 
                                   box(width = "100%", 
                                       background = 'red',
                                       checkboxGroupInput("aplikacje", "Applications to show:",
                                                      choices = c("Messenger" = "Messenger",
                                                                  "Facebook" = "Facebook",
                                                                  "YouTube" = "YouTube", 
                                                                  "Instagram" = "Instagram",
                                                                  "Browser" = "Browser",
                                                                  "TikTok" = "TikTok", 
                                                                  "Netflix" = "Netflix", 
                                                                  "Messages" = "Messages", 
                                                                  "Phone" = "Phone", 
                                                                  "Spotify/Tidal" = "Spotify/Tidal"), 
                                                      selected = c("Messenger",
                                                                   "Facebook",
                                                                   "YouTube", 
                                                                   "Instagram",
                                                                   "Przegladarka",
                                                                   "TikTok", 
                                                                   "Netflix", 
                                                                   "Wiadomosci", 
                                                                   "Telefon")
                                   )
                                   )
                            )
                        )
                    )
            ),
            tabItem(tabName = "menu_summary",
                    fluidPage(
                      h1("Summary of 6 apps", align = "center"),
                      h4("In this section is shown a short summary of 6 applications
                      for every person. Those apps were chosen among
                      the most often used between 8 December 2021 and 15 January 2022.", align = "center"),
                      br(),
                      br(),
                      
                      fluidRow(
                        column(
                          width = 4,
                          offset = 0,
                      box(width = "80%", 
                          background = 'red',
                      radioButtons(
                        "personInput",
                        "Select whose data you want to see:",
                        c("Filip", "Jędrek", "Malwina"),
                        selected = "Filip"
                      )))),

                      # First row of boxes
                      fluidRow(
                        column(
                          width = 4,
                          offset = 0,
                          box(
                            status="danger",
                            width = 12,
                            h4(textOutput("box1app")),
                            shinycssloaders::withSpinner(
                              imageOutput("box1image", height = "110px"),
                              type = 2, color.background = "#FF5454",
                              color = "#FF5454"),
                            tableOutput("box1table")
                          )),

                        column(
                          width = 4,
                          offset = 0,
                          box(
                            status="danger",
                            width = 12,
                            h4(textOutput("box2app")),
                            shinycssloaders::withSpinner(
                              imageOutput("box2image", height = "110px"),
                              type = 2, color.background = "#FF5454",
                              color = "#FF5454"),
                            tableOutput("box2table")
                          )),

                        column(
                          width = 4,
                          offset = 0,
                          box(
                            status="danger",
                            width = 12,
                            h4(textOutput("box3app")),
                            shinycssloaders::withSpinner(
                              imageOutput("box3image", height = "110px"),
                              type = 2, color.background = "#FF5454",
                              color = "#FF5454"),
                            tableOutput("box3table")
                          ))
                      ),

                      # Second row of boxes
                      fluidRow(
                        column(
                          width = 4,
                          offset = 0,
                          box(
                            status="danger",
                            width = 12,
                            h4(textOutput("box4app")),
                            shinycssloaders::withSpinner(
                              imageOutput("box4image", height = "110px"),
                              type = 2, color.background = "#FF5454",
                              color = "#FF5454"),
                            tableOutput("box4table")
                          )),

                        column(
                          width = 4,
                          offset = 0,
                          box(
                            status="danger",
                            width = 12,
                            h4(textOutput("box5app")),
                            shinycssloaders::withSpinner(
                              imageOutput("box5image", height = "110px"),
                              type = 2, color.background = "#FF5454",
                              color = "#FF5454"),
                            tableOutput("box5table")
                          )),

                        column(
                          width = 4,
                          offset = 0,
                          box(
                            status="danger",
                            width = 12,
                            h4(textOutput("box6app")),
                            shinycssloaders::withSpinner(
                              imageOutput("box6image", height = "110px"),
                              type = 2, color.background = "#FF5454",
                              color = "#FF5454"),
                            tableOutput("box6table")
                          ))
                      )
                    )
            ),
            tabItem(tabName = "average_activity_change",
                    fluidPage(
                      h1("Change of average activity time on app in single use for every day",
                                 align = "center"),
                      h4("Here you can see, how does change an average activity  time
                      for single use by person. You can choose from 20 most popular
                         apps.", align = "center"),
                      br(),
                      br(),
                      fluidRow(
                        column(width = 2,
                               box(
                                 width = "100%",
                                 background = 'red',
                               selectInput(
                                 "appsInput",
                                 "Type or choose apps' names. To delete app, select by clicking it and press Delete.",
                                 apps_all,
                                 multiple = TRUE,
                                 selected = "YouTube"
                               ),
                               radioButtons(
                                 "personInput2",
                                 "Select whose data you want to see:",
                                 c("Malwina", "Jędrek", "Filip"),
                                 selected = "Filip"
                               )))
                      ,
                        column(width = 10,
                               box(width = "100%",
                                   status = "danger", 
                                   solidHeader = TRUE,
                               shinycssloaders::withSpinner(plotly::plotlyOutput("average_activity_change"), type = 2, color.background = "#FF5454",
                                                            color = "#FF5454")),
                               h5("Click double the plot in order to change it's size to original.", algin = "rigth"))
                      )
                      
                      )),
            tabItem(tabName = "average_activity",
                    fluidPage(
                      tags$style(HTML(".js-irs-1 .irs-bar {
                                        border-top-color: #d01010;
                                        border-bottom-color: #d01010;
                                        } 
                                        .js-irs-1 .irs-bar-edge {
                                        border-color: #d01010;
                                        }
                                        .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
                                        background: #a00;
                                        }
                                        .js-irs-1 .irs-to,.js-irs-1 .irs-from {
                                        background: #a00}
                                        ")),
                      h1("Average activity time on apps in single use",
                         align = "center"),
                      h4("On this plot you can see, how an average activity time
                         in single use. Change a number of showing apps fro every person
                         and interval of dates to observe how does an activity change", align = "center"),
                      br(),
                      br(),
                      fluidRow(
                        column(width = 2,
                               box(width = "100%",
                                   background = 'red',
                               selectInput("appsNoInput",
                                           "Select how many apps do you want to see for one person:",
                                           choices = c(2, 3, 4, 5, 6),
                                           selected = 4))
                      ),
                      column(width = 10,
                             box(width = "100%",
                                 status = "danger", 
                                 solidHeader = TRUE,
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("average_activity"), type = 2, color.background = "#FF5454",
                                                              color = "#FF5454")),
                             h5("Click double the plot in order to change it's size to original.", algin = "rigth"),
                             br(),
                             h4("Select range of dates", align = "center"),
                                sliderInput(inputId = "date_interval",
                                                      label = NULL,
                                                      min = min_date,
                                                      max = max_date,
                                                      value = c(min_date, max_date),
                                                      width = "100%")))
                      ),
                  )
            )
        )
    ),
    tags$footer("Jędrzej Sokołowski, Filip Szympliński, Malwina Wojewoda", align = "center", style = "
              position:fixed;
              bottom:0;
              width: 100%;
              height:20px;
              color: white;
              padding: 1px;
              background-color: #DD4C39;
              left: 0px;
              z-index: 800;")  
))
