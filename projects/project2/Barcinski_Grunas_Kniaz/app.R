library(dashboardthemes)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(plotly)
library(tidyr)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(shinydashboard)
library(shinyWidgets)
library(ggtext)
library(scales)

####################### loading data ########################
df_plot1 <- read.csv("dane_obrobione/df_plot1.csv", na.strings=c("","NA"))
df_plot2 <- read.csv("dane_obrobione/df_plot2.csv", na.strings=c("","NA"))
df_0 <- read.csv("dane_obrobione/df_0.csv", na.strings=c("","NA"))
df_ruchygracza <- read.csv("dane_obrobione/df_ruchygracza.csv", na.strings=c("","NA"))
openingi <- read.csv("dane_obrobione/openingi.csv", na.strings=c("","NA"))
df_heatmap<- read.csv("dane_obrobione/df_heatmap.csv", na.strings=c("","NA"))
df_scatter<- read.csv("dane_obrobione/df_scatter.csv", na.strings=c("","NA"))

## custom_theme ######
customTheme <- shinyDashboardThemeDIY(
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "#2D2D2D"
    ,primaryFontColor = "#0F0F0F"
    ,infoFontColor = "#0F0F0F"
    ,successFontColor = "#0F0F0F"
    ,warningFontColor = "#0F0F0F"
    ,dangerFontColor = "#0F0F0F"
    ,bodyBackColor = "#D6D6D6"
    
    ### header
    ,logoBackColor = "#0A0808"
    
    ,headerButtonBackColor = "#0A0808"
    ,headerButtonIconColor = "#FFFFFF"
    ,headerButtonBackColorHover = "#FFFFFF"
    ,headerButtonIconColorHover = "#0A0808"
    
    ,headerBackColor = "#0A0808"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ### sidebar
    ,sidebarBackColor = "#B5B3B3"
    ,sidebarPadding = "12"
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = "0"
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"
    
    ,sidebarUserTextColor = "#000000"
    
    ,sidebarSearchBackColor = "#FFFAFA"
    ,sidebarSearchIconColor = "#E61C1C"
    ,sidebarSearchBorderColor = "#F57208"
    
    ,sidebarTabTextColor = "#000000"
    ,sidebarTabTextSize = "15"
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = "0"
    
    ,sidebarTabBackColorSelected = "#FFFFFF"
    ,sidebarTabTextColorSelected = "#000000"
    ,sidebarTabRadiusSelected = "0px"
    
    ,sidebarTabBackColorHover = "#FFFFFF"
    ,sidebarTabTextColorHover = "#000000"
    ,sidebarTabBorderStyleHover = "none solid none none"
    ,sidebarTabBorderColorHover = "#C8C8C8"
    ,sidebarTabBorderWidthHover = "9"
    ,sidebarTabRadiusHover = "0px"
    
    ### boxes
    ,boxBackColor = "#FFFFFF"
    ,boxBorderRadius = "5"
    ,boxShadowSize = "none"
    ,boxShadowColor = ""
    ,boxTitleSize = "18"
    ,boxDefaultColor = "#E1E1E1"
    ,boxPrimaryColor = "#5F9BD5"
    ,boxInfoColor = "#B4B4B4"
    ,boxSuccessColor = "#70AD47"
    ,boxWarningColor = "#ED7D31"
    ,boxDangerColor = "#E84C22"
    
    ,tabBoxTabColor = "#F8F8F8"
    ,tabBoxTabTextSize = "14"
    ,tabBoxTabTextColor = "#000000"
    ,tabBoxTabTextColorSelected = "#2D2D2D"
    ,tabBoxBackColor = "#F8F8F8"
    ,tabBoxHighlightColor = "#C8C8C8"
    ,tabBoxBorderRadius = "5"
    
    ### inputs
    ,buttonBackColor = "#A8A8A8"
    ,buttonTextColor = "#2D2D2D"
    ,buttonBorderColor = "#969696"
    ,buttonBorderRadius = "5"
    
    ,buttonBackColorHover = "#BEBEBE"
    ,buttonTextColorHover = "#000000"
    ,buttonBorderColorHover = "#969696"
    
    ,textboxBackColor = "#FFFFFF"
    ,textboxBorderColor = "#767676"
    ,textboxBorderRadius = "5"
    ,textboxBackColorSelect = "#A8A8A8"
    ,textboxBorderColorSelect = "#6C6C6C"
    
    ### tables
    ,tableBackColor = "#F8F8F8"
    ,tableBorderColor = "#EEEEEE"
    ,tableBorderTopSize = "1"
    ,tableBorderRowSize = "1"
)


# sidebar ###################
sidebar <- dashboardSidebar(
    sidebarMenu(
        id="menu1",
        menuItem("Introduction", tabName = "introduction", icon = icon("cloudsmith")),
        menuItem("Player comparison", tabName = "player_comparison", icon = icon("people-arrows")),
        conditionalPanel(
            condition = "input.menu1 == 'player_comparison'",
            prettyRadioButtons(
                inputId = "rodzaj",
                label = "Time control:",
                choices = c("bullet", "blitz", "rapid"),
                animation = "smooth",
                selected = "blitz",
                icon = icon("stopwatch"))
        ),
        menuItem("Pawns and pieces", tabName = "pawns_pieces", icon = icon("chart-bar")),
        menuItem("Heatmap - time", tabName = "Heatmap_time", icon = icon("chart-bar")),
        menuItem("Scatter plot elo", tabName = "scatter_elo", icon = icon("chart-bar")),
        menuItem("Openings", tabName = "openings", icon = icon("chart-bar")),
        conditionalPanel(
            condition = ("input.menu1 == 'pawns_pieces' | input.menu1 == 'Heatmap_time' |
                   input.menu1 == 'scatter_elo' | input.menu1 == 'openings'"),
            prettyRadioButtons(
                inputId = "rodzaj_2",
                label = "Time control:",
                choices = c("bullet", "blitz", "rapid"),
                animation = "smooth",
                selected = "bullet",
                icon = icon("stopwatch")
            ),
            prettyRadioButtons(
                inputId = "gracz",
                label = "Player:",
                choices = c("grooney", "Mlodziak77", "tymekkk"),
                icon = icon("user"), 
                animation = "smooth",
                selected = "grooney"
            )),
        conditionalPanel(
            condition = ("input.menu1 == 'pawns_pieces' | input.menu1 == 'Heatmap_time'"),
            prettyRadioButtons(
                inputId = "color",
                label = "Color:",
                choices = c("white", "black", "both"),
                icon = icon("chess-pawn"), 
                animation = "smooth",
                selected = "white"
            )
        ),
        conditionalPanel(
            condition = ("input.menu1 == 'scatter_elo' | input.menu1 == 'openings'"),
            prettyRadioButtons(
                inputId = "color_2",
                label = "Color:",
                choices = c("white", "black"),
                icon = icon("chess-pawn"), 
                animation = "smooth",
                selected = "white"
            )
        )
    ))


# tags$script("$(\"input:radio[name='rodzaj_2'][value='bullet']\").parent().css('icon', 'stopwatch');"),
# tags$script("$(\"input:radio[name='rodzaj_2'][value='blitz']\").parent().css('icon', 'cloudsmith');"),
# tags$script("$(\"input:radio[name='rodzaj_2'][value='rapid']\").parent().css('icon', 'stopwatch');"),

# footer #############
footer <- '<footer class="page-footer font-small blue">
              <!-- Copyright -->
              <div class="footer-copyright text-center py-3"> 2022 Copyright &emsp;
              <a href="https://github.com/grooney39">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <span style="font-weight: bold">Jakub Grunas<span/>
              <a href="https://lichess.org/@/grooney">
                <img src="https://images.prismic.io/lichess/5cfd2630-2a8f-4fa9-8f78-04c2d9f0e5fe_lichess-box-1024.png?auto=compress,format" alt="lichess logo" width = "1.5%" height = "1.5%">
              </a>
              &emsp;
              <a href="https://github.com/arekkn">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <span style="font-weight: bold">Arkadiusz Kniaz<span/>
              <a href="https://lichess.org/@/Mlodziak77">
                <img src="https://images.prismic.io/lichess/5cfd2630-2a8f-4fa9-8f78-04c2d9f0e5fe_lichess-box-1024.png?auto=compress,format" alt="lichess logo" width = "1.5%" height = "1.5%">
              </a> &emsp;
              <a href="https://github.com/tbarcinski">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <span style="font-weight: bold">Tymoteusz Barcinski<span/>
              <a href="https://www.chess.com/member/tymekkk">
                <img src="https://images.chesscomfiles.com/uploads/v1/images_users/tiny_mce/SamCopeland/phpmeXx6V.png" alt="chess.com logo" width = "1.5%" height = "1.5%">
              </a>
              </div>
              <!-- Copyright -->
              </footer>'


# introduction ###############
introduction_tabitem <- tabItem(tabName = 'introduction', fluidPage(
    fluidRow(box(
        shiny::HTML(
            '<p style = "font-size:16px"> The dashboard was created as a team project for the Data Visualization Techniques course at the Faculty of Mathematics and Information Systems at &nbsp;Warsaw University of Technology. We were given the task to visualize data about ourselves. Since all three members of our team are avid chess players, we decided to use this opportunity to combine our studies with pleasure and get an insightful analysis of our chess style and performance throughout the years.</p>
      <p style = "font-size:16px">The dashboard is divided into two main parts:</p>
      <ul style = "font-size:16px">
        <li>the first, smaller part contains two plots designed to compare our strength and style</li>
        <li>the second part is divided into four smaller sections, each one aimed at visualizing different aspect of our chess journeys without comparisons with one another. While analyzing this part you can select particular players, time formats and colors we played displayed on the sidebar on the left side.</li>
      </ul>
      <p style = "font-size:16px">The data that was used to create the app come from lichess.org and chess.com archives.</p>
      <p style = "font-size:16px">Our team members are:</p>
      <ul style = "font-size:16px">
      <li>Jakub Grunas (grooney)</li>
      <li>Arkadiusz Kniaz (Mlodziak77)</li>
      <li>Tymoteusz Barcinski (tymekkk)</li>
      </ul>
      <p style = "font-size:16px">You can visit our github and chess.com/lichess profiles clicking on the relevant icon in the footer.</p>'
        ), width = 7),
        box(shiny::HTML(
            '<img src="https://raw.githubusercontent.com/arekkn/test/main/ChessBoard.png" alt="chess.com logo" width = "100%" height = "100%">'
        ), width = 5)),
    fluidRow(
        box(width = 12, shiny::HTML(
            footer
        )))
))



# player_comparison ######
player_comparison <- tabItem(tabName = 'player_comparison', fluidPage(
    fluidRow(box("The plot below shows our rating progress since the day we started playing.
                 Each color represents one of us. You can hover over the lines to see the exact rating at the exact time.
                 As you can clearly see, we all started playing at different times and one of us plays only blitz.
                 All of us are also rather successful and gradually getting better!", width = 6),
             box("The second plot visualizes our opening choices and success rate. Each point represents a different opening played by a particular player.
                 Interestingly, the more we play a certain opening, the closer our average score is to 0.5 points.", width = 6)),
    fluidRow(box(shinycssloaders::withSpinner(plotlyOutput("plot1_k"), color = "#b48864ff", type = 5, color.background = 'white'), width = 6),
             box(shinycssloaders::withSpinner(plotlyOutput("scatter_openings"), color = "#b48864ff", type = 5, color.background = 'white'), width = 6)),
    fluidRow(box(sliderInput("dates",
                             label = NULL,
                             min = as.Date("30.03.2017", format = "%d.%m.%Y"),
                             max = as.Date("15.12.2021", format = "%d.%m.%Y"),
                             value = c(as.Date("29.11.2018", format = "%d.%m.%Y"), as.Date("15.12.2021", format = "%d.%m.%Y")),
                             timeFormat = "%m/%Y"), width = 6, height = "6em"),
             box(radioGroupButtons(
                 inputId = "color_3",
                 choices = c("white", "black"),
                 justified = TRUE,
                 selected = "white"
             ),
             tags$script("$(\"input:radio[name='color_3'][value='white']\").parent().css('background-color', '#ffffff', 'text-color', '#000000');"),
             tags$script("$(\"input:radio[name='color_3'][value='blacks']\").parent().css('background-color', '#000000', 'text-color', '#ffffff');"),
             width = 6, height = "6em")
    ),
    fluidRow(
        box(width = 12, shiny::HTML(
            footer
        )))
))



# pawns_pieces_tabitem #######
pawns_pieces_tabitem <-  tabItem(tabName = 'pawns_pieces',
                                 fluidPage(
                                     fluidRow(
                                         box("In this section, we wanted to visualize how often we move each of our pawns and pieces.
                                         Later we decided to extend the plots to captures (for both pawns and pieces) and promotions
                                         (just for pawns for obvious reasons). The plot on the left represents pawns and on the right ? pieces).
                                         Some interesting insights from the plots: central pawns move and capture most often but promote rarely,
                                         contrary to flank pawns. Unsurprisingly, the Queen captures most often and surprisingly (at least for us)
                                         the King is the most active piece, probably due to endgame.", width = 12)
                                     ),
                                     fluidRow(
                                         box(
                                             shinycssloaders::withSpinner(plotlyOutput("plot2_K_Pawns"), color = "#b48864ff",
                                                                          type = 5, color.background = 'white'),
                                             width = 6),
                                         box(
                                             shinycssloaders::withSpinner(plotlyOutput("plot2_K_Pieces"), color = "#b48864ff",
                                                                          type = 5, color.background = 'white'),
                                             width = 6)
                                     ),
                                     fluidRow(
                                         box(selectInput("typ1", "Pawns:",
                                                         c("Total number of moves per pawn" = "Pawns_moves",
                                                           "Total number of captures per pawn" = "Pawns_captures",
                                                           "Total number of promotions per pawn" = "Pawns_promotions"))
                                             ,width = 6),
                                         box(selectInput("typ2", "Pieces", c("Total number of moves per piece" = "Pieces_moves",
                                                                             "Total number of captures per piece" = "Pieces_captures")),
                                             width = 6)
                                     ),
                                     fluidRow(
                                         box(width = 12, shiny::HTML(
                                             footer
                                         )))
                                 )
)






# heatmap_time_tabitem ###########
heatmap_time_tabitem <- tabItem(tabName = 'Heatmap_time',
                                fluidRow(box(
                                    shinycssloaders::withSpinner(plotlyOutput("heatmap_time", height = "500px"), color = "#b48864ff",
                                                                 type = 5, color.background = 'white'), width = 12)),
                                fluidRow(box(selectInput(inputId = "category",
                                                         label = "Category:",
                                                         choices = c("Total number of games played" = "n",
                                                                     "Average score" = "mean")), width = 6),
                                         box("As the plot title states, we visualized the times of the day and the week of our chess activity and results.
                                                 You can switch between the players and time formats to discover how the data differ. ", width = 6)),
                                fluidRow(
                                    box(width = 12, shiny::HTML(
                                        footer
                                    ))))


# scatter_elo_tabitem #############
scatter_elo_tabitem <- tabItem(tabName = 'scatter_elo',
                               fluidPage(
                                   fluidRow(
                                       box("The scatter plot shows how we performed against opponents that are higher and lower rated than us.
                                       Each color represents a differnt game result (win, draw, loss). Before creating the plot,
                                       we assumed that blue points (wins) would be rather higher than the red points (losses) and it seems to be the case.
                                       On top of that, we added a trend line for each result to make the data easier to read. ", width = 12)
                                   ),
                                   fluidRow(
                                       box(
                                           shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height = "500px"), color = "#b48864ff",
                                                                        type = 5, color.background = 'white'), width = 12
                                       )
                                   ),
                                   fluidRow(
                                       box(sliderInput("dates_2",
                                                       label = NULL,
                                                       min = as.Date("30.03.2014", format = "%d.%m.%Y"),
                                                       max = as.Date("15.12.2021", format = "%d.%m.%Y"),
                                                       value = c(as.Date("29.11.2018", format = "%d.%m.%Y"), as.Date("15.12.2021", format = "%d.%m.%Y")),
                                                       timeFormat = "%m/%Y"),
                                           width = 12, height = "6em")
                                   ),
                                   fluidRow(
                                       box(width = 12, shiny::HTML(
                                           footer
                                       )))
                               ))



# openings_tabitem #################
openings_tabitem <- tabItem(tabName = 'openings',
                            fluidPage(
                                fluidRow(
                                    box(
                                        shinycssloaders::withSpinner(plotlyOutput("heatmap_openings", height = 600),
                                                                     color = "#b48864ff", type = 5, color.background = 'white'), 
                                        width = 6),
                                    box(uiOutput("gif")), width = 6)
                            ),
                            fluidRow(
                                box(selectInput(
                                    inputId = "dane_heatmap",
                                    label = "Dane heatmap:",
                                    choices = openingi
                                ), width = 6),
                                box("This section shows where do we move our pieces in the games which start with different openings.
                We uploaded the first moves of the given opening at the right side of the heatmap so that you can check,
                whether the data match the moves.", width = 6)
                            ),
                            fluidRow(
                                box(width = 12, shiny::HTML(
                                    footer
                                )))
)


# body ######
body <- dashboardBody(
    customTheme,
    tabItems(
        introduction_tabitem,
        player_comparison,
        pawns_pieces_tabitem,
        heatmap_time_tabitem,
        scatter_elo_tabitem,
        openings_tabitem)
)



# UI #####
ui <- dashboardPage(
    dashboardHeader(title = span(tagList(icon("chess-pawn"), "   Chess Analysis   ", icon("chess-pawn")))),
    sidebar,
    body
)
# server####
server <- function(input, output, session) {
    
    output$gif <- renderUI({
        serce <- case_when((input$dane_heatmap=="Caro Kann"|input$dane_heatmap=="Caro-Kann Defense") ~ '<img src="https://raw.githubusercontent.com/arekkn/test/main/caro_kann_white.gif"  width = "100%" height = "100%">' ,
                           input$dane_heatmap=="English Opening"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/english.gif"  width = "100%" height = "100%">'  ,
                           (input$dane_heatmap=="Four Knights Game"| input$dane_heatmap=="Four Knights")  ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/four_knights_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="French Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/french_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Giuoco Piano"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/giuoco_piano_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Grunfeld Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/grunfeld.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Indian Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/indian.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Italian Game"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/italian_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Modern Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/modern.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Philidor Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/philidor_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Pirc Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/pirc.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Queen's Gambit Declined"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/qgd.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Queen's Pawn Game"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/queens_pawn_game.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Russian Game"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/russian.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Ruy Lopez"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/ruy_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Scandinavian Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/scandi_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Sicilian Defense"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/sicilian_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Three Knights Opening"   ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/three_knights_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Zukertort Opening"     ~  '<img src="https://raw.githubusercontent.com/arekkn/test/main/zukerkort_white.gif"  width = "100%" height = "100%">'  ,
                           TRUE ~ '<img src="zukerkort_white.gif"  width = "100%" height = "100%">')
        shiny::HTML(serce)
    })
    
    text = paste("No data")
    emptyPlot <- ggplot() + 
        annotate(geom = "text",
                 x = 4,
                 y = 25,
                 size=8,
                 label = text,
        ) + 
        theme_void()
    emptyPlot <- ggplotly(emptyPlot)
    
    output$plot1_k <- renderPlotly({
        
        df <- df_plot1%>%
            mutate(date= as.Date(date))%>%
            filter(rodzaj==input$rodzaj) %>%
            filter(date >= input$dates[1] & date <= input$dates[2])
        p <- ggplot(df, aes(x = date)) +
            geom_line(aes(y = grooney), color = "#3f9428") +
            geom_line(aes(y = Mlodziak77), color = "#283f94") +
            geom_line(aes(y = tymekkk), color = "#94283f") +
            labs(y = "Player rating", x = element_blank(),
                 title = "Players rating across time")  + theme_bw()
        ggplotly(p)
    })
    
    output$heatmap_time <- renderPlotly({
        
        if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
            fig <- emptyPlot
        }
        else{
            df_heatmap_wynik <- df_heatmap %>%
                filter(color==input$color, gracz==input$gracz, rodzaj==input$rodzaj_2) %>%
                select(day, hour, input$category) %>%
                pivot_wider(names_from = hour, values_from = input$category) %>%
                ungroup() %>%
                arrange(day) %>%
                select(-day)
            df_heatmap_wynik <- df_heatmap_wynik[, as.character(c(0:23))]
            
            fig <- plot_ly(
                x = c(0:23),
                y = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"),
                z = as.matrix(df_heatmap_wynik),
                type = "heatmap",
                colors = colorRamp(c("#543005", "#f6e8c3"))) %>%
                layout(
                    title = "When do we play most and, more importantly, when do we play best?",
                    xaxis = list(
                        dtick = 1,
                        tick0 = 0,
                        tickmode = "linear"
                    ))
            
        }
        
        fig
        
    })
    
    output$plot2_K_Pieces <- renderPlotly({
        
        
        if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
            fig <- emptyPlot
        }
        else{
            df<-df_plot2 %>% 
                filter(rodzaj==input$rodzaj_2, gracz==input$gracz, typ==input$typ2, 
                       color==input$color)
            
            df$nazwa <- factor(df$nazwa, levels = c("R", "N", "B", "K", "Q", "B ", "N ", "R ")) 
            
            
            fig <- ggplot(df, aes(x = nazwa, y = n)) + geom_col(fill = "#b48864ff") +
                scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                labs(title = "Pieces", x = element_blank(), y = "Total number")  +
                theme_bw()
            
            fig <- ggplotly(fig)
        }
        fig
    })
    
    output$plot2_K_Pawns <- renderPlotly({
        
        if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
            fig <- emptyPlot
        }
        else{
            df<-df_plot2 %>% 
                filter(rodzaj==input$rodzaj_2, gracz==input$gracz,
                       typ==input$typ1,color==input$color)
            
            
            fig <- ggplot(df, aes(x = nazwa, y = n)) + geom_col(fill = "#b48864ff") +
                scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                labs(title = "Pawns from different lines", x = element_blank(), y = "Total number") +
                theme_bw()
            
            fig <- ggplotly(fig)
        }
        fig
        
    })
    
    output$scatter_openings <- renderPlotly({
        
        df_0 <- df_0 %>% 
            filter(rodzaj==input$rodzaj, color == input$color_3, n >=2)
        
        colnames(df_0)[c(3)] <- "number"
        
        p_0<-ggplot(df_0,mapping = aes(x=number,y=avg_points,color=gracz, opening = opening_name))+
            geom_jitter(width = 0.05, height = 0.01)+ 
            scale_x_continuous(trans = 'log2',breaks = c(2,4,8,16,32,62,128, 256,512,1024)) +
            labs(x = "Number of games (logaritmic scale)", y = "Average of points",
                 title = "Average score vs. number of games across openings") +
            scale_color_manual(values = c("#3f9428", "#283f94", "#94283f")) + theme_bw()
        
        ggplotly(p_0, tooltip = c("number", "avg_points", "color", "opening")) %>% 
            layout(legend=list(title=list(text='Player')))
    })
    
    output$scatter_plot <- renderPlotly({
        
        if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
            fig <- emptyPlot
        }
        else{
            df_here <- df_scatter %>%
                mutate(date = as.Date(data, format = "%Y.%m.%d")) %>%  
                filter(color==input$color_2, gracz==input$gracz, rodzaj == input$rodzaj_2) %>% 
                filter(date >= input$dates_2[1] & date <= input$dates_2[2])
            
            fig <- ggplot(df_here, aes(x = ranking_przeciwnik, y = ranking_gracz,
                                       color = as.character(wynik))) +
                geom_abline(intercept = 0, slope = 1) +
                geom_point(alpha = 0.4, size = 0.7) +
                geom_smooth(method = lm, formula = y ~ x, se=FALSE, size = 0.5)  +
                theme_bw()
            
            fig <- ggplotly(fig) %>% 
                layout(title = "Opponents' rating vs our rating",
                       legend =list(title=list(text='Score')),
                       xaxis = list(title = "Opponents' rating"),
                       yaxis = list(title = "Our rating"))
        }
        fig
        
    })
    
    observe(updateSelectInput(session,inputId ="dane_heatmap" ,
                              choices = head(pull(filter(df_0,color==input$color_2,
                                                         gracz==input$gracz, rodzaj==input$rodzaj_2),opening_name),5)))
    
    output$heatmap_openings <- renderPlotly({
        
        if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
            p <- emptyPlot
        }
        else{
            df_heatmap<-df_ruchygracza %>%
                filter(color == input$color_2,
                       opening_name==input$dane_heatmap,
                       gracz==input$gracz,
                       rodzaj==input$rodzaj_2)
            
            p <- ggplot(df_heatmap, aes(X, Y, fill= count)) +
                geom_tile()+
                scale_fill_gradient(high = "#f6e8c3",
                                    low = "#543005")+
                scale_y_continuous(breaks = seq(1, 8, len = 8))+
                theme_ipsum() +
                labs(x = element_blank(), y = element_blank())
            
            p <- ggplotly(p) %>% layout(
                title = "Where do we move pices across different opennings?"
            )
        }
        p
        
    })
}

shinyApp(ui, server)