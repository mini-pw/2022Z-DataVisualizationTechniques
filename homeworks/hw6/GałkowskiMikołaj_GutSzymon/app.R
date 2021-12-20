library(shiny)
library(shinycssloaders)
library(waffle)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(shinythemes)
library(shinyjqui)
library(shinydashboard)
library(imager)

theme_set(theme_bw(base_size = 20) + 
            theme(plot.title = element_text(face = "bold")))

options(repr.plot.width = 22, repr.plot.height = 8)


male =  "#7fbf7b" 
female = "#af8dc3" 

colour1 = "#1f78b4"

phy = "#9e0142"
che = "#f46d43"
med = "#5e4fa2"
pea = "#66c2a5" 
lit = "#3288bd"
ecn = "#190678"

df <- read_csv('complete.csv')

gen_pal <- c(male, female) 
cat_pal <- c(phy, che ,med ,pea ,lit ,ecn)
names(cat_pal) <- c('Physics', 'Chemistry', 'Physiology or Medicine',
                    'Peace', 'Literature', 'Economic Sciences')

df$laureate_or_org = if_else(is.na(df$knownName), "Organization", "Individual")
df$laureate_or_org = factor(df$laureate_or_org, levels = c("Individual", "Organization"))
df$gender <- str_to_title(df$gender)
df$gender <- factor(df$gender, levels = c('Male', 'Female'))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("  Introduction", tabName = "introduction", icon = icon("cloudsmith"),
             badgeLabel = "new", badgeColor = 'green'),
    menuItem("Dataset", tabName = "dataset", icon = icon("database")),
    menuItem("Charts", id = "charts",  icon = icon("chart-bar"), startExpanded = TRUE,
             menuSubItem("Age", tabName = "ageTab"),
             menuSubItem("Birth continent", tabName = "continentTab"),
             menuSubItem("No. laureates by sex", tabName = "sexTab"),
             menuSubItem("No. laureates by category", tabName = "categoryTab")),
    box(height = 0.0000001, background = "teal"),
    box(width = 12, background = 'light-blue',
        checkboxGroupInput("przyciski", label = "Wybierz kategorię",
                           choices = c('Physics', 'Chemistry', 'Physiology or Medicine',
                                       'Peace', 'Literature', 'Economic Sciences'),
                           selected = "Physics")
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = 'Noble dataset analysis', 
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Danonki",
                                 message = "Welcome in hw 6 :)"
                               )
                  ),
                  tags$li(a(href = 'https://ww2.mini.pw.edu.pl/',
                            img(src = 'https://konkurs.mini.pw.edu.pl/sites/default/files/m_logo_120x120_2.png',
                                title = "Logo MiNI", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  sidebar,
  dashboardBody(
    tabItems(
      tabItem(tabName = 'introduction', fluidPage(
        fluidRow(box(solidHeader = TRUE, status = "primary", shiny::HTML(
          "<h1>Nobel Prize</h1>
            &emsp; The Nobel Prize are five 
            separate prizes that, according to Alfred Nobel's will of 1895, are awarded to those who, during the preceding year, 
            have conferred the greatest benefit to Mankind. Alfred Nobel was a Swedish chemist, engineer, and industrialist most famously 
known for the invention of dynamite. He died in 1896. In his will, he bequeathed all of his remaining realisable assets to 
be used to establish five prizes which became known as Nobel Prizes. Nobel Prizes were first awarded in 1901.
<br/>
&emsp; Nobel Prizes are awarded in the fields of Physics, Chemistry, Physiology or Medicine, Literature, a
nd Peace (Nobel characterized the Peace Prize as to the person who has done the most or best to advance 
          fellowship among nations, the abolition or reduction of standing armies, and the establishment and promotion 
          of peace congresses).In 1968, Sveriges Riksbank (Sweden's central bank) funded the establishment of the 
Prize in Economic Sciences in Memory of Alfred Nobel, to also be administered by the Nobel Foundation. 
Nobel Prizes are widely regarded as the most prestigious awards available in their respective fields.
<br/>
&emsp; The prize ceremonies take place annually. Each recipient (known as a 'laureate') receives a gold medal, 
a diploma, and a monetary award. In 2021, the Nobel Prize monetary award is 10,000,000 SEK. A prize 
may not be shared among more than three individuals, although the Nobel Peace Prize can be awarded to 
organizations of more than three people. Although Nobel Prizes are not awarded posthumously, if a person 
is awarded a prize and dies before receiving it, the prize is presented.
<br/>
&emsp; The Nobel Prizes, beginning in 1901, and the Nobel Memorial Prize in Economic Sciences, beginning in 1969, have been awarded 603 
times to 962 people and 25 organizations.Four individuals have received more than one Nobel Prize."
        )),
          shiny::HTML(
            "<img src='https://cdn.editage.com/insights/editagecom/production/Does%20the%20Nobel%20Prize%20need%20some%20changes%20%28original%29_0_0.jpg' alt='Nobel Prize' width='30%' height='30%' 
            style='display: block; margin-left: auto; margin-right: auto; padding-top: 35px'>"
          )
        ),
        fluidRow(
          box(width = 12, shiny::HTML(
            '<footer class="page-footer font-small blue">
              
              <!-- Copyright -->
              <div class="footer-copyright text-center py-3">© 2021 Copyright
              <br/>
              <span style="font-weight: bold">Szymon Gut<span/>
              <a href="https://github.com/Szymon-Gut">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <br/>
              <span style="font-weight: bold">Mikołaj Gałkowski<span/>
              <a href="https://github.com/galkowskim">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              </div>
              <!-- Copyright -->
              
              </footer>'
          ))
        ))),
      tabItem(tabName = 'dataset', fluidPage(
        fluidRow(box(width = 12, DT::dataTableOutput("table"))),
        fluidRow(
          box(width = 12, shiny::HTML(
            '<footer class="page-footer font-small blue">
              
              <!-- Copyright -->
              <div class="footer-copyright text-center py-3">© 2021 Copyright
              <br/>
              <span style="font-weight: bold">Szymon Gut<span/>
              <a href="https://github.com/Szymon-Gut">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <br/>
              <span style="font-weight: bold">Mikołaj Gałkowski<span/>
              <a href="https://github.com/galkowskim">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              </div>
              <!-- Copyright -->
              
              </footer>'
          ))
        ))),
      tabItem(tabName = 'ageTab', fluidPage(
        fluidRow(
          box(solidHeader = TRUE, status = "primary", shinycssloaders::withSpinner(plotOutput("ageDistro"), color = "#3C8DBC", type = 3, color.background = 'white')),
          box(solidHeader = TRUE, status = "primary", shinycssloaders::withSpinner(plotOutput("oldestLaureates"), color = "#3C8DBC", type = 3, color.background = 'white'))
        ),
        fluidRow(
          column(width = 6, 
                 box(width = NULL, status='warning', "Wykres punktowy, pokazuje rozkład według wieku dla laureatów nagrody Nobla z podziałem na płeć.
Dodatkowo dodaliśmy violin plota, aby łatwiej  było odczytać, czy w danej wartości występuje dużo punktów czy są to raczej pojedyńcze przypadki.")),
          column(width = 6,
            box(width = NULL, status='warning', "Wykres słupkowy przedstawia najstarszych laureatów, po wybraniu odpowiednich kategorii, którymi jesteśmy zainteresowani. Zastosowane są 
kolory, aby odróżnić laureatów z poszczególnych dziedzin nauki."))
        ),
        fluidRow(
          box(width = 12, shiny::HTML(
            '<footer class="page-footer font-small blue">
              
              <!-- Copyright -->
              <div class="footer-copyright text-center py-3">© 2021 Copyright
              <br/>
              <span style="font-weight: bold">Szymon Gut<span/>
              <a href="https://github.com/Szymon-Gut">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <br/>
              <span style="font-weight: bold">Mikołaj Gałkowski<span/>
              <a href="https://github.com/galkowskim">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              </div>
              <!-- Copyright -->
              
              </footer>'
          ))
        )
      )),
      tabItem(tabName = 'continentTab', fluidPage(
        fluidRow(
          column(width = 12,
                 box(width = NULL, status = 'primary', shinycssloaders::withSpinner(plotOutput("birthContinent"), color = "#3C8DBC", type = 3, color.background = 'white')),
                 box(width = NULL, status='warning', "Barplot, pokazuje jak wygląda rozkład laureatów 
                 w zależności od kontynentu na którym sie urodzili, w zależności od kategorii którą wybierzemy."))
        ),
        fluidRow(
          box(width = 12, shiny::HTML(
            '<footer class="page-footer font-small blue">
              
              <!-- Copyright -->
              <div class="footer-copyright text-center py-3">© 2021 Copyright
              <br/>
              <span style="font-weight: bold">Szymon Gut<span/>
              <a href="https://github.com/Szymon-Gut">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <br/>
              <span style="font-weight: bold">Mikołaj Gałkowski<span/>
              <a href="https://github.com/galkowskim">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              </div>
              <!-- Copyright -->
              
              </footer>'
          ))
        )
        )),
      tabItem(tabName = 'sexTab', fluidPage(
        fluidRow(
          column(width = 3),
          box(width = NULL, solidHeader = TRUE, status = 'primary', shinycssloaders::withSpinner(plotOutput("waffle"), color = "#3C8DBC", type = 3, color.background = 'white')),
          box(width = NULL, status='warning', "Wykres ukazuje ilość laureatów w podziale na płeć. Dodatkowo działa on z checkBox'ami, które pozwalają filtrować kategorie.")),
          column(width = 4)  
        ),
        fluidRow(
          box(width = 12, shiny::HTML(
            '<footer class="page-footer font-small blue">
              
              <!-- Copyright -->
              <div class="footer-copyright text-center py-3">© 2021 Copyright
              <br/>
              <span style="font-weight: bold">Szymon Gut<span/>
              <a href="https://github.com/Szymon-Gut">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <br/>
              <span style="font-weight: bold">Mikołaj Gałkowski<span/>
              <a href="https://github.com/galkowskim">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              </div>
              <!-- Copyright -->
              
              </footer>'
          ))
        )
      ),
      tabItem(tabName = 'categoryTab', fluidPage(
        fluidRow(
          column(width = 3),
          box(width = NULL, solidHeader = TRUE, status = 'primary', shinycssloaders::withSpinner(plotOutput("numberOfNobelPrizeWinners"), color = "#3C8DBC", type = 3, color.background = 'white')),
          box(width = NULL, status='warning', "Barplot ukazuje liczbę laureatów dla poszczególnych (wybranych przez nas) kategorii.")),
          column(width = 4) 
        ),
        fluidRow(
          box(width = 12, shiny::HTML(
            '<footer class="page-footer font-small blue">
              
              <!-- Copyright -->
              <div class="footer-copyright text-center py-3">© 2021 Copyright
              <br/>
              <span style="font-weight: bold">Szymon Gut<span/>
              <a href="https://github.com/Szymon-Gut">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              <br/>
              <span style="font-weight: bold">Mikołaj Gałkowski<span/>
              <a href="https://github.com/galkowskim">
                <img src="https://cdn-icons-png.flaticon.com/512/25/25231.png" alt="github logo" width = "1.5%" height = "1.5%">
              </a>
              </div>
              <!-- Copyright -->
              
              </footer>'
          ))
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$table <- DT::renderDataTable({
    df %>% filter(category %in% input$przyciski) %>% select(awardYear, category, name,gender, birth_city,birth_country)
  })
  
  output$nobel <- renderImage({
    filename <- normalizePath(file.path('.',
                                        'nobel.png'))
  })
  
  output$waffle <- renderPlot({
    gender <- df %>% 
      filter(category %in% input$przyciski) %>% 
      select(gender)
    waffle(table(gender), rows=10, size=1, colors = gen_pal, 
           title = 'Number of Male/Female laureates')
  })
  
  output$numberOfNobelPrizeWinners <- renderPlot({
    cat_n <- df %>% 
      filter(category %in% input$przyciski) %>% 
      count(category, sort = TRUE)
    
    ggplot(cat_n, aes(x = reorder(category, n), y = n, fill = colour1)) +
      geom_col() + 
      labs(title = 'Number of Nobel prize winners', x = '', y = '') +
      geom_text(aes(label=n), hjust = 1.2, color = "white", size = 5, fontface = "bold") + 
      coord_flip() + 
      theme(legend.position = "none") + 
      scale_fill_manual(values = colour1) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank())
  })
  
  output$oldestLaureates <- renderPlot({
    df2 <- df %>%
      mutate(awardDate = as.Date(ISOdate(awardYear, 10, 01)), 
             age_days = awardDate - birth_date,
             age_years = round(interval(birth_date, awardDate) / years(1)))
    
    oldest <- df2 %>%
      filter(category %in% input$przyciski) %>% 
      select(name, category, age_days, age_years) %>%
      arrange(desc(age_days))
    
    ggplot(head(oldest, 15), aes(reorder(name, age_days), age_years, fill = category)) +
      geom_col() +
      labs(title = 'Oldest Laureates', x = '', y = '', fill = 'Category') +  
      geom_text(aes(label = age_years), hjust = 1.2, colour = "white", size = 5, fontface = "bold") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), 
                         expand = expansion(mult = c(0.004, 0))) + 
      coord_flip() +
      scale_fill_manual(values = cat_pal) +
      theme(legend.position='right', legend.justification = "top",
            panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.title = element_text(size = 15, face = 'bold'),
            legend.text = element_text(size = 12))
  })
  
  
  output$birthContinent <- renderPlot({
    by_continent <- df %>%
      filter(category %in% input$przyciski) %>% 
      drop_na(birth_continent) %>%
      group_by(birth_continent) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) 
    
    ggplot(by_continent, aes(reorder(birth_continent, n), n, fill = colour1)) +
      geom_col() +
      labs(title = 'Birth Continent', x = '', y = '') +  
      geom_text(aes(label = n), hjust = -0.3, colour = "#555555", size = 6, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0.005, 0.1))) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.position = "none",panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
  })
  
  output$ageDistro <- renderPlot({
    df2 <- df %>%
      mutate(awardDate = as.Date(ISOdate(awardYear, 10, 01)), 
             age_days = awardDate - birth_date,
             age_years = round(interval(birth_date, awardDate) / years(1)))
    
    age_data <- df2 %>% 
      filter(category %in% input$przyciski) %>% 
      drop_na(age_years) %>%
      select(awardYear, category, gender, age_days, age_years, name)
    
    
    ggplot(age_data, aes(x = gender, y = age_years, col = gender)) +
      geom_violin() + 
      labs(title = 'Age Distribution', x = '', y = '') +
      geom_jitter(size = 4.3, alpha = 0.4, width = 0.3) +
      coord_flip() +
      theme(legend.position = "none") + 
      scale_color_manual(values = gen_pal)
  })
  output$table <- DT::renderDataTable({
    df %>% filter(category %in% input$przyciski) %>% select(awardYear, category, name,gender, birth_city,birth_country)
    
  })
  
}

shinyApp(ui, server)

