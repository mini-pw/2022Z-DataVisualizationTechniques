library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(patchwork)
library(tidyr)
library(gganimate)
setwd("C:/school/TWD/hw/hw6")
nobles <- read.csv('complete.csv')


world <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
world <- world %>% select(-2)

x1<-c("Congo, Democratic Republic of the", "Korea, South", "Netherlands", "United States")
x2<-c("Democratic Republic of the Congo", "South Korea", "the Netherlands", "USA")


nobles <- nobles %>% group_by(category, birth_countryNow, awardYear) %>% 
  filter(birth_countryNow != '') %>%
  mutate(
    birth_countryNow = ifelse(
      birth_countryNow == x2[1], x1[1], 
      ifelse(birth_countryNow == x2[2], x1[2],
             ifelse(birth_countryNow == x2[3], x1[3],
                    ifelse(birth_countryNow == x2[4], x1[4],
                           birth_countryNow)))))
df <- nobles %>% ungroup() %>% 
  group_by(category, birth_countryNow, awardYear) %>% 
  summarise(nobles = n()) %>% ungroup() %>% group_by(category, birth_countryNow) %>% 
  select(awardYear, category, birth_countryNow, nobles) %>% 
  arrange(awardYear) %>% pivot_wider(names_from = awardYear, values_from = nobles) %>% 
  pivot_longer(cols = 3:118, names_to = "Year", values_to = "nobles") %>% 
  mutate(nobles = ifelse(is.na(nobles), 0, nobles),
  nobles = cumsum(nobles),
  Year = as.numeric(Year))

dfworld <- world %>% inner_join(df, by = c("COUNTRY" = "birth_countryNow")) %>%
  select(CODE, COUNTRY, nobles, Year, category) %>% 
  mutate(nobles = ifelse(nobles == 0, NA, nobles))


dfworld1 <- dfworld %>% rename(County = COUNTRY, "Amount of prizes" = nobles, Category = category)
dfworld2 <- dfworld %>% unique()

server <- function(input, output, session) {
  data <- dfworld %>% filter(category == "Physics")
  observe({
    cat <- input$category
    country <- input$country
    data <- dfworld %>% filter(category == cat)

    
  output$plot1 <- plotly::renderPlotly({
    plot_ly(data = data,
                   type='choropleth', locations= ~CODE,
                   z = ~nobles,
                   color = ~nobles,
                   colors = 'Reds',
                   frame = ~Year,
                   text = ~COUNTRY,
                   
                   hoverinfo = 'z+text') %>% 
      animation_opts(30) %>% 
      animation_button()
  }) %>% bindCache(input$category)
  
  output$plot2 <- plotly::renderPlotly({
    p1 <- ggplotly(
      dfworld %>% filter(COUNTRY == input$country) %>% 
        ggplot(aes(x = Year, y = nobles, color = category))+
        geom_path()+
        labs(x = "Year", y = "Number of prizes") + theme( 
          panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "white")))
    plotly::ggplotly(p1)
    }) %>% bindCache(input$country)
  
  output$tab1 <- renderDataTable(dfworld1 %>%
                                   filter(Year == max(Year)) %>%
                                   arrange(Category) %>% 
                                   select(2, 3, 5))
  })
}

ui <- fluidPage(
  
  titlePanel("Distribution of Nobel prizes in selected category"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "category",
        label = "Category:",
        choices = c("Physics", "Chemistry", "Literature", "Peace", "Physiology or Medicine", "Economic Sciences")
      )
      ,
      width = 3
    ),
    mainPanel(
      shinycssloaders::withSpinner(plotly::plotlyOutput("plot1")),
      width = 9
    )
  )
)


ui2 <- fluidPage(
  titlePanel("Number of nobel prize winners by country over the years"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = "Country:",
        choices = unique(dfworld$COUNTRY)
    ),
    width = 3),
    mainPanel(
      shiny::markdown("Instuction: \n to select or unselect categories, you must click on the chosen categories in the legend"),
      shinycssloaders::withSpinner(plotly::plotlyOutput("plot2")),
      width = 9
      
    )
  )
)

ui3 <- fluidPage(
  titlePanel("Number of nobel prize winners by country"),
 
    mainPanel(
      
      shinycssloaders::withSpinner(dataTableOutput("tab1")),
      width = 9
      
    )
  )


app_ui <- navbarPage(
  title = "Data analysis - Nobel Prize",
  tabPanel("World - map", ui),
  tabPanel("World - table", ui3),
  tabPanel("Selected country", ui2))

shinyApp(app_ui, server)

