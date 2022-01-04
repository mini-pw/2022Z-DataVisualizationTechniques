library(rsconnect)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(ggpubr)
library(gridExtra)

require(maps)

# setwd("C:/Users/laura/Desktop/TECHNIKI WIZUALIZACJI DANYCH/hw6")

world <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
colnames(world)[1] <- "country"
# head(world)

nobel <- read.csv("complete.csv")
# View(nobel)
# head(nobel)

data <- nobel[c("category","gender","ind_or_org","birth_countryNow","org_founded_countryNow")] %>%
    mutate( 
        gender_org = ifelse(gender=="","organization",gender), 
        country =  ifelse(gender=="",org_founded_countryNow,birth_countryNow),
    ) %>%
    select("category","gender_org","country")

# data$country[data$country == "Democratic Republic of the Congo"] <- "Congo, Democratic Republic of the" # ??????
# data$country[data$country == "East Timor"] <- "Timor-Leste"
# data$country[data$country == "Faroe Islands (Denmark)"] <- "Faroe Islands"
# data$country[data$country == "Myanmar"] <- "Burma"
# data$country[data$country == "North Macedonia"] <- "Macedonia"
# data$country[data$country == "Northern Ireland"] <- "Ireland"
# data$country[data$country == "Scotland"] <- "United Kingdom"
# data$country[data$country == "South Korea"] <- "Korea, South"
# data$country[data$country == "the Netherlands"] <- "Netherlands"
# data$country[data$country == "USA"] <- "United States"

# head(data)

world$country[world$country == "Congo, Democratic Republic of the"] <- "Democratic Republic of the Congo"
world$country[world$country == "Timor-Leste"] <- "East Timor"
world$country[world$country == "Faroe Islands"] <- "Faroe Islands (Denmark)"

data$country[data$country == "North Macedonia"] <- "Macedonia"
data$country[data$country == "Northern Ireland"] <- "Ireland"
data$country[data$country == "Scotland"] <- "United Kingdom"

# world$country[world$country == "Burma"] <- "Myanmar"
# world$country[world$country == "Macedonia"] <- "North Macedonia"
# world$country[world$country == "Ireland"] <- "Northern Ireland"

world$country[world$country == "United Kingdom"] <- "Scotland"
world$country[world$country == "Korea, South"] <- "South Korea"
world$country[world$country == "Netherlands"] <- "the Netherlands"
world$country[world$country == "United States"] <- "USA"

# head(world)

data.map <- data %>%
    full_join(., world, by="country")
# head(data.map)


# MAPA
ui <- fluidPage(

    # Application title
    titlePanel("Nobel Prize distribution in each country"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "category",
                label = "Select prize category:",
                choices = data$category %>% unique() ),
            checkboxGroupInput(inputId = "gender_org",
                               label = "Select gender\n/ organization: ",
                               choices = c("male","female", "organization")),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
            shinycssloaders::withSpinner(plotly::plotlyOutput("mapPlot"),1,"gray"),
            width = 10
        )
    )
)

# PLCI I WIEKU
dane <- nobel

dane$birth_date <- ymd(dane$birth_date)
df <- dane %>% 
    filter(ind_or_org == "Individual") %>% 
    select(birth_continent, category, gender, awardYear, birth_date) %>% 
    na.omit() %>% 
    # filter(birth_date != "") %>% 
    mutate(age = awardYear - year(birth_date)) %>%
    select(birth_continent, category, gender, age)

ui2 <- fluidPage(
    
    # Application title
    titlePanel("Comparison between number of female and male Nobel winners by continent and category"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectedContinent",
                        "Select continent :",
                        choices = unique(df$birth_continent)),
            checkboxGroupInput("categories",
                               "Select categories :",
                               c("Chemistry" ="Chemistry",
                                 "Economic Sciences" = "Economic Sciences",
                                 "Literature" = "Literature",
                                 "Peace"= "Peace",
                                 "Physics" = "Physics",
                                 "Physiology or Medicine" = "Physiology or Medicine")),
            width = 3
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            shinycssloaders::withSpinner(plotOutput("distPlot"),1,"gray"),
            width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # MAPA
    output$mapPlot <- plotly::renderPlotly({
        # cat = "Physics"
        # gen.org = c("male","female","organization")
        cat = input$category
        gen.org = input$gender_org
        data2 <- data.map[ data.map$category == cat, ]
        data2 <- data2[ data2$gender_org %in% gen.org, ]
        # head(data2)
        
        data.map.new <- data2 %>%
            group_by(country, CODE) %>%
            summarise( licz.os = n(), .groups = "keep" )
        
        g <- list(
            showframe = F,
            showland = T,
            landcolor = toRGB("grey90")
        )
        
        a <- list(
            x = 0,
            y = 0,
            text = "(Hover over for details)",
            showarrow = FALSE
        )

        mapa <- plot_ly(data.map.new, type='choropleth', locations=~CODE, 
                        z=~licz.os, 
                        text=~country, hovertemplate = "%{text}: %{z}<extra></extra>",
                        colors = "RdYlGn",
                        # colorscale = "Viridis",
                        reversescale = FALSE,
                        marker = list(
                            line = list(color = toRGB("black")),
                            size = seq(0,39),
                            color = seq(0,39)
                            ),
                        colorbar = list(title = "Number of\nthe awarded", limits = c(0,80), which = 0)#, len = 2, lenmode = "pixels")
                        ) %>%
            layout( geo = g, coloraxis = list(colorscale="Jet"), annotations = a )
        # layout( title = "Nobel Prize distribution in each country", geo = g)
        plotly::ggplotly(mapa)
    })
    
    # PLCI I WIEKU
    output$distPlot <- renderPlot({
        df_subset <- df[df$birth_continent == input$selectedContinent,]
        df_subsubset <- df_subset[df_subset$category %in% input$categories,]
        f <- df_subsubset %>% filter(gender == "female")
        m <- df_subsubset %>% filter(gender == "male")
        
        p <- ggplot() + 
            geom_histogram(data=f, aes(f$age, y= -..count..),
                           color = "white",
                           fill = "hotpink1",
                           bins = 30) +
            geom_histogram(data=m, aes(m$age, y= ..count..),
                           color = "white",
                           fill = "steelblue2",
                           bins = 30) +
            scale_x_continuous(breaks = seq(16, 100, 2),
                               labels = c('16', '', '20', '', '24', '', '28','','32','','36','','40','','44','','48','','52','','56','','60','','64','','68','','72','','76','','80','','84','','88','','92','','96','','100')) +
            scale_y_continuous(breaks = seq(-36,36,2),
                               labels = c('36','', '32','', '28','', '24','', '20','', '16','', '12','', '8','', '4','', '0','', '4','', '8','', '12','', '16','', '20','', '24','', '28','', '32','', '36')) + 
            expand_limits(y=c(-36, 36)) +
            labs(x = "Age",
                 y = "Females vs Males") +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "grey80", size = 0.5),#, linetype = 2),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13),
                  axis.text.x = element_text(size = 11),
                  axis.text.y = element_text(size = 11)) +
            coord_flip() 
        
        
        tab <- as.data.frame(
            c( Females = "", Males = "" )
        )
        
        p_tab <- ggtexttable(unname(tab), 
                             theme = ttheme(tbody.style = tbody_style(fill = c("hotpink1", "steelblue2"))))
        grid.arrange(p, p_tab, widths = c(2.5, 0.3), ncol = 2)
        
    })
    
}

app_ui <- navbarPage(
    title = "Data analysis: Nobel Prize",
    tabPanel("Map plot", ui, icon = icon("globe-americas")),
    tabPanel("Gender-age plot", ui2, icon = icon("venus-mars")),
    theme = bslib::bs_theme( bootswatch = "lux" ),
)

# Run the application 
# shinyApp(ui = ui, server = server)
shinyApp(ui = app_ui, server = server)
# runApp(shinyApp(app_ui, server))


