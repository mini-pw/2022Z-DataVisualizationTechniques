
library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(tidyr)


#wczytanie ramki z zadania
suppressWarnings(
    df <- read_csv("complete.csv", col_types = cols(awardYear = col_date(format = "%Y"))) %>% 
    select("awardYear","category", gender ,"birth_countryNow", "birth_continent")
)



make_df <- function(select) {     
    #funkcja potrzebna do pierwszego wykresu, zwraca odpowiednia ramke danych w zaleznosci od selectInput
    if(select == "country"){
        countries <- df %>% 
            filter(!is.na(birth_countryNow)) %>% 
            group_by(birth_countryNow) %>% 
            count() %>%
            arrange(desc(n)) %>% 
            head(6) 
        df1 <- df %>% 
            select(temp = birth_countryNow,awardYear) %>% 
            filter(temp %in% countries$birth_countryNow)
    } else if (select == "continent"){
        df1 <-  df %>% 
            select(temp = birth_continent,awardYear) 
    } else {
        df1 <-  df %>% 
            select(temp = category,awardYear) 
    }
    
    df1 <- df1 %>% 
        select(temp,awardYear) %>% 
        filter(!is.na(temp),!is.na(awardYear)) %>% 
        group_by(temp,awardYear) %>% 
        count() %>% 
        pivot_wider(names_from = awardYear, values_from = n,values_fill = 0) %>%  
        pivot_longer(cols = -temp, names_to = "awardYear" ,values_to = "n") %>% 
        group_by(temp) %>% 
        arrange(awardYear, .by_group = TRUE) %>% 
        mutate(cumulative = cumsum(n)) 
    df1
}   #end funkja pomocnicza


#wczytenie ramki danych potrzebnej do wykresu drugiego - mapy
df_c <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") %>% select(-GDP..BILLIONS. )

#zamiana nazw panstw ktore roznia sie w ramkach
df_c$COUNTRY[which(df_c$COUNTRY == "Netherlands")] <- "the Netherlands"
df_c$COUNTRY[which(df_c$COUNTRY == "United States")] <- "USA"

#koncowa tabelka do drugiego wykresu
df2 <- df %>% select(awardYear,category,gender, COUNTRY =birth_countryNow )






shinyServer(function(input, output,session) {
    
    # to jest potrzebne, aby wykres i tekst drugie wyswietlaly sie odpowiednio, zamieniam tez mozliwe checkboxy w zaleznosci od wyboru radioButtons 
    prev <- reactiveValues(b = "male", a = "Economic Sciences")
    observeEvent(input$by2,{
        if(input$by2 == "gender"){
            if(length(input$next_box) != 0 &&input$next_box[1] != "male"){
                prev$a <- input$next_box}
            updateCheckboxGroupInput(session,"next_box", "Gender:",
                                     choices = list("Male" = "male","Female" = "female"),selected = prev$b)
        } else {
            prev$b <- input$next_box
            updateCheckboxGroupInput(session,"next_box", "Category:",
                                     c("Economic Sciences","Physics","Chemistry","Peace","Physiology or Medicine","Literature" ),
                                     selected = prev$a)
        }
    })

    
    
    
    output$dispPlotly1 <-  renderPlotly({       #troche zmieniony plot z HW5
        df1 <- make_df(input$select) %>% filter(awardYear>=input$range[1], awardYear<=input$range[2]) %>% mutate(awardYear = as.Date(awardYear,"%Y-%m-%d"))
        
        #tekst nad wykresem
        output$text1 <-  renderText(paste0("Nobel Prize laureates by ", input$select))
        
        plt <- plot_ly() %>% 
            add_lines(data = df1, x = ~awardYear,y = ~cumulative, split = ~temp,
                       text = df1$cumulative,
                       hovertemplate = paste('<b>%{text}</b>'), line = list(width = 4)
                      ) %>% 
            layout(
                xaxis = list(
                    title = "Year",
                    hoverformat =  "%Y"
                    ),
                yaxis = list(title = "Number of laureates"),
                hovermode = "x",
                plot_bgcolor='transparent',
                paper_bgcolor='transparent',
                font = list(color = '#FFFFFF'),
                margin = list(l = 100)
            ) %>%
            config(displayModeBar = FALSE)
        
    })  ## end dispPlotly1
    
    output$text2 <- renderText({    ## rozny tekst w zaleznosci od zaznaczonych checkboxow
        hover = input$next_box[1]   
        title_text <- paste0("Share of ", hover," Nobel Prize laureates by country")
        if(input$by2 == "gender"){
            if(length(input$next_box) >1){
                hover = "male and female"
            }
            title_text <- paste0("Share of ", hover," Nobel Prize laureates by country")
        } else { 
            if(length(input$next_box) == 6){
                hover = "all categories"
            } else {
                hover = input$next_box[1]
                
                for(i in 2:(length(input$next_box))){
                    hover = paste0(hover, ", ",input$next_box[i])
                }   
            }
            title_text <- paste0("Share of  Nobel Prize laureates in ", hover, " by country")
        }
        if(length(input$next_box) == 0){
            title_text <- "No checkbox selected"
        }
        title_text
    }) %>% bindCache(input$next_box, input$map_type) ##end text2
        
    output$dispPlotly2 <- renderPlotly({
       
        #w zaleznosci od wyboru w radioButtons mamy rozna ramke danych -> rozny wykres
        if(input$by2 == "gender"){
            df_gen <- df2 %>% 
                filter(gender %in% input$next_box) %>% 
                select(COUNTRY,gender) %>% 
                na.omit()
            
            bottom = nrow(df_gen)
            
            df_gen <-  df_gen %>% 
                group_by(COUNTRY) %>% 
                count() %>% 
                mutate(ratio = n/bottom * 100)
            
            final_df <- df_gen
        } else { 
            
            df_cat <- df2 %>% 
                filter(category %in% input$next_box) %>% 
                select(COUNTRY,category) %>% 
                na.omit()
            
            bottom = nrow(df_cat)
            
            df_cat <-  df_cat %>% 
                group_by(COUNTRY) %>% 
                count() %>% 
                mutate(ratio = n/bottom * 100)
            
            final_df <- df_cat
        }
        
        #ostateczna ramka do plota
        final_df<- left_join(final_df,df_c, by = "COUNTRY")
        
        plt2 <- plot_ly(final_df, type='choropleth', locations=final_df$CODE, z=final_df$ratio,
                        text = paste0(final_df$COUNTRY, "<br>Share: ", round(final_df$ratio,2),"%", "<br>Number of laureates: ", final_df$n),
                        hoverinfo = 'text',
                        colorscale = "Greens",reversescale = T) %>% 
            layout(
                geo = list(projection = list(type = input$map_type),
                           showland = TRUE,landcolor = toRGB("#d3d3d3"),
                           bgcolor = "rgba(0,0,0,0)",
                           showocean = T,oceancolor = toRGB("LightBlue")),
                plot_bgcolor="rgba(0,0,0,0)",
                paper_bgcolor= "rgba(0,0,0,0)",
                margin = list(l = 50,r = 50,pad = 4)
            ) 
        suppressWarnings(plt2 <- plt2 %>% 
            colorbar(len = 1,
                     title = list(text = "Share",font = list(color = "#FFFFFF")),
                     ticksuffix = "%",
                     tickvals = round(seq(floor(max(final_df$ratio)),ceiling(min(final_df$ratio)),length = 5),0),
                     ticks = "outside",
                     tickfont = list(color = '#FFFFFF'),
                     ypad = 150) %>% 
            config(displayModeBar = FALSE))
        
        
    }) %>% bindCache(input$next_box, input$map_type)    ##end dispPlotly2
}) ## end server



