library(plotly)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)

data <- read.csv("data.csv") # https://www.kaggle.com/imdevskp/nobel-prize
world <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
world <- world %>% select(-2)

x1<-c("Congo, Democratic Republic of the", "Korea, South", "Netherlands", "United States")
x2<-c("Democratic Republic of the Congo", "South Korea", "the Netherlands", "USA")

data_countries <- data %>% group_by(birth_countryNow, awardYear) %>% 
  summarise(n = n()) %>%
  filter(birth_countryNow != '') %>% 
  group_by(awardYear, birth_countryNow) %>% 
  mutate(
    birth_countryNow = ifelse(
      birth_countryNow == x2[1], x1[1], 
        ifelse(birth_countryNow == x2[2], x1[2],
               ifelse(birth_countryNow == x2[3], x1[3],
                      ifelse(birth_countryNow == x2[4], x1[4],
                             birth_countryNow)))),
    tmp = sum(n))

len <- length(unique(dataCountryCode$awardYear))+2

dataCountryCode <- world %>% inner_join(data_countries, by = c("COUNTRY" = "birth_countryNow"))
df <- dataCountryCode %>% 
  arrange(awardYear) %>% 
  pivot_wider(names_from = awardYear, values_from = tmp) %>% 
  select(-3) %>% pivot_longer(cols = 3:len, names_to = "Year", values_to = "cumulated") %>% 
  mutate(cumulated = ifelse(is.na(cumulated), 0, cumulated)) %>% 
  arrange(Year) %>% 
  ungroup() %>% group_by(CODE) %>% 
  mutate(cumulated = cumsum(cumulated)) %>% 
  mutate(cumulated = ifelse(cumulated == 0, NA, cumulated)) #nowe państwa "doskakują" na mapę


fig <- plot_ly(data = df,
               type='choropleth', locations= ~CODE,
               z = ~cumulated,
               color = ~cumulated,
               colors = 'Reds',
               frame = ~Year,
               text = ~COUNTRY,
               hoverinfo = 'z+text') #Ciekawszy, choć potencjalnie bardziej mylący jest ln(cumulated)

fig <- fig %>% animation_opts(30) %>% 
  animation_button(x = 0.55, y = 0.05) %>% 
  layout(title ="accumulated noble laureates by country until Year \nnotice the USA's Brain Drain") 

fig

#### natural log, looks much nicer, is less informative i guess
 fig2 <- plot_ly(data = df,
                type='choropleth', locations= ~CODE,
                z = ~log(cumulated),
                color = ~log(cumulated),
                colors = 'Reds',
                frame = ~Year,
                text = ~COUNTRY,
                hoverinfo = 'z+text') %>%
   layout(title =list(text = "accumulated noble laureates by country until Year \nnotice the USA's Brain Drain"),
          legend = list( 
            title = list(text = "natural log")))
 fig2
 fig2 <- fig2 %>% 
   animation_opts(30) %>%
   animation_button(x = 0.55, y = 0.05) 
 fig2
 