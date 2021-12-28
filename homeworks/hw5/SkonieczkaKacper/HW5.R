library(plotly)
library(dplyr)

data <- read.csv("complete.csv")

data %>% 
  mutate(Wiek =as.numeric(awardYear) - as.numeric(substring(birth_date,1, 4))) %>% 
  select(Wiek, gender, category, awardYear) %>% 
  filter(gender != "") ->
  data

data[data$category == "Chemistry",]$category <- "Chemia"
data[data$category == "Economic Sciences",]$category <- "Ekonomia"
data[data$category == "Literature",]$category <- "Literatura"
data[data$category == "Peace",]$category <- "Pokojowa"
data[data$category == "Physics",]$category <- "Fizyka"
data[data$category == "Physiology or Medicine",]$category <- "Fizjologia lub medycyna"


femaleData <- filter(data, gender == "female")
maleData <- filter(data, gender == "male")


m <- list(
  t = 80,
  pad = 4
)


plot_ly() %>% 
  add_trace(data = maleData,
            type = "histogram",
            x = ~Wiek,
            frame = ~category,
            name = "mężczyźni") %>% 
  add_trace(data = femaleData,
            type = "histogram",
            x = ~Wiek,
            frame = ~category,
            name = "kobiety") %>% 
  layout(
    barmode="stack",
    bargap=0.1,
    yaxis = list(title = "Liczba nagrodzonych osób", range = list(0,41)),
    xaxis = list(range = list(0,100),  dtick = 5),
    title = list(text ="Liczba osób, które otrzymały nagrodę Nobla będąc w danym wieku", 
                 y = 0.97,
                 font = list(size = 25) ),
    legend=list(title=list(text='<b> Płeć </b>')),
    margin = m) %>% 
  animation_opts(
    2000, easing = "elastic"
  ) %>% 
  animation_button(label = "Start",  visible = FALSE) %>% 
  animation_slider(currentvalue = list(prefix= "Kategoria: ", xanchor = "left"))

 
