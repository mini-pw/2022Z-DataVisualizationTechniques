library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(forcats)
library(viridis)
library(htmltools)
library(tmaptools)

# loading data
data_path <- "data/nobel_prize.csv"
nobel_df <- read.csv(data_path, encoding = 'UTF-8')


wrap_with_spinner <- function(element) {
  shinycssloaders::withSpinner(
    element,
    type = 1, 
    color = "#2c3e50",
    color.background = "white"
  )
}

# preparing data
path <- file.path(".", "data")
filepath <- file.path(path, "birthPlace_df.csv")

if (!file.exists(filepath)) {
  birthPlace <- nobel_df %>% 
    filter(birth_cityNow != "", birth_countryNow != "") %>% 
    transmute(birthPlace = paste(birth_cityNow, birth_countryNow, sep= ", ")) %>% 
    pull(birthPlace) %>% 
    unique() 
  
  birthPlace_df <- geocode_OSM(birthPlace, details = FALSE, as.data.frame = TRUE)
  write.csv(birthPlace_df, filepath, row.names = FALSE)
} else {
  birthPlace_df <- read.csv(filepath, encoding = "UTF-8")
}


birthPlace_df <- nobel_df %>%
  select(awardYear, category, name, motivation, birth_cityNow, birth_countryNow, birth_continent) %>% 
  filter(birth_cityNow != "", birth_countryNow != "") %>% 
  mutate(birthPlace = paste(birth_cityNow, birth_countryNow, sep= ", ")) %>% 
  right_join(birthPlace_df, by = c("birthPlace" = "query"))
