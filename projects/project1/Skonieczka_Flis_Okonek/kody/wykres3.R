#wykres liniowy

library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(viridis) 

options(scipen = 999)
glu <- read.csv("global-land-use-since-1000bc.csv")
allLand <- sum(filter(glu, Year == 2001)$area_aggregated_categories)


CropPasture <- filter(glu, Entity == "Cropland")
Pasture <- filter(glu, Entity == "Pasture")
CropPasture$area_aggregated_categories <- CropPasture$area_aggregated_categories + Pasture$area_aggregated_categories
CropPasture$Entity <- "Cropland & Pastures"
glu <- filter(glu, Entity != "Cropland", Entity != "Pasture")
glu <- rbind(glu, CropPasture)


UrbanVillages <- filter(glu, Entity == "Urban")
Villages <- filter(glu, Entity == "Villages")
UrbanVillages$area_aggregated_categories <- UrbanVillages$area_aggregated_categories + Villages$area_aggregated_categories
UrbanVillages$Entity <- "Urban & Villages"
glu <- filter(glu, Entity != "Urban", Entity != "Villages")
glu <- rbind(glu, UrbanVillages)

colnames(glu)[1] <- c("Rodzaj")

glu$area_aggregated_categories <- glu$area_aggregated_categories *100 / allLand
glu %>%
  filter(Year >= 1500) %>% 
  pivot_wider(names_from = Rodzaj, values_from = area_aggregated_categories, values_fill = 0)  -> glu2

p <- plot_ly(glu2, x = ~Year, y = ~`Permanent ice`, name = 'Permanent Ice', type = 'scatter', mode = 'lines',line = list(dash = NA, width = 4))
p <- p  %>% add_trace(y = ~`Semi-natural land`, name = 'Semi-natural land', mode = 'lines', line = list(dash = NA, width = 4, color = "#74C476")) %>% 
  add_trace(y = ~`Wild woodlands`, name = 'Wild woodlands', mode = 'lines', line = list(width = 4)) %>% 
  add_trace(y = ~`Wild barren land`, name = 'Wild barren lands', mode = 'lines', line = list(dash = NA, width = 4, color = "#FECC5C")) %>% 
  add_trace(y = ~`Cropland & Pastures`, name = 'Cropland & pasture', mode = 'lines', line = list(dash = NA, width = 4, color = "#A63603")) %>% 
  add_trace(y = ~`Urban & Villages`, name = 'Urban & Villages', mode = 'lines', line = list(dash = NA, width = 4, color = "white")) %>% 
  layout(title = 'Global land use since 1500', 
         xaxis = list(title = 'Year'), font=t, plot_bgcolor = "#e5ecf6",
         yaxis = list(title = 'Percentage of land use', ticksuffix= '%'), 
         legend = list(title=list(text='Type', size = 50)))
p
