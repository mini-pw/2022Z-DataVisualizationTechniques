mammals <- read.csv('endemic-mammal-species-by-country.csv')
amphibians <- read.csv('endemic-amphibian-species-by-country.csv')
birds <- read.csv('endemic-bird-species-by-country.csv')

library(OpenStreetMap)
library(DT)
library(RColorBrewer)
library(mapproj)
library(sf)
library(RgoogleMaps)
library(scales)
library(rworldmap)
library(maps)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)
library(maptools)
library(leaflet)
library(sf)
library(tmap)
library(here)
library(rgdal)
library(scales)
library(flextable)
library(dplyr)
library("rnaturalearth")



data1 <- inner_join(mammals,birds, by = c("Entity"))
data1 <- inner_join(data1,amphibians, by = c("Entity"))
data1 <- data1 %>% 
  mutate(suma = Mammals..total.endemic.+Amphibians..total.endemic.+Birds..total.endemic.)



###########################################################


library(dplyr)
library(maps)

mammals <- read.csv('endemic-mammal-species-by-country.csv')
amphibians <- read.csv('endemic-amphibian-species-by-country.csv')
birds <- read.csv('endemic-bird-species-by-country.csv')

data1 <- inner_join(mammals,birds, by = c("Entity"))
data1 <- inner_join(data1,amphibians, by = c("Entity"))
data1 <- data1 %>% 
  mutate(suma = Mammals..total.endemic.+Amphibians..total.endemic.+Birds..total.endemic.)

country <- map_data("world")

data1 <- data1 %>% 
  mutate(nazwa = ifelse(Entity == "United States","USA",
                        ifelse(Entity == "United Kingdom","UK",
                               ifelse(Entity == "Democratic Republic of Congo",
                               "Democratic Republic of the Congo",
                               ifelse(Entity == "Czechia","Czech Republic", 
                                      ifelse(Entity == "Cote d'Ivoire","Ivory Coast",
                                             ifelse(Entity =="Congo","Republic of Congo",Entity))))))) %>% 
  mutate(maxymalne = ifelse(Entity %in% c("Indonesia","Brazil","Australia"), suma, ""))

country %>% 
  left_join(data1, by = c("region"="nazwa")) %>% 
  ggplot(aes(long,lat)) +
  geom_polygon(aes(group = group, fill = suma)) +
  labs(title = "Endemiczne gatunki ssaków, ptaków i płazów",
       fill = "Liczba - skala logarytmiczna") +
  scale_fill_gradientn(trans = "log2",colours = terrain.colors(2,rev = TRUE),na.value = "white") +
  theme(panel.background = element_rect(fill = "darkslategray3"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  coord_map(xlim=c(-180,180),ylim=c(-45,180), "mercator") 
  


