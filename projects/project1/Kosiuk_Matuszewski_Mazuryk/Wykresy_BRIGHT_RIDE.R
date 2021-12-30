# WYKRES 1: AVERAGE GCO2/KM EMISSIONS BY CARS REGISTERED IN 2020

library(geojsonio)
library(dplyr)
library(broom)
library(ggplot2)
library(viridis)

spdf <- geojson_read("https://raw.githubusercontent.com/leakyMirror/map-of-europe/master/GeoJSON/europe.geojson",  what = "sp")

spdf_fortified <- tidy(spdf)

avg_em_by_country <- read.csv("avg emissions by country (gCO2_km) - NEDC.csv")
avg_em_by_country <- avg_em_by_country %>% 
  filter(ď.żCountries != "IE")

avg_em_by_country <- cbind(avg_em_by_country,ID = c(5,37,6,9,25,11,32,22,24,18,39,41,15,10,42,17,46,21,29,16,28,12,13,38,7,43,35,19,36))
colnames(avg_em_by_country) <- c("Countries","CO2.emissions","ID")

spdf_fortified$id <- as.double(spdf_fortified$id)
spdf_fortified = spdf_fortified %>%
  left_join(avg_em_by_country, by = c("id"="ID"))

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = CO2.emissions, x = long, y = lat, group = group) , size=0, alpha=0.9,color="black") +
  theme_void() +
  scale_fill_gradientn(na.value = "lightgrey",colours = c("white","#B6F9C9","#4B7F52")) +
  #labs(
  # title = "Średnia emisja dwutlenku węgla na kilometr przez samochody rejestrowane w 2020 roku"
  #  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, l=-99, unit = "cm") )
  ) +
  coord_map()


# WYKRES 2: HOW MANY CARS DRIVE ON CLEAN ENERGY

EV<-mutate(EV, CleanEnergy = CarAmount*CleanEnergyUsage/100, OtherEnergy = CarAmount*(100-CleanEnergyUsage)/100)

EV %>% select(Country,OtherEnergy,CleanEnergy) %>%  
  pivot_longer(!Country,names_to = 'Type', values_to = 'Value') %>%
  mutate(Country = fct_reorder(Country,Value)) %>%
  ggplot(aes(fill=fct_rev(Type), y=Value, x=Country)) +
  geom_bar(position="stack", stat="identity")+
  labs(title = "Number of EV that drive on clean energy",
       fill = "Type",
       y = "Amount (millions)",
       x = "Country")

# WYKRES 3: STOCK VALUE
stock <- read.csv("IEA-EV-dataEV stockCarsHistorical.csv")

stock1 <- stock %>%
  filter(region == 'World' & year>2012) %>%
  group_by(year) %>% summarise(Value = sum(value)/10000)

ggplot(data = stock1, aes(x = year, y = Value))+
  geom_line()+
  labs(title = "Stock Value ",
       subtitle = "of electric cars market",
       y = "Value (mln$)",
       x = "Year")

# WYKRES 4: CHARGING STATIONS

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)
StationsUS <- read.csv("C:/Users/wojtek/Desktop/pw/Techniki wizualizacji danych/Projekt1/alt_fuel_stations (Oct 19 2021).csv")

StationsUS %>% count(State)

StationsUS %>%
  mutate(rok = format(as.Date(StationsUS$Open.Date),format = "%Y")) %>%
  count(rok) 

st <- StationsUS %>%
  mutate(rok = as.integer(format(as.Date(StationsUS$Open.Date),format = "%Y"))) %>%
  filter(rok >= 2010)



ggplot(st, aes(x=rok)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=2) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  ) + 
  labs(title = "Charging Stations Creation Date", subtitle = "Number of new stations in years 2010-2021", x = "Year", y = "Number of new stations")