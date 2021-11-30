library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(tidyr)
library(stringr)

## Loading Data
world <- ne_countries(scale = "medium", returnclass = "sf")

przystapienie <- read.csv("Dane/theMontrealProtocol_data.csv")
emisja <- read.csv("Dane/emissions_data.csv")

## Data Transformation
przystapienie <- przystapienie %>% pivot_longer(cols = starts_with("X"), values_drop_na = T, names_repair = "unique") %>% 
  filter(value == 1)

przystapienie <- world %>% merge(przystapienie, by.y = "ISO.3.Code", by.x = "iso_a3", all.x = T) %>% 
  mutate(value1 = as.numeric(str_sub(name.y,2))) %>% 
  mutate(binned_value = cut_number(value1, 4))

for (year in 2000:2014) {
  emisja[,paste("X", year, sep ="")] <- ifelse(emisja[,paste("X", year, sep ="")] == "NULL",emisja[,paste("X", year-1, sep ="")], emisja[,paste("X", year, sep ="")])
  
}
for (year in 1989:2000) {
  emisja[,paste("X", year, sep ="")] <- ifelse(emisja[,paste("X", year, sep ="")] == "NULL",emisja[,paste("X", year+1, sep ="")], emisja[,paste("X", year, sep ="")])
}


emisja <- emisja %>% mutate(zmiana = as.numeric(X2014)/as.numeric(X1989),
                            zmiana = if_else(is.nan(zmiana), 0, zmiana),
                            zmiana_bin = cut(zmiana, c(0.0,0.02,0.8,1.2,10, Inf), include.lowest = T))



emisja <- world %>% merge(emisja, by.y = "ISO.3.Code", by.x = "iso_a3")

## Plotting

ggplot(data = przystapienie, aes(fill = binned_value)) +
  geom_sf() + 
  scale_fill_discrete(type = c("#1a9850", "#a6d96a", "#d9ef8b", "#ffffbf"), na.value = "gray", labels = c("1988-1989", "1990-1992", "1993-1995", "1996-2012")) + 
  theme_minimal() +
  guides(fill = guide_legend(title = "Join year")) +
  theme(plot.background = element_rect(fill = "lightblue")) +
  labs(title = "Montreal protocol joining worldwide")



ggplot(data = emisja, aes(fill = zmiana_bin)) + 
  geom_sf() + 
  scale_fill_discrete(type = c("#1a9850", "#a6d96a", "#ffffbf", "#fdae61", "#ff2c15"), na.value = "gray", labels = c("Nearly complete reduction (98-100%)",
                                                                                                                   "Noticeable reduction (20-97%)",
                                                                                                                   "Negligible change (up to 20%)",
                                                                                                                   "Moderate increase (20-1000%)",
                                                                                                                   "Extreme increase (>1000%)",
                                                                                                                   "No data")) +
  theme_minimal() +
  guides(fill = guide_legend(title = "")) +
  theme(plot.background = element_rect(fill = "lightblue")) +
  labs(title = "Change in ozone depleting substances emission", subtitle = "from 1989 to 2014")

