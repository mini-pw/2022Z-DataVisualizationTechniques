library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

library(dplyr)
library(tidyr)
library(SmarterPoland)
library(stringi)

library(maps)

fao <- read.csv2("data/FAO.csv", header = T, sep = ",")
food <- read.csv2("data/Food_Production.csv", header = T, sep = ",")


years <- paste("Y", 1961:2013, sep = "")

fao2013 <- fao %>% 
  pivot_longer(paste("Y", 1961:2013, sep = ""),
               names_to = "Year",
               values_to = "value") %>% 
  filter(Year == "Y2013") %>% 
  select(-Year)

typeoffood <- food %>% 
  select(Food.product, Eutrophying.emissions.per.kilogram..gPOâ..eq.per.kilogram.) %>% 
  rename(Item = Food.product)

food.summary <- fao2013 %>% 
  inner_join(typeoffood, by = "Item") %>% 
  rename(emissionCO2 = Eutrophying.emissions.per.kilogram..gPOâ..eq.per.kilogram.) %>% 
  mutate(emissionCO2 = as.numeric(emissionCO2)) %>% 
  mutate(total.emission = value * emissionCO2 * 1000) %>% 
  group_by(Area.Abbreviation, Area) %>% 
  summarise(emissionCO2 = sum(total.emission, na.rm = T)) %>% 
  rename(name = Area.Abbreviation, region = Area) %>% 
  mutate(emissionCO2 = as.integer(emissionCO2)) %>% 
  ungroup()

# add USA
food.summary[food.summary$region == "United States of America", "region"] = "USA"
 
# add China
china_val <- food.summary %>% 
  filter(stri_detect(region, regex = "China")) %>%
  summarise(emissionCO2 = sum(emissionCO2)) %>% 
  pull()

# add Russia
food.summary[food.summary$region == "Russian Federation", "region"] = "Russia"

food.summary <- food.summary %>% 
  add_row(name = "CH", region = "China", emissionCO2 = china_val)

map <- map_data("world")

worldmap <- left_join(map, food.summary)

write.csv2(worldmap, file = "./data/map_complete_data.csv")

ggplot(worldmap, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = log10(emissionCO2))) + 
  scale_fill_gradient(
    low = "#e6ffee",
    high = "#00330f",
    limits = c(0,10),
    ) +
  coord_quickmap(
    xlim = c(-170,190),
    ylim = c(-60,90)
    ) + 
  labs(title = "Emisja CO2 per capita w roku 2013",
     subtitle = "w wyniku produkcji ¿ywnoœci",
     x = "",
     y = "",
     fill = "Emisja CO2 w tonach") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) + 
  scale_x_discrete(breaks = c(), labels = c()) + 
  scale_y_discrete(breaks = c(), labels = c()) +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  theme_light()
  



