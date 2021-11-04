library(ggplot2)
library(dplyr)
library(readxl)

# Path to xlsx file
xl <- "D:\\Hubert\\Dokumenty\\Politechnika Warszawska\\IIAD\\Semestr III\\WD\\Laby\\HW2\\owid-covid-data.xlsx"


# Choosing data only younger than 2 weeks ago, and adjusting location to mach region data from world map
read_excel(xl) %>% 
  select(location, date, new_cases) %>% 
  filter(date>="2021-10-21" & date <="2021-11-03") %>% 
  mutate(location = case_when(location == "United States" ~ "USA",
                              location == "United Kingdom" ~ "UK",
                              location == "Samoa" ~ "American Samoa",
                              location == "Czechia" ~"Czech Republic",
                              location == "Micronesia (country)" ~ "Micronesia",
                              location == "Georgia" ~ "South Georgia",
                              location == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
                              location == "British Virgin Islands" ~ "Virgin Islands",
                              TRUE ~ location)) %>% 
  group_by(location) %>% 
  summarise(sum.from.2weeks = sum(new_cases)) -> d1

# Adjusting region data to match d1 location 
map_data("world") %>% 
  mutate(region = case_when(region == "Antigua" ~ "Antigua and Barbuda",
                            region == "Barbuda" ~ "Antigua and Barbuda",
                            region == "Democratic Republic of the Congo" ~ "Congo",
                            region == "Republic of the Congo" ~ "Congo",
                            region == "Nevis" ~ "Saint Kitts and Nevis",
                            region == "Saint Kitts" ~ "Saint Kitts and Nevis",
                            region == "Bonaire" ~ "Bonaire Sint Eustatius and Saba",
                            region == "Sint Eustatius" ~ "Bonaire Sint Eustatius and Saba",
                            region == "Saba" ~ "Bonaire Sint Eustatius and Saba",
                            region == "Trinidad" ~ "Trinidad and Tobago",
                            region == "Tobago" ~ "Trinidad and Tobago",
                            region == "Saint Vincent" ~ "Saint Vincent and the Grenadines",
                            region == "Grenadines" ~ "Saint Vincent and the Grenadines",
                            TRUE ~ region)) -> world_map

# Joining d1 and World by country (region == location)
merge(world_map, d1, by.x="region", by.y="location",all.x = TRUE) %>% 
  arrange(order)  %>%
  mutate(corrected.sum = ifelse(is.na(sum.from.2weeks)| sum.from.2weeks<=0,-1,sum.from.2weeks)) %>% 
  filter(corrected.sum!=-1) -> data

# Drawing world map and mapping new cases
ggplot() +
  geom_map(
    data = world_map, map = world_map,
    aes(long, lat, map_id = region),
    color = "black", fill = 'grey',size = 0.1
  ) + 
  theme_bw() +
  geom_map(
    data = data, map = data,
    aes(long, lat, map_id = region, fill = log10(corrected.sum)),
    color = "black",size = 0.1
  ) +
  scale_fill_gradient(
    low = "yellow", high = "red"
  ) +
  labs(title = "New cases of COVID-19 by country (from '21-10-2021' to '03-11-2021')", x = "Longitude",y = "Latitude" ,fill = "New cases (Log10)")

