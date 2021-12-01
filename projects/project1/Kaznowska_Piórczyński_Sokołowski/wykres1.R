library(dplyr)
library(ggplot2)
library(maps)


# energy
# source: https://github.com/owid/energy-data
energy <- read.csv('data/energy-data-master/owid-energy-data.csv')
energy_codebook <- read.csv('data/energy-data-master/owid-energy-codebook.csv')

# continents
# source: https://datahub.io/JohnSnowLabs/country-and-continent-codes-list
continents <- read.csv("data/countries_continents.csv", header = TRUE)
continents <- continents %>% 
  select(Three_Letter_Country_Code, Continent_Name)
colnames(continents) <- c("iso_code", "continent")


# countries
europe_countries <- energy %>% 
  select(iso_code, country) %>% 
  distinct() %>% 
  left_join(continents, by = "iso_code") %>%
  filter(continent == "Europe") %>% 
  pull(country)

europe_countries
europe_countries[europe_countries == "Czechia"] <- "Czech Republic"
europe_countries[europe_countries == "United Kingdom"] <- "UK"

asia_countries <- c("Armenia", "Azerbaijan", "Kazakhstan", "Georgia", "Faeroe Islands")

chosen_countries <- setdiff(europe_countries, asia_countries)
chosen_countries[chosen_countries == "Norway"] <- "Norway(?![:Jan Mayen :Svalbard])"
chosen_countries[chosen_countries == "Russia"] <- "Russia(:32)" # Kaliningrad
chosen_countries

# map
europe <- map_data("world", region = chosen_countries) 
palette <- c("#FFF59D", "#DCE775", "#8BC34A", "#388E3C")


renewables_energy_df <- energy %>% 
  select(country, year, renewables_share_energy) %>% 
  filter(year == 2018) %>% 
  mutate(country = ifelse(country == "Czechia", "Czech Republic", country)) %>% 
  mutate(country = ifelse(country == "United Kingdom", "UK", country)) %>% 
  mutate(eco = cut(renewables_share_energy,
                   breaks = c(0, 7, 14, 22, 100),
                   labels = c("< 7%", "7% - 14%", "14% - 22%", ">22%"),
                   right = TRUE))

p <- europe %>% 
  right_join(renewables_energy_df, by = c("region" = "country")) %>% 
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group, fill = eco)) + 
  borders(region = chosen_countries, size = 0.1, colour = "#000000") +
  scale_fill_manual(values = palette) +
  coord_map() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.margin = unit(c(0.5,1,0.75,0.75), "cm"),
    legend.title = element_text(size = 15, face = 'bold'),
    plot.background = element_blank(),
    panel.background = element_blank()
  ) + 
  labs(
    title = "Ecological energy share in European countries",
    subtitle = "Year 2018",
    fill = "Ecological \nenergy \nshare" 
  )

p
