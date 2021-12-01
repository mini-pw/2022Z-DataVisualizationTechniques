library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)


# energy
# source: https://github.com/owid/energy-data
energy <- read.csv('data/energy-data-master/owid-energy-data.csv')
energy_codebook <- read.csv('data/energy-data-master/owid-energy-codebook.csv')

# continents
# source: https://datahub.io/JohnSnowLabs/country-and-continent-codes-list
continents <- read.csv("data/countries_continents.csv", header = TRUE)
continents <- continents %>% 
  select(Three_Letter_Country_Code, Continent_Name)

colnames(continents) <- c("iso_code", "continent_name")


# gdp 
# source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
gdp <- read.csv("data/gdp_per_capita.csv", check.names = FALSE)
gdp <- gdp %>% 
  select(!c(`Indicator Name`, `Indicator Code`)) %>% 
  pivot_longer(!c(`Country Name`, `Country Code`), names_to = "Year", values_to = "GDP per capita") %>% 
  mutate(Year = as.numeric(Year))

colnames(gdp) <- c("country", "iso_code", "year", "gdp_per_capita")


p <- energy %>% 
  select(country, iso_code, year, renewables_share_energy) %>% 
  left_join(gdp, by = c("iso_code", "year"), keep = FALSE) %>% 
  left_join(continents, by = "iso_code") %>% 
  filter(year == 2018, continent_name == "Europe") %>% 
  mutate(labeled = (gdp_per_capita == max(gdp_per_capita, na.rm = TRUE)) | renewables_share_energy %in% tail(sort(renewables_share_energy), 2)) %>%  
  ggplot(aes(x = gdp_per_capita, y = renewables_share_energy, label = ifelse(labeled, country.x, ""))) + 
  geom_point() + 
  geom_text_repel() + 
  geom_smooth(method='lm', formula= y~x, se = FALSE) + 
  stat_smooth(method='lm', formula= y~x, fullrange = TRUE, geom='ribbon', aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..), ymax=ifelse(..ymax.. > 80, 80, ..ymax..)), alpha = 0.07) + 
  labs(
    title = "Ecological energy share vs GDP per capita", 
    subtitle = "Year 2018",
    x = "GDP per capita", 
    y = "Ecological enegy share"
  ) + 
  scale_y_continuous(labels = label_number(suffix = '%')) + 
  scale_x_continuous(labels = label_dollar()) + 
  theme(
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey"),
    panel.grid.minor = element_line(color = "grey"), 
  )

p
