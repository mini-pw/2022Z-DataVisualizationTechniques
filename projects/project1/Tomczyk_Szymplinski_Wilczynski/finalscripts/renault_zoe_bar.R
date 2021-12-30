# data: https://ourworldindata.org/grapher/co-emissions-by-sector
#       https://www.jato.com/new-car-co2-emissions-hit-the-highest-average-in-europe-since-2014/

library(ggplot2)
library(dplyr)

total_emission <- read.csv("../plotsData/co-emissions-by-sector.csv")
electricity_generation <- read.csv("../plotsData/electricity-generation.csv")
countreis_co2 <- read.csv("../plotsData/countries_co2.csv", stringsAsFactors = FALSE)

electricity_emission <- total_emission %>% 
  filter(Year == 2016) %>% 
  select(Entity,Electricity...Heat..CAIT..2020.) %>% 
  rename(CO2EmissionInTonnes = Electricity...Heat..CAIT..2020.)

electricity_generation_2016 <- electricity_generation %>% 
  filter(Year == 2016) %>% 
  select(Entity, Electricity.Generation..TWh.) %>% 
  rename(ElectricityGenerationInTwh = Electricity.Generation..TWh.)

emissions_per_twh  <- electricity_emission %>% 
  inner_join(electricity_generation_2016, by='Entity') %>% 
  mutate(RenaultZoeEmission = CO2EmissionInTonnes * 164.67 / (ElectricityGenerationInTwh * 1000000))

chosen_countries <- as.character(countreis_co2[,1])

emissions_per_twh <- emissions_per_twh %>% filter(Entity %in% chosen_countries)

CO2_per_twh_chosen_countries <- emissions_per_twh %>%
  filter(Entity %in% chosen_countries) %>%
  rename(Country = Entity)

df <- CO2_per_twh_chosen_countries %>% 
  filter(CO2_per_twh_chosen_countries$Country %in% c("France", "Italy", "Poland", "Norway")) %>% 
  mutate(Country = forcats::fct_reorder(Country, RenaultZoeEmission))

ggplot(df, aes(x = Country, y = RenaultZoeEmission, fill = Country)) +
  geom_col() + 
  scale_fill_manual(values = c(rgb(0.2,0.5,0.5,0.4), rgb(0.7,0.5,0.1,0.4),
                               rgb(0.8,0.2,0.5,0.4) , rgb(0.6,0.2,0.7,0.2))) +
  coord_flip() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(), 
    legend.position = NULL
  )
