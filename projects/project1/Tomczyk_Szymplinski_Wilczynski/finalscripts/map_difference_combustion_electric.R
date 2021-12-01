#data:   https://ourworldindata.org/grapher/co-emissions-by-sector
#       https://ourworldindata.org/grapher/new-passenger-vehicles-type
#       https://www.jato.com/new-car-co2-emissions-hit-the-highest-average-in-europe-since-2014/
#       https://ec.europa.eu/eurostat/web/transport/data/database

require(maps)
library(ggplot2)
library(dplyr)
library(stringi)

total_emission <- read.csv("../PlotsData/co-emissions-by-sector.csv")
electricity_generation <- read.csv("../PlotsData/electricity-generation.csv")
countreis_co2 <- read.csv("../PlotsData/countries_co2_2.csv", stringsAsFactors = FALSE)
electric_vehicles <- read.csv("../PlotsData/number_of_evs.csv", stringsAsFactors = FALSE)
all_vehicles <- read.csv("../PlotsData/number_of_total.csv")
missing_vehicles <- read.csv("../PlotsData/new-passenger-vehicles-type.csv")

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

final_df <- emissions_per_twh %>% left_join(countreis_co2, by = c("Entity" = "region")) %>% 
  select(Entity, RenaultZoeEmission, emission)

electric_vehicles <- electric_vehicles %>% filter(TIME == 2019)
new_row <- electric_vehicles[electric_vehicles$GEO == "United Kingdom",]
new_row["GEO"] <- "UK"
electric_vehicles <- rbind(electric_vehicles, new_row)

all_vehicles <- all_vehicles %>% filter(TIME == 2019)
new_row1 <- all_vehicles[all_vehicles$GEO == "United Kingdom",]
new_row1["GEO"] <- "UK"
all_vehicles <- rbind(all_vehicles, new_row1)

missing_vehicles<- missing_vehicles %>%  filter(Year == 2019)
new_row_2 <- missing_vehicles[missing_vehicles$Entity == "United Kingdom",]
new_row_2["Entity"] <- "UK"
missing_vehicles <- rbind(missing_vehicles, new_row_2)

# missing vehicles = data for Austria, Greece and UK (cause there were in other 
#df used for first prototype, but this df doesn't have other countries, like Poland, Czechia,
# Romania etc.)

missing_vehicles <- missing_vehicles %>% 
  filter(Entity %in% c("Austria", "UK", "Greece")) %>%
  group_by(Entity) %>% 
  mutate(all_number = sum(battery_electric_number, plugin_hybrid_number, full_mild_hybrid_number,
                          diesel_gas_number, petrol_number)) %>% 
  ungroup() %>% 
  mutate(ev_percent = 100 * battery_electric_number/all_number) %>% 
  select(Entity, ev_percent)

#vehicles <- new_vehicles %>% 
#  group_by(GEO)
  
electric_vehicles$Value <- as.numeric(gsub(",","",electric_vehicles$Value))
all_vehicles$Value <- as.numeric(gsub(",","",all_vehicles$Value))

vehicles <- inner_join(electric_vehicles, all_vehicles, by = "GEO") %>% 
  select("GEO","Value.x","Value.y") %>% 
  rename(Electric_cars = Value.x,All_cars = Value.y, Entity = GEO)%>% 
  mutate(ev_percent = 100 * Electric_cars/All_cars)
vehicles <- vehicles %>%  select("Entity", "ev_percent")
vehicles <- vehicles %>%  filter(!Entity %in% c("Greece", "UK", "Austria"))
vehicles <- rbind(vehicles,missing_vehicles)

final_df <- final_df %>% 
  full_join(vehicles, by = "Entity") %>% 
  mutate(balance = case_when(
         emission != 0 ~ (emission*100 - RenaultZoeEmission*ev_percent)/(100 - ev_percent) - RenaultZoeEmission ))
# final_df <- na.omit(final_df)
final_df <- final_df %>% 
  filter(Entity != "United Kingdom")

chosen_countries <- as.character(final_df[,1])

chosen_countries[chosen_countries == "Norway"] <- "Norway(?!:Svalbard)"
chosen_countries[chosen_countries == "Russia"] <- "Russia(:32)"

europe <- map_data("world", region = chosen_countries)


max = max(final_df[,"balance"], na.rm = TRUE)
min = min(final_df[,"balance"], na.rm = TRUE)


# ----------------- plot ----------------
map.plot <- europe %>% 
  left_join(final_df, by = c("region" = "Entity")) %>% 
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group, fill = balance)) +
  scale_fill_gradient2(low = "#94e9ff", high = "#0080a1",
                       limits = c(min, max)) +
  borders(region = chosen_countries, size = 0.25, colour = "#000000") +
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  ) + labs(
    title = "How much CO2 does combustion engine cars emit more than electric cars",
    fill = "CO2 emission (g/km)"
  )
map.plot
# Saving a plot
ggsave(filename = "How much CO2 does combustion engine cars emit more than electric cars.svg",
       plot = map.plot,
       device = "svg",
       path = "C:/Users/micha/OneDrive/Dokumenty",
       height = 15,
       width = 22,
       units = "cm",
       dpi = 200)
