library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)

data4 <- read.csv("dane/energy-consumption-by-source-and-region.csv")
world_map <- map_data("world") %>% filter(region != "Antarctica")
energyConsumptionData <- left_join(world_map, data1_2016, by = "region")




data4_2004 <- data4 %>% filter(Year == 1990) %>% mutate(SumaNieodnawialnych = Oil.Consumption...EJ + Gas.Consumption...EJ + Coal.Consumption...EJ + Nuclear.Consumption...EJ + Biofuels..TWh.) %>%
  mutate(SumaOdnawialnych = Solar.Consumption...EJ + Hydro.Consumption...EJ + Geo.Biomass.Other...EJ + Wind.Consumption...EJ) %>%
  mutate(SumaWszystkich = SumaNieodnawialnych + SumaOdnawialnych) %>%
  mutate(Stosunek = 100*(SumaOdnawialnych/SumaWszystkich)) %>% 
  mutate(Entity = replace(Entity, Entity == "United States", "USA")) %>%
  mutate(Entity = replace(Entity, Entity == "United Kingdom", "UK")) %>%
  mutate(Entity = replace(Entity, Entity == "Czechia", "Czech Republic"))

data4_2015 <- data4 %>% filter(Year == 2019) %>% 
  mutate(SumaNieodnawialnych = Oil.Consumption...EJ + Gas.Consumption...EJ + Coal.Consumption...EJ + Nuclear.Consumption...EJ + Biofuels..TWh.) %>%
  mutate(SumaOdnawialnych = Solar.Consumption...EJ + Hydro.Consumption...EJ + Geo.Biomass.Other...EJ + Wind.Consumption...EJ) %>% 
  mutate(SumaWszystkich = SumaNieodnawialnych + SumaOdnawialnych) %>%
  mutate(Stosunek = 100*(SumaOdnawialnych/SumaWszystkich)) %>% 
  mutate(Entity = replace(Entity, Entity == "United States", "USA")) %>%
  mutate(Entity = replace(Entity, Entity == "United Kingdom", "UK")) %>%
  mutate(Entity = replace(Entity, Entity == "Czechia", "Czech Republic"))

StosunekOdnDoNodn2004 <- left_join(world_map, data4_2004, by = c("region" = "Entity"))
StosunekOdnDoNodn2015 <- left_join(world_map, data4_2015, by = c("region" = "Entity"))



data_do_pop_mapki_1990 <- data4_2004 %>% select(Entity, Year, Stosunek) %>% rename(Stosunek_1990 = Stosunek)
data_do_pop_mapki_2019 <- data4_2015 %>% select(Entity, Year, Stosunek) %>% rename(Stosunek_2019 = Stosunek)
data_do_pop_mapki_final <- left_join(data_do_pop_mapki_2019, data_do_pop_mapki_1990, by = "Entity") %>%
  select(Entity, Stosunek_2019, Stosunek_1990) %>% mutate(Roznica = Stosunek_2019 - Stosunek_1990)
mapka_dane <- left_join(world_map, data_do_pop_mapki_final, by = c("region" = "Entity")) %>% mutate(Roznica  = replace(Roznica, region == "Sri Lanka", NA))

wykres_mapka_stosunek_2019_1990 <- ggplot(mapka_dane, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Roznica), color = "black", size = 0.05) + scale_fill_gradientn(colours = c("#FF0000", "#FFFFFF", "#41B619"), breaks = c(-5, 0, 5, 10, 15, 20, 25), limits = c(-6, 25),  values = scales::rescale(c(-6, 0, 25))) +
  ggtitle("Zmiana udzialu odnawialnej energii w calkowitym jej zuzyciu na przestrzeni lat 1990-2019 ") +
  xlab("") + 
  ylab("") + 
  theme(axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks = element_blank(),
                              rect = element_blank()) +
  coord_map("gall", lat0 = 0, xlim = c(-180, 180))

wykres_mapka_stosunek_2019_1990

