intensity <- read.csv("carbon-intensity-electricity.csv")

intensity <- intensity %>% 
  select(-1) %>% 
  dplyr::rename(region=Code)


countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
               "Czech Republic", "Denmark", "Estonia", "Finland",
               "France", "Germany", "Greece","Hungary", "Ireland",
               "Italy", "Latvia", "Lithuania","Luxemburg", "Malta",
               "Netherlands", "Poland","Portugal", "Romania",
               "Slovakia", "Slovenia","Spain", "Sweden", "UK")

EUROPE_countries <- c(countries, "Norway", "Switzerland", "Ukraine", "Belarus",
                      "Albania", "Moldova", "Serbia","Bosnia and Herzegovina",
                      "Montenegro", "Iceland", "Andorra", "Monaco",
                      "Liechtenstein", "San Marino", "North Macedonia",
                      "Kosovo")




intensity_2000 <- intensity %>% filter(Year == 2000 & region != "") %>% 
  mutate(region = countries) %>% 
  select(c(1,3))

intensity_2020 <- intensity %>% filter(Year == 2020 & region != "") %>% 
  mutate(region = countries) %>% 
  select(c(1,3))

intensity_diff <- merge(intensity_2000, intensity_2020, by= 'region') %>% 
  mutate(difference=Carbon.intensity.x-Carbon.intensity.y) %>% 
  select(-c(2:3))

maps <- map_data("world", region = EUROPE_countries)




intensity_diff_map <- maps %>% 
  left_join(intensity_diff)

intensity_map_2000 <- maps %>% 
  left_join(intensity_2000)

intensity_map_2020 <- left_join(intensity_2020, maps, by="region")

region.lab.data <- maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))



plot_diff <- ggplot(intensity_diff_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = difference ), color = "#e9e9e9")+
  # scale_fill_viridis_c(option = "B", breaks=c(200, 600, 1000), direction = -1, limits=c(0,1200),
  #                      name = "gCO2/kWh") +
  scale_fill_gradient2(name = "gCO2/kWh",low="#ff0000", mid = "white", high= "#4ac16dff", limits=c(-100,500))+
  ggtitle("Reduction in carbon intensity in the period 2000 - 2020") +
  theme_void() +
  coord_map(xlim=c(-23,40), ylim=c(31,70)) +
  easy_center_title()