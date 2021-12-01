library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

temperatures <- read.csv("city_temperature.csv")
ghg_emmision <- read.csv("ghg-emissions.csv")
temperatures2 <- read.csv("Environment_Temperature_change_E_All_Data_NOFLAG.csv")


temperatures_mean <- temperatures %>% group_by(Year,Region) %>%
  summarise(mean_temp = mean(AvgTemperature)) %>% 
  filter(Year >= 1995)

ddf <- ghg_emmision[, -3]
ddf <- ddf %>% pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Amount", values_drop_na = TRUE)
ddf$Year <- str_remove(ddf$Year, "X")
ddf <- ddf %>% group_by(Country.Region) %>% summarise(mean_amount = mean(Amount))
colnames(ddf) <- c("region", "value")
ddf$region[ddf$region == "United States"] <- "USA"


ghg_emmision2 <- ghg_emmision[, -3]
ghg_emmision2 <- ghg_emmision2 %>% pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Amount", values_drop_na = TRUE)
ghg_emmision2$Year <- str_remove(ghg_emmision2$Year, "X")
names(ghg_emmision2)[names(ghg_emmision2)=="Country.Region"] <- "region"
population <- read.csv("population_by_sex.csv") 
names(population)[names(population)=="Time"] <- "Year"
names(population)[names(population)=="Location"] <- "region"
population$region[population$region == "United States of America"] = "USA"
population$region[population$region == "Russian Federation"] = "Russia"
population$region[population$region == "Czechia"] = "Czech Republic"
population$region[population$region == "United Republic of Tanzania"] = "Tanzania"
ghg_emmision2$region[ghg_emmision2$region == "United States"] = "USA"
ghg_emmision2$region[ghg_emmision2$region == "United Kingdom"] = "UK"
population$region[population$region == "United Kingdom"] = "UK"
population$region[population$region == "Bolivia (Plurinational State of)"] = "Bolivia"
population$region[population$region == "Venezuela (Bolivarian Republic of)"] = "Venezuela"
population$region[population$region == "Iran (Islamic Republic of)"] = "Iran"
population$region[population$region == "Congo"] = "Republic of Congo"
ghg_emmision2 <- transform(ghg_emmision2, Year = as.numeric(Year))

population_ghg<-inner_join(population, ghg_emmision2, by = c("region" = "region", "Year" = "Year")) %>%
  select(-VarID, -LocID, -MidPeriod, -PopDensity, -Variant) %>% mutate(value = 1000000*Amount/PopTotal)

population_ghg <- population_ghg %>% select(-PopTotal, -unit, -Amount)
population_ghg <- population_ghg %>% group_by(region) %>% summarise(value = mean(value))
population_ghg$region[population_ghg$region == "CĂ´te d'Ivoire"] = "Ivory Coast"


mapdata <- map_data("world") ##ggplot2
mapdata <- left_join(mapdata, population_ghg, by="region")

mapdata1<-mapdata %>% filter(!is.na(mapdata$value))

map1<-ggplot(mapdata1, aes(x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = value),color = "black")+ theme_void()

map3 <- map1 + scale_fill_gradient2(name = "Mean GHG \nper capita",
                                    low = "#004529",
                                    high =  "#de2d26", 
                                    mid = "white",
                                    midpoint = median(mapdata1$value) ,
                                    na.value = "grey50")+
  annotate("segment", x = 71, xend = 30, y = -40, yend = -23,
           colour = "black", size = 1.2, arrow = arrow())+
  annotate("label", label = "Botswana", x = 72, y = -42, size = 3) +
  
  
  annotate("segment", x = -145, xend = -110, y = 45, yend = 55,
           colour = "black", size = 1.2, arrow = arrow())+
  annotate("label", label = "Canada", x = -146, y = 43, size = 3) +
  
  
  annotate("segment", x = -145, xend = -110, y = 20, yend = 40,
           colour = "black", size = 1.2, arrow = arrow())+
  annotate("label", label = "USA", x = -146, y = 18, size = 3) +
  
  
  annotate("segment", x = 140, xend = 130, y = -50, yend = -28,
           colour = "black", size = 1.2, arrow = arrow())+
  annotate("label", label = "Australia", x = 140, y = -51, size = 3) +
  
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank(),
        panel.background = element_rect(fill = "#5C7EA8",
                                        colour = "#5C7EA8",
                                        size = 0.5, linetype = "solid"),
        panel.grid = element_blank())+
  theme(plot.background = element_rect(fill = "#5C7EA8"))

map3



