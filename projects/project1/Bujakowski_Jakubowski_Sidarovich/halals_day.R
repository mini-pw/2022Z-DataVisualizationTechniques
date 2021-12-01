library(dplyr)
require(maps)
library(stringr)
library(ggplot2)
library(broom)
library(rgdal)

poland_spdf <- readOGR( dsn= "~/studia/sem 3/Techniki Wizualizacji/PROJEKT 1/Polska_Noise/poland_map" , layer="poland_administrative_boundaries_province_polygon", verbose=FALSE) 
poland_spdf$name <- sub('.*\\ ', '', poland_spdf$name)
poland_spdf$name[c(2, 3, 6, 8, 12, 14)] <- c("małopolskie", "śląskie", "dolnośląskie", "łódzkie", "świętokrzyskie", "warmińsko-mazurskie")
str(poland_spdf)
spdf_fortified <- tidy(poland_spdf, region = "name")


halas <- read.csv2("halas.csv")
halas %>% group_by(Nazwa.odcinka.drogi)%>% 
  summarise(Srednia = mean(Laeq.po.korekcie..dB., na.rm = T)) %>% 
  filter(Srednia >= 70)
halas %>%
  filter(grepl("^Noc", Czas.odniesienia)) %>%  
  group_by(Województwo)%>% 
  summarise(Srednia = mean(Laeq.po.korekcie..dB., na.rm = T)) -> halas

spdf_fortified = spdf_fortified %>%
  inner_join(halas, by=c("id"="Województwo"))

ggplot() +
  geom_polygon(data = spdf_fortified, colour = "white",
               aes(fill = Srednia, x = long, y = lat, group = group), size = 0.5) +
  theme_void() +
  coord_map() +
  scale_fill_gradient2(low = "lightblue", high = "darkblue",
                       mid = "dodgerblue", midpoint = 62) + 
  labs(title = "Daytime Road Noise Pollution in Poland", 
       subtitle = "Distribution by voivodeship",
       fill = "Mean LAeq, dB") +
  theme(plot.title = element_text(face = "bold", size = 16, color  = "white"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic",
                                     size = 14, color  = "white"),
        legend.title = element_text(size = 10, color  = "white"),
        legend.text = element_text(color = "white")) +
  theme(legend.background = element_rect(fill = "transparent", color  = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) -> ggp_transparent2
ggp_transparent2
  ggsave(ggp_transparent2,            # Save transparent png file
         filename = "night_noise.png",
         bg = "transparent")
