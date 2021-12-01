library(ggplot2)
library(dplyr)
#setwd("~/Projekty/twd/projekt1")

path <- file.path("data", "disasterlocations", "disasterlocations.csv")

df <- read.csv(path) %>% 
  filter(disastertype != "mass movement (dry)",
         disastertype != "landslide",
         disastertype != "volcanic activity")

df_temp <- df %>% 
  filter(disastertype=="extreme temperature ") #%>% 
  #sample_frac(0.3)
df_notemp <- filter(df ,disastertype!="extreme temperature ")
  

ggplot(map_data("world")) +
  geom_polygon(aes(long, lat, group=group), fill="#555555") +
  geom_point(
    data = filter(df_temp, year > 2010),
    mapping = aes(longitude, latitude, color = disastertype),
    size = 0.5
  ) +
  geom_point(
    data = filter(df_notemp, year > 2010),
    mapping = aes(longitude, latitude, color = disastertype),
    size = 0.5
  ) +
  scale_color_manual(
    values = c("#fdffb6", "#dab894", "#f08080", "#a0c4ff", "#ffc6ff"),
    guide = guide_legend(override.aes = list(size = 5)),
    labels = c("Susza", "Trzęsienie ziemi", "Ekstremalna temperatura", "Powódź", "Burze i huragany")
  ) +
  labs(
#    title = "Miejsca wystąpień katastrof naturalnych",
#    subtitle = "w latach 2010 - 2018",
    color = "Rodzaj katastrofy",
#    caption = "The GDIS data descriptor: Rosvold, E.L., Buhaug, H. GDIS, a global dataset of geocoded disaster locations.",
    x = NULL,
    y = NULL
  ) + 
  scale_x_continuous(guide = NULL) + 
  scale_y_continuous(guide = NULL, limits = c(-55,90)) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    plot.title = element_blank(), #element_text(hjust = 0.5),
    plot.subtitle = element_blank(), #element_text(hjust = 0.5),
    legend.position = "bottom",
    text=element_text(size=20, family="sans", face="plain")
  )

path <- file.path("plots")
ggsave(filename = "map.png", path = path, bg = "transparent", width=35, height=20, units="cm")
