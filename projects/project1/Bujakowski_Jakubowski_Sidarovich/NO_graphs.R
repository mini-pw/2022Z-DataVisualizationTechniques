library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(tidyverse)
library(patchwork)

#datasets loading and structurizing
df <- read.csv('pollutants.csv')
df
colnames(df) <- c("pollutant", "total", "sth", "others", "road.exhaust", "road.nonexhaust",
                  "railways", "domestic.shipping", "intl.shipping", "dm.aviation", "intl.aviation")

df <- df %>% 
  filter(pollutant == "NOx") %>% 
  mutate(transport = (road.exhaust + road.nonexhaust + railways + domestic.shipping + intl.shipping + dm.aviation)) %>% 
  select(pollutant, transport, others) %>%  
  pivot_longer(!pollutant, names_to = "sector", values_to = "count")
df
df2 <- read.csv("data_pollutants.csv", sep = ";")
df2 <- df2 %>% filter(Pollutant == "NOx")

# wykres % wkladu sektoru transportu w calkowita emisje NO i NO2 przez przemysl
p1 <- df %>%
  ggplot(aes(x = pollutant, y = count, fill = sector)) +
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = c("#261269", "#bacbe6")) + 
  labs(#title = "Contribution of the transport sector to emissions of the NOx air pollutants", #Contribution of the transport sector to total emissions of the NOx air pollutants
       x = "", 
       y = "") + 
  theme_light() + 
  coord_flip() + 
  theme(#plot.title = element_text(hjust = 0.5, size = 13, face = "bold", color = "white"),
        axis.title = element_text(color = "white", size = 11),
        axis.text = element_text(color = "white", size = 12),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white", size = 13),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white"),
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  scale_y_continuous(expand = c(0,0), labels=scales::percent) + 
  scale_x_discrete(expand = c(0.01,0)) + 
  geom_abline(slope=0, intercept=0.5) 
  

p1

ggsave(p1,
       filename = "NOx_distribution.png",
       bg = "transparent",
       width = 8,
       height = 1.2
       )

#wykres regresji liniowej emisji tlenkow azotu NO i NO2
p2 <- df2 %>% 
  ggplot(aes(x = Year, y = Emissions, color=Pollutant)) + 
  geom_point(size = 3, show.legend = FALSE, color = c("#45ccf5")) +
  stat_smooth(method = lm, color = c("darkblue"), fill = "#261269") + 
  labs(#title = "Emission of highly dangerous NO and NO2 air pollutants by transport sector in Poland",
       #subtitle = "chart created with linear regression method",
       x = "Year", 
       y = "Gigagrams [1000 tones]") + 
  theme_light() + 
  scale_x_continuous(expand = c(0.02,0), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(breaks = seq(180, 300, 10)) + 
  theme(#plot.title = element_text(hjust = 0.5, face = "bold", color = "white", size = 14),
        #plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "white"),
        axis.title = element_text(color = "white", size = 13),
        axis.text = element_text(color = "white", size = 12),
        legend.position = "none", ## transparent
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))
p2

ggsave(p2,
       filename = "NOx_graph_transparent.png",
       bg = "transparent")

ps <- plot_spacer() 

p0 <- (p2 / ps / p1)  + patchwork::plot_layout(heights = c(15,0.25,1.25))
p0


ggsave(p0, filename = "NOx_transparent.png",  bg = "transparent")
p0
