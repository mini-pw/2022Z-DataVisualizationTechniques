library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(data.table)


energyCost <- read.csv("dane/levelized-cost-of-energy.csv")
energyCost <- energyCost %>% filter(Entity == "World", Year >= 2010) %>%
  select(-Entity, -Code, -Geothermal.LCOE..2019.USD.kWh.) %>%
  rename("Skoncentrowana energia słoneczna" = CSP.LCOE..2019.USD.kWh.,
         "Energia z wiatru na morzu" = Offshore.wind.LCOE..2019.USD.kWh.,
         "Energia z fotowoltaiki" = Solar.LCOE..2019.USD.kWh.,
         "Bioenergia" = Bioenergy.LCOE..2019.USD.kWh.,
         "Energia z wiatru na lądzie" = Onshore.wind.LCOE..2019.USD.kWh.,
         "Energia wodna" = Hydro.LCOE..2019.USD.kWh.)

EC <- melt(as.data.table(energyCost), id.vars = c("Year"),
           measure.vars = c("Skoncentrowana energia słoneczna", "Energia z wiatru na morzu",
                            "Energia z fotowoltaiki","Bioenergia","Energia z wiatru na lądzie", "Energia wodna"))

my_colors <- RColorBrewer::brewer.pal(12, "Paired")[c(8,9,7,4,10,2)]


EC %>% rename(Technology = variable) %>%
  ggplot( aes(x=as.factor(Year), y=value, group=Technology, color=Technology)) +
  
  geom_hline(
    mapping=aes(yintercept=0.05),
    color="ivory", linetype="dashed", size=1.2
  ) +
  geom_hline(
    mapping=aes(yintercept=0.17),
    color="ivory", linetype="dashed", size=1.2
  ) +
  scale_fill_manual("Paliwa kopalne ", values=rep(1,2),
                    guide=guide_legend(
                      override.aes = list(colour="ivory", linetype = 3),
                      order=0
                    ),
                    limits = c("Zakres cen")
  ) +
  
  geom_line(size = 1.5, show_guide = TRUE) +
  scale_color_manual(values = c("orangered1", "mediumpurple1", "sienna1", "chartreuse1",
                                "deeppink", "turquoise1"), name = "Energie odnawialne") +
  ggtitle("Koszt produkcji energii") +
  theme_modern_rc(axis_title_size = 13, axis_text_size = 11) +
  theme(axis.title.x = element_blank(), legend.title=element_text(size=11.5), 
        legend.text=element_text(size=10.5)) +
  scale_y_continuous(breaks = seq(0, 0.35, by = 0.05)) +
  ylab("$/kWh")

