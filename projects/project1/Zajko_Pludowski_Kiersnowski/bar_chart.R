library(dplyr)
library(ggplot2)


meat_data <- read.csv('data/WorldMeatProductionDatasets/global-meat-production-by-livestock-type.csv')


# Weights source: https://www.greeneatz.com/foods-carbon-footprint.html

meat_summary <- meat_data %>% 
                  group_by(Year) %>% 
                  summarise(Sheep = sum(Sheep.and.Goat..tonnes., na.rm = T)*39.2,
                            Beef = sum(Beef.and.Buffalo..tonnes., na.rm = T)*27,
                            Pork = sum(Pigmeat..tonnes., na.rm = T)*12.1,
                            Poultry = sum(Poultry..tonnes., na.rm = T)*6.9) %>% 
                  transmute(Year = Year, 
                            'Carbon footprint of meat' = Sheep + Beef + Pork + Poultry)

meat_summary

co2_world_data <- read.csv('data/world_ghg_emission.csv')

data_to_plot = co2_world_data %>% 
                left_join(meat_summary, by='Year') %>% 
                mutate(GHG.emitted = GHG.emitted*1000000/1e9,
                       `Carbon footprint of meat` = `Carbon footprint of meat`/1e9)

data_to_plot %>% 
  ggplot() +
    geom_segment(aes(x = Year, xend = Year, 
                     y = `Carbon footprint of meat`, yend = GHG.emitted), 
                     color = 'grey') + 
    geom_point(aes(x = Year, y = `Carbon footprint of meat`, color = 'World summary'), size = 3) + 
    geom_point(aes(x = Year, y = GHG.emitted, color = 'Meat production'), size = 3) + 
    theme(legend.position = 'right') +
    scale_x_discrete(limits = 1990:2018, expand = c(.01, 0)) +
    scale_y_continuous(expand = c(.05, 0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
    expand_limits(y = 0) +
    labs(x = 'Year', y = 'Gigatons of GHG', 
         title = 'Comparison of GHG emitted by meat production to total world GHG emission',
         colour='Legend') +
    scale_color_manual(values = c('orange', '#1f77b4'), labels = c('World summary', 
                                                                   'Meat production'))

    
ggsave('plots/ghg_emission_plot.png',  width = 16, height = 8)

