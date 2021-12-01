data <- read.csv("global-energy-substitution_v2.csv")

data <- data %>% 
  select(-c(1,2))

colnames(data)[c(6,7)] = c("Traditional biomass", "Other renewables")

data <- melt(data, id="Year")

correct_order = rev(c("Traditional biomass", "Coal", "Oil", "Gas", "Nuclear",
                      "Hydropower", "Wind", "Solar", "Biofuels", "Other renewables"))

data <- data %>% 
  dplyr::rename(Source = variable) %>% 
  mutate(Source = factor(Source, levels = correct_order))


pal <- c(RColorBrewer::brewer.pal(n = 9,name = "Greens"),'#000000')

ggplot(data, aes(x=Year, y=value, fill=Source)) + 
  geom_area() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid =element_line(color = "white",size = 0.05,linetype = 1),
        legend.key.size = unit(1.1, 'cm'))+
  scale_fill_viridis_d() +
  ggtitle("Energy consumption by source, World") +
  xlab("") +
  ylab("Energy consumption [TWh]") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(xintercept=1979, linetype='dotted', col = 'red',size=2)+
  geom_vline(xintercept=2008, linetype='dotted', col = 'red',size=2)+
  annotate("text", x = 1975, y = 130000, label = "1979", vjust = -0.5)+
  annotate("text", x = 2004, y = 163000, label = "2008", vjust = -0.5)
