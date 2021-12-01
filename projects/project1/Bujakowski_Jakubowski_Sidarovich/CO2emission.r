library(dplyr)
library(ggplot2)
library(patchwork)


p <- tmp %>% 
  group_by(Ft) %>% 
  filter(Country=='PL') %>% 
  summarise(srednia = mean(Enedc..g.km., na.rm = T)) %>%
  arrange(-srednia) %>%
  ggplot(aes(x = reorder(Ft, srednia), y = srednia)) +
  geom_col(color = 'white', fill = 'darkblue') +
  coord_flip() +
  labs(title = "",
       x = "Fuel type",
       y = "Emission [gCO2/km]") + 
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.06)))+
  theme(plot.title = element_text(color='white', size = 17, face = "bold"),
        plot.subtitle = element_text(color='white'),
        axis.title = element_text(color='white', size = 14),
        axis.text = element_text(color='white', size = 12),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color='white'),
        plot.background = element_rect(fill = "transparent", color = NA))


p1 <- df2 %>% 
  filter(Entity == 'Poland') %>% 
  ggplot(aes(x = Year, y = Transport..per.capita...CAIT..2020.)) +
  geom_line(color = 'blue', linetype= 'twodash', size = 1.25) +
  geom_point(size = 3, color = "#45ccf5") +
   # labs(title = "Per capita CO₂ emissions from transport",subtitle = "in Poland",
  labs(y = "[tonnes]") +
  geom_smooth(method = 'lm', se = FALSE, fill = '#261269') +
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.06)), 
                     breaks = seq(0.6,1.4,0.2)) +
  scale_x_continuous(breaks = 1990:2016, expand = expansion(mult = c(0.01,0.03))) +
  theme(plot.title = element_text(color='white',size = 17, face = "bold"),
      plot.subtitle = element_text(color='white'),
      axis.title = element_text(color='white', size = 17),
      axis.text = element_text(color='white', size = 14),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_line(color='white'),
      panel.border = element_rect(color='white'),
      plot.background = element_rect(fill = "transparent", color = NA))



p2 <- p1 + 
  scale_x_continuous(breaks = seq(1991,2021,3),
                     expand = expansion(mult = c(0.01,0.023))) +
  scale_y_continuous(breaks = seq(0.6,1.6,0.2), expand = expansion(mult = c(0.01, 0.06))) +
  annotate(geom = 'point', x = 2021, y = res, color = 'lightblue', size = 4) +
  annotate('segment', x = 2016, y = predict(lm(y~x),data.frame(x=2016)),
           xend = 2021, yend = res, colour = "red") +
  annotate('curve', xend = 2021, yend = res-0.012, x = 2019, y = 1.1, 
           curvature = .3, arrow = arrow(length = unit(2, 'mm')), color = 'lightblue') +
  annotate('text', x = 2019.25, y = 1.06, label = paste0(round(res,3), " t"), color = 'lightblue',
           size = 6)
  
ggsave(p2,
       filename = "wykresX",
       bg = "transparent")