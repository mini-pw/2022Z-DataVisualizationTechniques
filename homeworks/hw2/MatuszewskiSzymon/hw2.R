## Tworzę ramkę danych na podstawie https://talksport.com/football/695017/premier-league-richest-owners-newcastle-glazers-manchester-united-chelsea-abramovich-kroenke-net-worth/

pl_budget <- data.frame(club = c("Newcastle","Man City","Chelsea","Arsenal","Aston Villa","Wolves","Tottenham","Man Utd","Crystal Palace","Leicester","Southampton","Liverpool","Everton","West Ham","Brighton","Leeds","Watford","Burnley","Norwich","Brentford"),
                        budget = c(320,22.9,9.6,6.35,5.3,4.5,3.6,3.5,3.36,2.9,2.32,2.14,1.9,1.62,0.765,0.344,0.093,0.062,0.023,0.003))

## Chwilowa obróbka  tych danych

pl_budget$club <- factor(pl_budget$club, levels = rev(pl_budget$club))

others <- data.frame(club = "Others", budget =   
                       tail(pl_budget,6) %>% 
                       summarise(budget = sum(budget)))

pl_budget <- pl_budget %>% 
  top_n(14) %>% 
  rbind(others)

## Tworzenie wykresu

pl_budget %>% ggplot(aes(x = club, y = budget, fill = club)) +
  background_image(readPNG("pl_logo2.png")) +
  geom_col(color = "black", size = 1) +
  geom_text(stat='identity', aes(x = club, y = budget, label = budget), hjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = rev(c("darkgrey","#43C5EE","blue","#EF1717","#5F2A2A","orange","white","red","#2344A6","#5B81F2","#F74F1F","#C70000","#250AB0","#8B5549","#3BD9D4"))) +
  theme_bw() +
  labs(
    title = "Richest Premier League Owners 2021/22",
    subtitle = "Transfer Market Opportunities",
    caption = "Data: talksport.com | Creation: Szymon Matuszewski"
  ) +
  ylab("Owner Net Worth in Billion GBP") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(t = 0.4, unit = "cm"), face = "bold"),
    plot.subtitle = element_text(size= 17, hjust=0.5, color = "#4e4d47", margin = margin(t = 0.43, unit = "cm"), face = "bold"),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12,face = "bold"),
    axis.title.y = element_blank()
    
  ) +
  scale_y_continuous(expand = c(0.001,0),breaks = seq(0,300,50), limits = c(0,350)) +
  coord_flip()
