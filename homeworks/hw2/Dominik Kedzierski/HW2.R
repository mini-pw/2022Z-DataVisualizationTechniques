library(ggplot2)


df <- data.frame(Partia = c("PIS", "Koalicja Obywatelska","Polska 2050",
                            "Lewica", "Konfederacja", "PSL", "NIE WIEM" ),
                 Poparcie = c(36,25,14,9,5,5,6))

ggplot(df, aes(x = reorder(Partia, -Poparcie), y = Poparcie)) +
  geom_bar(stat = "identity", fill = c('#2438a6', '#2b79ff','#fff82b',
                                       '#7a206b','#080a40','#00c210',
                                       '#6e6e6e'))+
  labs(y = "Procent udzielonych odpowiedzi",
       x = "Na kogo byœ g³osowa³?",
       title = "Na kogo zag³osowaliby Polacy?",
       caption = "ród³o: IBRiS dla \"RZ\",badanie CATI przeprowadzone 22-23
       paŸdzernika 2021 r. na 1100 osobowej grupie respondentów
       ")+
  ylim(0,40)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold",
                                  color = '#EEEEEE', size = 15),
        plot.caption = element_text(face = "bold",color = '#EEEEEE', size = 7
                                    ),
        axis.title.y = element_text(face = "bold",color = '#EEEEEE', size = 12),
        axis.title.x = element_text(face = "bold",color = '#EEEEEE', size = 12),
        axis.text.x = element_text(face = "bold", color = '#EEEEEE', size = 10), 
        axis.text.y = element_text(face = "bold", color = '#EEEEEE', size = 10), 
        panel.background = element_rect(fill = '#333333'),
        plot.background = element_rect(fill = '#333333'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        
        )+
  geom_text(aes(label=Poparcie), position=position_dodge(width=0.9), vjust=-0.25,
            color = '#FFFFFF', size =7)




  #d0d1e6

