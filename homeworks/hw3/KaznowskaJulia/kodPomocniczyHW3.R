library(dplyr)
library(ggplot2)
library(RColorBrewer)

# 1 - wykres slupkowy + linia + etykiety + rozne kolory + wyrazne kreski z tylu
# 2 - porownanie - slupkowy o normalnym kolorze + ew ewykiety
# ktory bardziej czytelny?
# dlaczego?

# Dochody pracownika firmy Cats&Dogs w roku 2035
miesiace <- c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec",
              "lipiec", "sierpień", "wrzesień", "październik", 
              "listopad", "grudzień")
dochod <- c(3015, 2098, 3111, 2981, 2810, 3291, 2013, 3712, 2048,
            3992, 2791, 2091)
lp <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
dane12 <- data.frame(lp, miesiace, dochod)

# wykres 1
dane12 %>%
  ggplot(aes(x = reorder(miesiace, lp), y = dochod, fill = miesiace)) +
  geom_col() +
  geom_line(group = 1, color = "red", size = 2) +
  geom_point(size = 4) + 
  geom_text(aes(label=dochod), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.position = "none") + 
  labs(x = "\nMiesiąc",
       y = "Dochód (zł)\n",
       title = "Dochody pracownika firmy Cats&Dogs",
       subtitle = "Rok 2035\n") +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 18),
        plot.subtitle = element_text(face = "italic", size = 16),
        plot.margin = unit(c(0.5,1,0.75,0.5), "cm"))

# wykres 2
dane12 %>%
  ggplot(aes(x = reorder(miesiace, lp), y = dochod)) +
  geom_col(fill = "#2b8cbe") +
  geom_text(aes(label=dochod), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.position = "none") + 
  labs(x = "\nMiesiąc",
       y = "Dochód (zł)\n",
       title = "Dochody pracownika firmy Cats&Dogs",
       subtitle = "Rok 2035\n") +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 18),
        plot.subtitle = element_text(face = "italic", size = 16),
        plot.margin = unit(c(0.5,1,0.75,0.5), "cm"))

# 3 - sprawdzenie wartosci w wykresie kolowym (uporzadkuj)
# 4 - uporzadkuj na slupkowym (nie po kolei na wykresie)
# ktory prostszy?

gatunek3 <- c("pies", "królik", "kot", "chomik", "rybka")
liczba3 <- c(38, 15, 40, 14, 13)
dane3 <- data.frame(gatunek3, liczba3)

pie(x = dane3$liczba3, labels = dane3$gatunek3, 
    col = c("#b3e2cd", "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9"),
    main = "Najczęściej posiadane zwierzęta w gospodarstwach domowych\nKsięstwo Tusant, rok 3193")

gatunek4 <- c("pies", "królik", "kot", "chomik", "rybka")
liczba4 <- c(15, 40, 13, 38, 14)
dane4 <- data.frame(gatunek4, liczba4)

dane4 %>% 
  ggplot(aes(x = gatunek4, y = liczba4)) +
  geom_col(fill = "#43a2ca") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "\nGatunek zwierzęcia",
       y = "Liczba gospodarstw, \nw których zwierze występuje\n",
       title = "Najczęściej posiadane zwierzęta w gospodarstwach domowych",
       subtitle = "Księstwo Tusant, rok 3193") +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 18),
        plot.subtitle = element_text(face = "italic", size = 16),
        plot.margin = unit(c(0.5,1.75,0.75,0.5), "cm"))

  