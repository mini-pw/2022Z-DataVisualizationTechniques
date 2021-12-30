library(dplyr)
library(ggplot2)
library(readxl)


# dane pochodzą z dokumentu źródłowego (podanego w raporcie)
# zostały one pobrane z tabeli bezpośrednio nad zmienianym wykresem
# i zmodyfikowane tak, aby były jak najłatwiejsze do wizualizacji

df <- read_excel("danedane.xlsx")


ggplot(df, aes(x = reorder(Rodzaj, -Import), y=Import)) +
  geom_col(fill = "#3182bd", width = 0.75) +
  geom_text(aes(label = Import), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,41)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) + 
  labs(title = "Struktura importu według sekcji nomenklatury SITC",
       subtitle = "Dla roku 2020\n",
       x = "Rodzaj produktu",
       y = "Wartość procentowa") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 13),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, b = 13),
                                    size = 15),
        title = element_text(size = 18),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.margin = unit(c(0.5,1,0,0), "cm"))


ggplot(df, aes(x = reorder(Rodzaj, -Eksport), y=Eksport)) +
  geom_col(fill = "#3182bd", width = 0.75) +
  geom_text(aes(label = Eksport), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,41)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) + 
  labs(title = "Struktura eksportu według sekcji nomenklatury SITC",
       subtitle = "Dla roku 2020\n",
       x = "Rodzaj produktu",
       y = "Wartość procentowa") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 13),
                                    size = 15),
        axis.title.x = element_text(margin = margin(t = 20, b = 13),
                                    size = 15),
        title = element_text(size = 18),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.margin = unit(c(0.5,1,0,0), "cm"))
