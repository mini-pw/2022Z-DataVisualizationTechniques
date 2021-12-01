library(dplyr)
library(ggplot2)
library(patchwork)

dobowe_synop <- read.csv("ramki/dobowe_synop.csv")
# dane ze strony: https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/synop/

pory_roku <- function(i){
  
  zima <- dobowe_synop %>% 
    filter(Rok == i) %>% 
    filter(Miesiac == 1 | Miesiac == 2 | (Miesiac == 3 & Dzien < 21) | (Miesiac == 12 & Dzien > 21)) %>% 
    mutate(Pora.roku = "zima")
  
  wiosna <- dobowe_synop %>% 
    filter(Rok == i) %>% 
    filter(Miesiac == 4 | Miesiac == 5 | (Miesiac == 3 & Dzien >= 21) | (Miesiac == 6 & Dzien < 23)) %>% 
    mutate(Pora.roku = "wiosna")
  
  lato <- dobowe_synop %>% 
    filter(Rok == i) %>% 
    filter(Miesiac == 7 | Miesiac == 8 | (Miesiac == 6 & Dzien >= 23) | (Miesiac == 9 & Dzien <= 23)) %>% 
    mutate(Pora.roku = "lato")
  
  jesien <- dobowe_synop %>% 
    filter(Rok == i) %>% 
    filter(Miesiac == 10 | Miesiac == 11 | (Miesiac == 9 & Dzien > 23) | (Miesiac == 12 & Dzien < 23)) %>% 
    mutate(Pora.roku = "jesień")
  
  wynik <- rbind(wiosna, lato, jesien, zima) %>% 
    filter(is.na(Status.uslonecznienie) | Status.uslonecznienie == 9)
  
  wynik <- wynik %>% 
    filter(Nazwa.Stacji == "ŁÓDŹ" | Nazwa.Stacji == "ŁÓDŹ-LUBLINEK") 
}

wykres_pory <- function(i){
  wykres_g <- pory_roku(i) %>% 
    ggplot(aes(x = Uslonecznienie, y = Srednia.temp , color = Pora.roku)) +
    geom_point() +
    theme_minimal() +
    scale_color_manual(values = c("orange", "gold", "green", "deepskyblue"))  +
    ylab("Średnia temperatura dobowa [°C]") +
    scale_y_continuous(limits = c(-15, 25)) + 
    labs(title = paste("Rok", i)) +
    xlab("Usłonecznienie [godziny]") +
    theme(legend.position = "none")
  
  wykres_y <-  pory_roku(i) %>% 
    ggplot(aes(x = Srednia.temp, color = Pora.roku, fill = Pora.roku)) +
    geom_density(alpha = 0.35) +
    theme_minimal() +
    scale_color_manual(values = c("orange", "gold", "green", "deepskyblue")) +
    scale_fill_manual(values = c("orange", "gold", "green", "deepskyblue"))  +
    coord_flip() + 
    theme(axis.title.y = element_blank(), 
          legend.position = "none") +
    ylab("Częstość") +
    scale_x_continuous(limits = c(-15, 25)) +
    scale_y_reverse(lim = c(0.15, 0))
  
  wykres_x <-  pory_roku(i) %>% 
    ggplot(aes(x = Uslonecznienie, color = Pora.roku, fill = Pora.roku)) +
    geom_density(alpha = 0.35) +
    theme_minimal() +
    scale_color_manual(values = c("orange", "gold", "green", "deepskyblue")) +
    scale_fill_manual(values = c("orange", "gold", "green", "deepskyblue"))  +
    theme(axis.title.x = element_blank(),
          legend.position = "none") +
    ylab("Częstość")  +
    scale_y_reverse(lim = c(0.25, 0))
  
  wykres <- wykres_y + wykres_g + plot_spacer() + wykres_x +
    plot_layout(ncol = 2, heights = c(0.8, 0.2), widths = c(0.2, 0.8))  
}

wykres <- wykres_pory(1991) | wykres_pory(2020) 

wykres
