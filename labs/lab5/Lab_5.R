###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 5            ###
###########################################

library(ggplot2)
library(dplyr)
library(SmarterPoland)

## Zadanie 1
# Z zamieszczonego pliku pdf w folderze lab5 należy wybrać jedno z dwóch zadań. Dane potrzbne do odtowrzenia wizualizacji 
# należy wczytać następująco:

df <- read.csv(file = "https://raw.githubusercontent.com/R-Ladies-Warsaw/PoweR/master/Cz%C4%99%C5%9B%C4%87%202%20-%20Formatowanie%20danych/R/data/ranking.csv", 
               encoding = "UTF-8")


## ggrepel

### Etykiety tekstowe

library(ggrepel)

#Więcej: https://ggrepel.slowkow.com/articles/examples.html



## Zadanie 2
# Narysuj wykres punktowy zależności między wskaźnikiem urodzeń a wskaźnikiem śmiertleności 
# oraz podpisz punkty nazwą kraju o najniższym i najwyższym wskaźniku śmiertelności.


## patchwork

library(patchwork)



## Zadanie 3 - wykres gęstości brzegowych
# a) wykres punktowy dwóch zmiennych + kolor
# b) rozkład jednej zmiennej x 2


## ggstatsplot

library(ggstatsplot)

# Więcej: https://indrajeetpatil.github.io/ggstatsplot/index.html

## Mapy

require(maps)
?map_data

# maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()

?coord_map()

# Więcej: https://ggplot2.tidyverse.org/reference/coord_map.html

## Zadanie 4
# Wykorzystując dane countries narysuj mapę świata i zaznacz na niej wskaźnik urodzeń. 