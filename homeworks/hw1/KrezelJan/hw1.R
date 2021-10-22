library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(count = n()) %>% 
  top_n(1)

## Odp: 2011


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  top_n(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(between(Rok.produkcji, 2005, 2011),  Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  nrow()

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>% 
  summarise(avg.price = mean(Cena.w.PLN)) %>% 
  top_n(1)


## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(avg.price = mean(Cena.w.PLN)) %>% 
  top_n(-1)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?


auta2012 %>% 
  filter(Liczba.drzwi == "2/3", Cena.w.PLN / KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n()) %>% 
  top_n(1)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

library(tidyr)

auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(avg.price = mean(Cena.w.PLN)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = avg.price) %>% 
  rename("diesel"="olej napedowy (diesel)") %>% 
  mutate(diff = abs(diesel-benzyna)) %>%
  na.omit() %>% 
  ungroup() %>% 
  top_n(-1)


## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

library(stringr)

auta2012 %>% filter(Marka == "Lamborghini") -> lambo
lambo$Wyposazenie.dodatkowe %>% 
  str_split(', ') %>% 
  unlist() %>% 
  data.frame() %>% 
  rename('wyposazenie'='.') %>% 
  filter(wyposazenie != "") %>% 
  group_by(wyposazenie) %>% 
  summarise(count = n()) -> freq_table

freq_table %>% top_n(1)
freq_table %>% top_n(-1)

## Odp: 
## Najczesciej: ABS, alufelgi, wspomaganie kierownicy
## Najrzadziej: blokada skrzyni biegów, klatka

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi


auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate("Model.group" = case_when( 
    str_starts(Model, "A") ~ "A",
    str_starts(Model, "S") ~ "S",
    str_starts(Model, "RS") ~ "RS",
    TRUE ~ "other")) %>% 
  filter(Model.group != "other") %>% 
  group_by(Model.group) %>% 
  summarise(power.avg = mean(KM, na.rm = TRUE),
            power.med = median(KM, na.rm = TRUE)) %>% 
  mutate(is.median.greater.than.average = power.med > power.avg)
  

## Odp:
## 1 A                160.       140 FALSE                         
## 2 RS               500.       450 FALSE                         
## 3 S                344.       344 TRUE 

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.


auta2012 %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  filter(count > 10000) %>% 
  select(Marka) -> top_cars

auta2012 %>% 
  filter(Marka %in% top_cars$Marka) %>% 
  group_by(Marka, Model) %>% 
  summarise(count = n()) %>% top_n(1) -> top_models

auta2012 %>% 
  filter(Model %in% top_models$Model) %>% 
  group_by(Model, Kolor) %>% 
  summarise(count = n()) %>% top_n(1) -> top_colors

top_colors


## Odp: 
## 1 320    srebrny-metallic   441
##2 A4     czarny-metallic    853
## 3 Astra  srebrny-metallic  1539
## 4 C 220  srebrny-metallic   205
## 5 Focus  srebrny-metallic  1711
## 6 Megane srebrny-metallic   733
## 7 Passat srebrny-metallic  1466
