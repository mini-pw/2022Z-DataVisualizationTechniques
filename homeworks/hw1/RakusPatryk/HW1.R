library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)


colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>%
  group_by(Rok.produkcji) %>% 
  summarise(ilosc = n()) %>% 
  slice_max(ilosc)

## Odp: 2011 - 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji==2011) %>% 
  group_by(Marka) %>% 
  summarise(ilosc = n()) %>% 
  slice_max(ilosc)
  

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>% 
  select(Rodzaj.paliwa) %>% table

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rok.produkcji == 2011,
         Rodzaj.paliwa== "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(avg_cena_PLN = mean(Cena.w.PLN)) %>% 
  slice_max(avg_cena_PLN)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(avg_cena_PLN = mean(Cena.w.PLN)) %>% 
  slice_min(avg_cena_PLN)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  mutate(ratio = Cena.w.PLN / KM) %>% 
  filter(ratio > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(ilosc = n()) %>% 
  slice_max(ilosc)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta2012 %>% 
  filter(Marka == "Skoda",
         Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(avg = mean(Cena.w.PLN)) %>% 
  pivot_wider(names_from=Rodzaj.paliwa, values_from=avg) %>%
  remove_missing %>% 
  mutate(difference = abs(benzyna - `olej napedowy (diesel)`)) %>% 
  ungroup() %>% 
  slice_min(difference)

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  separate_rows(Wyposazenie.dodatkowe, sep = ", ") %>%
  group_by(Wyposazenie.dodatkowe) %>% 
  summarise(count = n()) %>%
  arrange(-count) -> x
  head(x, 1)
  tail(x, 1)
  

## Odp: najczesciej ABS, najrzadziej klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
  
auta2012 %>%
  filter(Marka == "Audi") %>%
  mutate(Group = case_when(str_starts(Model, "A") ~ "A",
                           str_starts(Model, "S") ~ "S",
                           str_starts(Model, "RS") ~ "RS"), .before=Model) %>%
  filter(!is.na(Group) & !is.na(KM)) %>%
  group_by(Group) %>% 
  summarise(mean_KM = mean(KM), median_KM = median(KM))

## Odp: porownanie jest w powyzszej tabelce 3x3


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>%
  group_by(Marka) %>%
  filter(n() > 10000) %>% 
  group_by(Model, Marka) %>% 
  mutate(n = n()) %>%
  group_by(Marka) %>% 
  filter(n == max(n)) %>% 
  select(Marka, Model, Kolor) %>% 
  group_by(Model, Marka, Kolor) %>% 
  mutate(n = n()) %>% 
  group_by(Model, Marka) %>% 
  filter(n == max(n)) %>% 
  unique

 ## Odp: dane s� w powyzszej tabeli
