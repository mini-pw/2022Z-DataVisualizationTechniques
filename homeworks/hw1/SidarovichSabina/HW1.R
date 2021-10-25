install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))
View(auta2012[,-ncol(auta2012)])

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>%  
  group_by(Rok.produkcji) %>%
  summarise(Ilosc = n()) %>% 
  top_n(1, Ilosc)

## Odp: 
# 2011 17418 


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>%
  summarise(Ilosc = n()) %>% 
  top_n(1, Ilosc)

## Odp:
## Skoda  (1288)

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)",
         Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>% 
  nrow()

## Odp:
## 59534

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)",
         Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
  top_n(1, Srednia.cena)
## Odp:
## Porsche      (avg price - 345669).

## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda",
         Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
  arrange(Srednia.cena) %>% 
  head(1)


## Odp:
## Fabia

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3", Cena.w.PLN / KM  > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(Ilosc = n()) %>% 
  top_n(1, Ilosc)

## Odp: 
## automatyczna      

## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda",
         Rodzaj.paliwa == "benzyna" 
         | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
  filter(n() == 2) %>% 
  summarise(Diff = abs(Srednia.cena - lag(Srednia.cena))) %>% 
  na.omit() %>% 
  ungroup() %>% 
  filter(Diff == min(Diff))
  
## Odp: 
## Felicia  
## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
auta2012 %>% 
  filter(Marka == "Lamborghini") %>%
  select(Wyposazenie.dodatkowe) %>% 
  separate_rows(1, sep = ", ") %>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  summarise(Ilosc = n()) %>% 
  filter(Ilosc == max(Ilosc) | Ilosc == min(Ilosc))

## Odp: 
# 1 ABS                        18 - max
# 2 alufelgi                   18 - max
# 3 wspomaganie kierownicy     18 - max

# 1 blokada skrzyni biegAlw     1 - min
# 2 klatka                      1 - min

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka == "Audi") %>% 
  select(Model, KM) %>% 
  na.omit() %>% 
  mutate(Grupa = ifelse(grepl("^A", Model), "A",
                        ifelse(grepl("^RS", Model), "RS",
                               ifelse(grepl("^S", Model), "S", "")))) %>% 
  filter(Grupa != "") %>% 
  group_by(Grupa) %>% 
  summarise(Mediana = median(KM), Srednia = mean(KM))

## Odp:
# Grupa Mediana Srednia
# <chr>   <dbl>   <dbl>
# 1 A         140    160.
# 2 RS        450    500.
# 3 S         344    344.


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012 %>% 
  select(Marka, Model, Kolor) %>% 
  group_by(Marka) %>% 
  filter(n() > 10000) %>% 
  group_by(Marka, Model) %>% 
  mutate(Ilosc = n()) %>%
  group_by(Marka, Model,Kolor) %>% 
  mutate(Ilosc.kolor = n()) %>% 
  group_by(Marka) %>% 
  unique() %>% 
  filter(Ilosc == max(Ilosc)) %>% 
  filter(Ilosc.kolor == max(Ilosc.kolor)) 
  
## Odp: 
# 1 Ford          srebrny-metallic  
# 2 Volkswagen    srebrny-metallic  
# 3 Opel          srebrny-metallic  
# 4 BMW           srebrny-metallic  
# 5 Renault       srebrny-metallic  
# 6 Mercedes-Benz srebrny-metallic  
# 7 Audi          czarny-metallic   
