library(PogromcyDanych)
library(dplyr)
library(stringr)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n=n()) %>% 
  top_n(1)


## Odp: Z 2011 roku, jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1)

## Odp: Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  filter(Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>% 
  summarise(n=n())

## Odp: 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(SredniaCena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  top_n(1)

## Odp: Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  filter(Marka == "Skoda") %>% 
  group_by(Model) %>% 
  summarise(SredniaCena.wPLN = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  top_n(-1)

## Odp:Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == '2/3') %>% 
  mutate(stosunek.ceny.do.KM = Cena.w.PLN/KM) %>% 
  filter(stosunek.ceny.do.KM > 600) %>%
  group_by(Skrzynia.biegow) %>% 
  summarise(n=n()) %>% 
  top_n(1)

## Odp: automatyczna.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda") %>% 
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(srednia.cena = mean(Cena.w.PLN, na.rm = T)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = srednia.cena) %>% 
  mutate(roznica = abs(benzyna - `olej napedowy (diesel)`)) %>% 
  arrange(roznica) %>% 
  head(1)



## Odp: Felicia.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
auta2012 %>%
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe) %>% 
  .$Wyposazenie.dodatkowe %>% 
  str_split(pattern = ", ") %>%
  unlist() %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(Freq) %>% 
  filter(Freq == min(Freq) | Freq == max(Freq) )
              

## Odp: Najrzadziej blokada skrzyni biegów, klatka.
## Najczęściej: ABS, alufelgi, wspomaganie kierownicy   


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>%
  filter(Marka == "Audi") %>% 
  filter(str_starts(Model, "A") | str_starts(Model, "S") | str_starts(Model, "RS")) %>% 
  mutate(Grupa = case_when(str_starts(Model, "A") ~ "A",
                           str_starts(Model, "S") ~ "S",
                           str_starts(Model, "RS") ~ "RS",
                                TRUE ~ "Other")) %>%
  group_by(Grupa) %>% 
  summarise(Srednia.KM = mean(KM, na.rm = T), Mediana.KM = median(KM, na.rm = T))

  

## Odp: Grupa Srednia.KM Mediana.KM
##      A           160.        140
##      RS          500.        450
##      S           344.        344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
marki <- auta2012 %>% 
  group_by(Marka) %>%
  summarise(n=n()) %>% 
  filter(n > 10000) %>% 
  .$Marka

popularne_modele <- auta2012 %>% 
  filter(Marka %in% marki) %>%
  group_by(Marka, Model) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) 

popularne_modele %>% 
  left_join(auta2012, by = c("Marka", "Model")) %>% 
  group_by(Marka, Model, Kolor) %>% 
  summarise(n = n()) %>%
  top_n(1, n) %>% 
  select(-n)

## Odp: Marka(>10000)   NajpopularniejszyModel      NajpopularniejszyKolor          
##      Audi                    A4                  czarny-metallic
##      BMW                     320                 srebrny-metallic
##      Ford                    Focus               srebrny-metallic
##      Mercedes-Benz           C 220               srebrny-metallic
##       Opel                   Astra               srebrny-metallic
##      Renault                 Megane              srebrny-metallic
##      Volkswagen              Passat              srebrny-metallic
