library(PogromcyDanych)
library(dplyr)
library(stringr)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 0. Najpiekniejszy dream car
auta2012 %>% 
  filter(Marka == "Nissan", Model == "350 Z", Skrzynia.biegow == "manualna", KM >= 313)

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>%
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  slice(1)


## Odp: 2011


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  slice_head()

## Odp:Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji>=2005, Rok.produkcji<=2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(n = n())
  

## Odp:59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji ==2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(-srednia_cena) %>% 
  slice_head()
  

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011, Marka == "Skoda") %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(srednia_cena) %>% 
  slice_head()

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3", Cena.w.PLN/KM > 600) %>% 
  group_by(Skrzynia.biegow) %>%
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  slice_head()
  

## Odp: Automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa =="benzyna"|Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  group_by(Model) %>% 
  summarise(n = n(),roznica = max(srednia_cena)-min(srednia_cena)) %>% 
  filter(n == 2) %>% 
  arrange(roznica) %>% 
  slice_head
  
## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
pom <- auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe) %>% 
  separate_rows(Wyposazenie.dodatkowe, sep = ", ") %>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  summarise(n = n()) %>% 
  arrange(n) 
pom %>% 
  slice_max(n)
pom %>% 
  slice_min(n)
  

## Odp: Najczęsciej występujące : ABS, alufelgi, wspomaganie kierownicy
## Najrzadziej występujące : blokada skrzyni biegów, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  filter(Marka == "Audi") %>% 
  select(Model,KM) %>% 
  mutate(grupa_modeli = case_when(str_starts(Model, "A") ~ "A",
                                  str_starts(Model, "S") ~ "s",
                                  str_starts(Model, "RS") ~ "RS",
                                  TRUE ~ "Other"
                                  ), .before = Model) %>% 
  filter(grupa_modeli != "Other") %>% 
  group_by(grupa_modeli) %>% 
  summarise(sredniaMoc = mean(KM, na.rm = TRUE), medianaMoc = median(KM, na.rm = TRUE))
  
## Odp: Wynikowa tabela 3x3


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012 %>%
  group_by(Marka) %>%
  filter(n() > 10000) %>% 
  group_by(Marka, Model) %>% 
  mutate(n = n()) %>% 
  group_by(Marka) %>% 
  filter(n == max(n)) %>% 
  group_by(Marka, Model, Kolor) %>% 
  mutate(n = n()) %>% 
  group_by(Marka, Model) %>% 
  filter(n == max(n)) %>% 
  group_by(Marka, Model, Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(-n)
  
  

## Odp: Wynikowa tabela