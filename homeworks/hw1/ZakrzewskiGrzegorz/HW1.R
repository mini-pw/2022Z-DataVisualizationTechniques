library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>%
  group_by(Rok.produkcji) %>% 
  summarise(n = n()) %>% 
  slice_max(n, n = 1)

## Odp: Najwięcej aut jest z 2011 roku. Jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  slice_max(n, n = 1)

## Odp: Tą marką jest Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & between(Rok.produkcji, 2005, 2011)) %>% 
  count()

## Odp: Jest ich 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(srednia.cena = mean(Cena.w.PLN)) %>%
  slice_max(srednia.cena, n = 1)

## Odp: Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(srednia.cena = mean(Cena.w.PLN)) %>%
  slice_min(srednia.cena, n = 1)

## Odp: Skoda Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  filter(!is.na(KM)) %>%  # bo jest 27069 wartości NA w kolumnie KM
  mutate(cena.do.KM = Cena.w.PLN / KM) %>% 
  filter(cena.do.KM > 600 & Liczba.drzwi == "2/3") %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  slice_max(n, n = 1)

## Odp: automatyczna skrzynia biegów


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

# uznaję, że rodzaje paliwa: benzyna, benzyna+LPG oznaczają silnik benzynowy
# hybrydy, choć teoretycznie ma też silnik benzynowy, nie uznaję

library(tidyr) # do użycia pivot_wider

auta2012 %>% 
  filter(Marka == "Skoda") %>%
  # kategoryzujemy silniki na benzynowe, diesel i inne
  mutate(silnik = case_when(  
    Rodzaj.paliwa %in% c("benzyna", "benzyna+LPG") ~ "benzynowy",
    Rodzaj.paliwa == "olej napedowy (diesel)" ~ "diesel",
    TRUE ~ "other"
  )) %>% 
  
  filter(silnik != "other") %>% 
  group_by(Model, silnik) %>% 
  summarise(srednia.cena = mean(Cena.w.PLN)) %>%
  pivot_wider(names_from = silnik, values_from = srednia.cena) %>% 
  mutate(roznica.srednich.cen = abs(benzynowy - diesel)) %>%
  arrange(roznica.srednich.cen) %>%
  head(n = 1)
  
## Odp: Ten model to Scoda Favorit


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

library(stringr)

# Wybieram (jako wektor) kolumnę wyposażeń samochodów Lamborghini
col.wyposazenie <- auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  pull(Wyposazenie.dodatkowe)

# Z tej kolumny robię bardzo długi wektor zawierający każde wyposażenie, 
# tyle wystąpień ile ono było użyte
wyposazenie <- c(str_split(col.wyposazenie, ", ", simplify = TRUE))

# Robię z powrotem tibble, żeby pogrupować i policzyć liczbę wystąpień
# każdego rodzaju wyposażenia
tibble(wyposazenie) %>% 
  filter(wyposazenie != "") %>% 
  group_by(wyposazenie) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  View()

## Odp: Najrzadziej: blokada skrzyni biegów, klatka
## Najczęściej: ABS, alufelgi, wspomaganie kierownicy 


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  filter(Marka == "Audi" ) %>%
  mutate(grupa.modeli = case_when(  
    str_starts(Model, "A") ~ "A",
    str_starts(Model, "S") ~ "S",
    str_starts(Model, "RS") ~ "RS",
    TRUE ~ "other"
  )) %>% 
  filter(grupa.modeli != "other" & !is.na(KM)) %>% 
  group_by(grupa.modeli) %>%
  summarise(
    srednia.KM = mean(KM),
    mediana.KM = median(KM)
  )

## Odp:
# grupa.modeli   srednia.KM mediana.KM
# 1 A                  160.        140
# 2 RS                 500.        450
# 3 S                  344.        344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% 
  # filtrujemy marki
  group_by(Marka) %>% 
  add_count() %>% 
  filter(n > 10000) %>% 
  ungroup() %>% 
  
  # wybieramy najpopularniejsze modele
  group_by(Marka, Model) %>% 
  add_count() %>% 
  ungroup() %>% 
  group_by(Marka) %>% 
  slice_max(nn, n = 1) %>% 
  ungroup() %>% 
  
  # wybieramy najpopularniejsze kolory
  group_by(Marka, Kolor) %>% 
  add_count() %>% 
  ungroup() %>% 
  group_by(Marka) %>% 
  slice_max(nnn, n = 1, with_ties = FALSE) %>% 
  select(Marka, Model, Kolor)


## Odp: 
# Marka         Model  Kolor       

# Audi          A4     czarny-metallic 
# BMW           320    srebrny-metallic
# Ford          Focus  srebrny-metallic
# Mercedes-Benz C 220  srebrny-metallic
# Opel          Astra  srebrny-metallic
# Renault       Megane srebrny-metallic
# Volkswagen    Passat srebrny-metallic