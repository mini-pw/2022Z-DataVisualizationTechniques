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

## Odp: 2011 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  top_n(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>% 
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  summarise(count = n())


## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Marka) %>% 
  summarise(mean_price = mean(Cena.w.PLN)) %>% 
  top_n(1, mean_price)
## Odp: Porshe


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011, Marka == 'Skoda') %>% 
  group_by(Model) %>% 
  summarise(mean_price = mean(Cena.w.PLN)) %>% 
  top_n(1, -mean_price)


## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == '2/3') %>% 
  mutate(price_hp_ratio = Cena.w.PLN/KM) %>% 
  filter(price_hp_ratio > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n())

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
diesels_mean_prices <-
  auta2012 %>%
    filter(Marka == 'Skoda', Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
    group_by(Model) %>% 
    summarise(mean_diesel_price = mean(Cena.w.PLN))

gasoline_mean_prices <-
  auta2012 %>%
    filter(Marka == 'Skoda', (Rodzaj.paliwa == 'benzyna' | Rodzaj.paliwa == 'benzyna+LPG')) %>% 
    group_by(Model) %>% 
    summarise(mean_gasoline_price = mean(Cena.w.PLN))

diesels_mean_prices %>% 
  left_join(gasoline_mean_prices, by = 'Model') %>% 
  mutate(diff = abs(mean_diesel_price - mean_gasoline_price)) %>% 
  top_n(1, -diff)
  
## Odp: Favorit


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
library(stringr)
lamborghini <- auta2012 %>% 
                filter(Marka == 'Lamborghini') %>% 
                select(Wyposazenie.dodatkowe)

splitted <- unlist(str_split(unlist(lamborghini), ', '))
sort(table(splitted))
## Odp: najrzadziej: blokada skrzyni biegow, klatka; 
##      najczesciej:  wspomaganie kierownicy, alufelgi, ABS


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka == 'Audi', str_detect(Model, '(^A)|(^S)|(^RS)')) %>% 
  mutate(Seria = case_when(str_detect(Model, '^A') ~ 'A',
                           str_detect(Model, '^S') ~ 'S',
                           str_detect(Model, '^RS') ~ 'RS')) %>% 
  group_by(Seria) %>% 
  summarise(mean_hp = mean(KM, na.rm = TRUE), median_hp = median(KM, na.rm = TRUE))

## Odp:
## Seria mean_hp    median_hp
## A     160.       140
## RS    500.       450
## S     344.       344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

frequent <- auta2012 %>% 
              group_by(Marka) %>% 
              summarise(count = n()) %>% 
              filter(count > 10000) %>% 
              select(Marka)

frequent_models <- frequent %>% 
                    left_join(auta2012) %>% 
                    group_by(Marka, Model) %>% 
                    summarise(count = n()) %>%
                    arrange(-count) %>% 
                    filter(row_number() == 1)

frequent_models %>% 
  left_join(auta2012, by = c('Marka', 'Model')) %>% 
  group_by(Marka, Model, Kolor) %>% 
  summarise(color_count = n()) %>% 
  arrange(-color_count) %>% 
  filter(row_number() == 1) %>% 
  select(Marka, Model, Kolor)
  

## Odp: 
# Marka         Model        Kolor           
# Ford          Focus  srebrny-metallic
# Opel          Astra  srebrny-metallic
# Volkswagen    Passat srebrny-metallic
# Audi          A4     czarny-metallic 
# Renault       Megane srebrny-metallic
# BMW           320    srebrny-metallic
# Mercedes-Benz C 220  srebrny-metallic