library(PogromcyDanych)
library(dplyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

apply(auta2012, 2, function(x) sum(is.na(x)))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

## Odp: 2011 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

## Odp: Skoda          


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

unique(auta2012[c('Rodzaj.paliwa')])

auta2012 %>% 
  filter(Rok.produkcji >= 2005, 
         Rok.produkcji <= 2011, 
         Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(count = n())

## Odp:59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rok.produkcji == 2011, 
         Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(Cena_srednia = mean(Cena.w.PLN)) %>% 
  arrange(-Cena_srednia) %>% 
  head(1)
  
## Odp: Porsche              


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Rok.produkcji == 2011, 
         Marka == "Skoda") %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(srednia_cena) %>% 
  head(1)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

unique(auta2012[c('Liczba.drzwi')])
unique(auta2012[c('KM')])

# usunę wiersze w których KM == NA
auta2012_bezNA_wKM <- auta2012[!is.na(auta2012$KM),]

apply(auta2012_bezNA_wKM, 2, function(x) sum(is.na(x)))

auta2012_bezNA_wKM %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  mutate(stosunek_PLN_KM = Cena.w.PLN / KM) %>% 
  filter(stosunek_PLN_KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

## Odp: automatyczna 


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

unique(auta2012$Rodzaj.paliwa)

diesel <- auta2012 %>% 
  filter(Marka == 'Skoda', Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) 
  
benzyna <- auta2012 %>% 
  filter(Marka == 'Skoda', Rodzaj.paliwa == 'benzyna') %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) 

diesel %>% 
  merge(benzyna, by="Model", all.x = TRUE) %>% 
  mutate(roznica = abs(srednia_cena.x - srednia_cena.y)) %>% 
  arrange(roznica) %>% 
  head(1)

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

tmp <- auta2012 %>% 
  filter(Marka == 'Lamborghini') %>% 
  select(Wyposazenie.dodatkowe) 

tmp <- unlist(tmp)
tmp <- str_split(tmp, ', ')
tmp <- unlist(tmp)
sort(table(tmp))


## Odp: najrzadsze: blokada skrzyni biegow i klatka
##      najczestsze: ABS, alufelgi, wspomaganie kierownicy


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi


auta2012 %>% 
  filter(Marka == 'Audi') %>% 
  mutate(Model_poprawiony = case_when(str_starts(Model, 'RS')  ~ 'RS',
                                      str_starts(Model, 'A') ~ "A",
                                      str_starts(Model, 'S') ~ 'S',
                                      T ~ 'inny')) %>% 
  group_by(Model_poprawiony) %>% 
  summarise(srednia_moc = mean(KM, na.rm = TRUE), mediana_mocy = median(KM, na.rm = TRUE))

## Odp: A - srednia moc: 160 mediana mocy: 140
#       S - srednia moc: 344 mediana mocy: 344
#       RS - srednia moc: 500 mediana mocy: 450

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% 
  select(Marka, Model, Kolor) %>% 
  group_by(Marka) %>%  
  mutate(ilosc_aut_marki = n()) %>% 
  filter(ilosc_aut_marki > 10000) %>% 
  group_by(Marka, Model) %>% 
  mutate(ilosc_modeli = n()) %>% 
  group_by(Marka, Model, Kolor) %>% 
  mutate(ilosc_kolorow = n()) %>%
  unique() %>% 
  arrange(Marka, -ilosc_modeli) %>% 
  group_by(Marka) %>% 
  filter(ilosc_modeli == max(ilosc_modeli)) %>% 
  filter(ilosc_kolorow == max(ilosc_kolorow))


## Odp:

#  Marka         Model  Kolor            ilosc_aut_marki ilosc_modeli ilosc_kolorow
#  Audi          A4     czarny-metallic            12851         4280           853
#  BMW           320    srebrny-metallic           10126         1791           441
#  Ford          Focus  srebrny-metallic           17717         5691          1711
#  Mercedes-Benz C 220  srebrny-metallic           10945          667           205
#  Opel          Astra  srebrny-metallic           19092         6348          1539
#  Renault       Megane srebrny-metallic           17498         3759           733
#  Volkswagen    Passat srebrny-metallic           22826         6883          1466
