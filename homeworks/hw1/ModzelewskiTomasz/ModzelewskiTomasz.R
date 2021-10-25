library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  top_n(1)

## Odp: Rocznik 2011, liczba aut 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  count(Marka) %>% 
  arrange(-n) %>% 
  top_n(1)

## Odp: Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)', Rok.produkcji <= 2011, Rok.produkcji >= 2005) %>% 
  count()

## Odp: 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% filter(Rodzaj.paliwa == 'olej napedowy (diesel)', Rok.produkcji == 2011) %>% group_by(Marka) %>% summarize(mean = mean(Cena.w.PLN)) %>% arrange(-mean) %>% top_n(1)

## Odp: Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% filter(Rok.produkcji == 2011, Marka == 'Skoda') %>% group_by(Model) %>% summarize(mean=mean(Cena.w.PLN)) %>% filter(Model != 'inny') %>% top_n(-1)

## Odp: Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  filter(Liczba.drzwi == '2/3', Cena.w.PLN / KM > 600) %>%
  count(Skrzynia.biegow) %>%
  top_n(1)

## Odp: automatyczna.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel
library(tidyr)
roznice <- auta2012 %>%
  filter(Marka == 'Skoda') %>%
  select(Model, Cena, Rodzaj.paliwa) %>%
  mutate(silnik = case_when(
    grepl('diesel', Rodzaj.paliwa) ~ 'diesel',
    grepl('benzyna', Rodzaj.paliwa) ~ 'benzyna',
    TRUE ~ 'Other'
  )) %>%
  filter(silnik != 'Other') %>%
  group_by(Model, silnik) %>% 
  summarize(srednia_cena = mean(Cena)) %>% 
  pivot_wider(names_from = silnik, values_from = srednia_cena) %>%
  na.omit() %>%
  mutate(roznica = benzyna-diesel) %>%
  filter(Model != 'inny')

roznice %>% filter(roznica == min(roznice$roznica)) %>% select(Model)

## Odp: Yeti


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
library(stringr)

wyp_Lam <- table(unlist(str_split(as.vector((auta2012 %>% filter(Marka=='Lamborghini'))$Wyposazenie.dodatkowe), ', ')))
wyp_Lam[wyp_Lam == max(wyp_Lam)] # maksymalne
wyp_Lam[wyp_Lam == min(wyp_Lam)] # minimalne

## Odp: Najczęściej: ABS, alufelgi, wspomaganie kierownicy. Najrzadziej: blokada skrzyni biegów, klatka.


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka == 'Audi') %>%
  mutate(Model_litera = case_when(
    substring(Model, 1, 1) == 'A' ~ 'A',
    substring(Model, 1, 1) == 'S' ~ 'S',
    substring(Model, 1, 2) =='RS' ~'RS',
    TRUE ~ 'Other'
  )) %>%
  filter(Model_litera != 'Other') %>%
  group_by(Model_litera) %>%
  summarize(srednia = mean(KM, na.rm=TRUE), mediana = median(KM, na.rm=TRUE))

## Odp: W przypadku modeli A tudzież RS średnia jest większa od mediany, a dla modelu S te dwie wielkości są równe.


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

marki <- table(auta2012$Marka)
marki_powyzej_10000 <- marki[marki > 10000]
names(marki_powyzej_10000)

najczestsze_kolory_dla_modeli <- auta2012[, c("Marka", "Model", "Kolor")] %>% 
  filter(Marka %in% names(marki_powyzej_10000), Model != "", Kolor != "") %>% 
  group_by(Marka, Model, Kolor) %>% summarize(liczba_wystapien_koloru = n()) %>% 
  filter(liczba_wystapien_koloru == max(liczba_wystapien_koloru))

najczestsze_modele_dla_marek <- auta2012[, c("Marka", "Model")] %>% 
  filter(Marka %in% names(marki_powyzej_10000), Model != "") %>% 
  group_by(Marka, Model) %>% summarize(liczba_wystapien_modelu = n()) %>% 
  filter(liczba_wystapien_modelu == max(liczba_wystapien_modelu))

left_join(najczestsze_modele_dla_marek, najczestsze_kolory_dla_modeli, by=c("Marka", "Model")) %>% select(Marka, Kolor)
  
## Odp: Ponad 10000 razy występują: Audi, BMW, Ford, Mercedes-Benz, Opel, Renault, Volkswagen.
## Kolory: dla Audi czarny-metallic, dla pozostałych: srebrny-metallic
