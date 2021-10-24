library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[, -ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>%
  group_by(Rok.produkcji) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) %>%
  head(1)

## Odp: Rok 2011 - jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>%
  filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(-liczba) %>%
  head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>%
  filter(2005 <= Rok.produkcji & Rok.produkcji <= 2011,
         Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  count()



## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>%
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(sr_cena = mean(Cena.w.PLN)) %>%
  arrange(-sr_cena)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>%
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>%
  group_by(Model) %>%
  summarise(sr_cena = mean(Cena.w.PLN)) %>%
  arrange(sr_cena)

## Odp: Skoda Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>%
  mutate(cena_do_km = Cena.w.PLN / KM) %>%
  filter(Liczba.drzwi == "2/3", cena_do_km > 600) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(liczba = n()) %>%
  arrange(-liczba) %>%
  head(1)


## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen
##    między samochodami z silnikiem benzynowym, a diesel?

auta2012 %>%
  filter(Marka == "Skoda",
         Rodzaj.paliwa == c("benzyna", "olej napedowy (diesel)")) %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(sr_cena = mean(Cena.w.PLN)) %>%
  group_by(Model) %>%
  filter(n() == 2) %>%
  summarise(sr_roznica = max(sr_cena) - min(sr_cena)) %>%
  arrange(sr_roznica) %>%
  head(1)

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe
##    samochodów marki Lamborghini

#najrzadziej
czestosci_fraz <- auta2012 %>%
  filter(Marka == "Lamborghini") %>%
  select(Wyposazenie.dodatkowe) %>%
  transmute(Wyposazenie.dod = str_split(Wyposazenie.dodatkowe, ', ')) %>% 
  unlist() %>% 
  table()
  
names(czestosci_fraz[czestosci_fraz == min(czestosci_fraz)])

#najczesciej
names(czestosci_fraz[czestosci_fraz == max(czestosci_fraz)])


## Odp: najrzadziej: 'blokada skrzyni biegow' oraz 'klatka'
##      najczesciej: 'ABS', 'alufelgi' oraz 'wspomaganie kierownicy'


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS
##    samochodów marki Audi

b <- auta2012 %>%
  filter(Marka == "Audi") %>%
  mutate(Model = sub("[0-9]", "", Model)) %>%
  filter(Model %in% c("A", "S", "RS")) %>%
  group_by(Model) %>%
  summarise(KM_sr = mean(KM, na.rm = T), KM_med = median(KM, na.rm = T))


## Odp: KM srednia: A: 158, S: 344, RS: 500; KM mediana A: 140, S: 344, RS: 450


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>%
  select(Marka, Model, Kolor) %>%
  group_by(Marka) %>%
  filter(n() > 10000) %>%
  group_by(Marka, Model) %>%
  summarise(liczba = n(), ulubiony_kolor = names(which.max(table(Kolor)))) %>%
  filter(liczba == max(liczba))


## Odp: Audi: A4, czarny-metallic; BMW: 320, srebrny-metallic;
## Ford: Focus, srebrny-metallic; Mercedes-Benz: C 220, srebrny-metallic;
## Opel: Astra, srebrny-metallic; Renault: Megane, srebrny-metallic;
## Volkswagen: Passat, srebrny-metallic