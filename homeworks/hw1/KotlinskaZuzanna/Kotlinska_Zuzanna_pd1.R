library(PogromcyDanych)
library(dplyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))


## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)


## Odp: Rocznik 2011, liczba aut 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)


## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011) %>% 
  count()


## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2011) %>%
  mutate(Cena.brutto = ifelse(Brutto.netto == "brutto", Cena.w.PLN, 
                              Cena.w.PLN * 1.23)) %>% 
  group_by(Marka) %>% 
  summarise(Sredni.koszt = mean(Cena.brutto)) %>% 
  arrange(-Sredni.koszt) %>% 
  head(1)
  
  
## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
  
auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>%
  mutate(Cena.brutto = ifelse(Brutto.netto == "brutto", Cena.w.PLN, 
                              Cena.w.PLN * 1.23)) %>%
  group_by(Model) %>% 
  summarise(Sredni.koszt = mean(Cena.brutto)) %>%
  arrange(Sredni.koszt) %>% 
  head(1)
    

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>%
  filter(Liczba.drzwi == "2/3" & Cena.w.PLN / KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)
  

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta.benzyna <- auta2012 %>% 
  filter(Marka == "Skoda" & Rodzaj.paliwa == "benzyna") %>% 
  mutate(Cena.brutto = ifelse(Brutto.netto == "brutto", Cena.w.PLN, 
                            Cena.w.PLN * 1.23)) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena.b = mean(Cena.brutto))


auta.diesel <- auta2012 %>% 
  filter(Marka == "Skoda" & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  mutate(Cena.brutto = ifelse(Brutto.netto == "brutto", Cena.w.PLN, 
                              Cena.w.PLN * 1.23)) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena.d = mean(Cena.brutto))


inner_join(auta.benzyna, auta.diesel, by="Model" ) %>% 
  mutate(Roznica.cena = abs(Srednia.cena.b - Srednia.cena.d)) %>% 
  arrange(Roznica.cena) %>% 
  head(1)


## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

auta2012_new <- filter(auta2012, Marka == "Lamborghini")
wyposazenie <- str_split(auta2012_new$Wyposazenie.dodatkowe, ", ")
wyposazenie %>% 
  unlist() %>% 
  table() %>% 
  sort()

## Odp: Najczęściej - 18 - ABS, alufelgi, wspomaganie kierownicy
##      Najrzadziej - 1 - blokada skrzyni biegów, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
  
auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(Seria = ifelse(substr(Model, 2, 2) == "S",
                        substr(Model, 1, 2), substr(Model, 1, 1))) %>%
  group_by(Seria) %>% 
  filter(Seria == "A" | Seria == "S" | Seria == "RS") %>% 
  summarise(Srednia.moc = mean(KM, na.rm = TRUE), Mediana.mocy = median(KM, na.rm = TRUE))
  

## Odp: Średnia moc: A - 160, RS - 500, S - 344
##      Mediana mocy: A - 140, RS - 450, S - 344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% 
  select(Marka, Model, Kolor) %>% 
  group_by(Marka) %>% 
  mutate(liczba.aut = n()) %>% 
  filter(liczba.aut > 10000) %>% 
  mutate(liczba.modeli = n()) %>% 
  group_by(Marka, Model, Kolor) %>% 
  mutate(liczba.kolorow = n()) %>% 
  unique() %>% 
  arrange(-liczba.kolorow) %>% 
  group_by(Marka) %>% 
  filter(liczba.modeli == max(liczba.modeli)) %>% 
  filter(liczba.kolorow == max(liczba.kolorow))

  
## Odp: Audi - A4 - czarny metallic, 
##      BMW - 320 - srebrny metallic, 
##      Ford - Focus - srebrny metallic,
##      Mercedes-Benz - C 220 - srebrny metallic, 
##      Opel - Astra - srebrny metallic, 
##      Renault - Scenic - srebrny metallic, 
##      Volkswagen - Passat - srebrny metallic