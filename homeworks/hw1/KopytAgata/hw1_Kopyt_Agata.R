library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>%
  group_by(Rok.produkcji) %>% 
  summarise(ilosc_z_rocznika = n()) %>% 
  arrange(-ilosc_z_rocznika) %>% 
  head(1)

## Odp: Z 2011, jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>%
  filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>% 
  summarise(ilosc_z_marki = n()) %>% 
  arrange(-ilosc_z_marki) %>% 
  head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>%
  filter(Rok.produkcji>= 2005,Rok.produkcji<= 2011,Rodzaj.paliwa == "olej napedowy (diesel)" ) %>%
  summarise(ilosc_z_dieslem = n())

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>%
  filter(Rok.produkcji== 2011,Rodzaj.paliwa == "olej napedowy (diesel)" ) %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(-srednia_cena) %>% 
  head(1)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>%
  filter(Rok.produkcji== 2011, Marka == "Skoda" ) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(srednia_cena) %>% 
  head(1)

## Odp: Skoda Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  filter(Liczba.drzwi == "2/3") %>% 
  mutate(stosunek_PLN_KM = Cena.w.PLN/KM)%>%
  filter(stosunek_PLN_KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(ilosc_z_skrzynia = n()) %>% 
  arrange(-ilosc_z_skrzynia) %>% 
  head(1)

## Odp: Automatyczna skrzynia biegów


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>%
  filter(Marka == "Skoda", (Rodzaj.paliwa == "olej napedowy (diesel)" | Rodzaj.paliwa == "benzyna")) %>% 
  group_by(Model,Rodzaj.paliwa) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN, na.rm = TRUE )) %>%
  summarise(roznica_srednich_cen = abs(srednia_cena[Rodzaj.paliwa == "benzyna"] - srednia_cena[Rodzaj.paliwa == "olej napedowy (diesel)"])) %>%
  arrange(roznica_srednich_cen) %>% 
  head(1)
  
## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
auta2012 %>% 
  select(Marka, Wyposazenie.dodatkowe) %>% 
  filter(Marka == "Lamborghini") %>% 
  transmute(Wyposazenie.dodatkowe = str_split(Wyposazenie.dodatkowe, ', ')) %>% 
  unlist() %>% 
  as.data.frame() %>%
  sapply(table) %>%  as.data.frame() %>% 
  filter(. == max(.)| . == min(.)) %>% 
  View()
  

## Odp: Najczęściel: ABS, alufelgi, wspomaganie kierownicy ; Najrzadziej: blokada skrzyni biegAlw, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>%
  filter(Marka == "Audi") %>%
  mutate(Model = str_replace(Model,regex('^A[0-9]*$'),"A"), Model = str_replace(Model,regex('^S[0-9]*$'),"S"), Model = str_replace(Model,regex('^RS[0-9]*$'),"RS")) %>% 
  filter(Model == "A"|Model == "S"|Model == "RS") %>%
  group_by(Model) %>% 
  summarise(srednia_KM = mean(KM, na.rm = TRUE ), mediana_KM = median(KM, na.rm = TRUE)) %>%
  head(3)

## Odp: A : srednia = 158, mediana = 140 ; S: srednia = 344, mediana 344 ; RS: srednia = 500, mediana = 450  
  


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012 %>%
  group_by(Marka) %>% 
  select(Marka,Model,Kolor) %>% 
  mutate(ilosc_z_marki = n()) %>%
  filter(ilosc_z_marki>10000) %>%
  group_by(Marka,Model) %>% 
  mutate(ilosc_z_modelu = n()) %>% 
  group_by(Marka,Model,Kolor) %>% 
  mutate(ilosc_z_koloru = n()) %>% 
  unique() %>% 
  group_by(Marka,Model) %>% 
  filter(ilosc_z_koloru == max(ilosc_z_koloru)) %>% 
  group_by(Marka) %>% 
  filter(ilosc_z_modelu == max(ilosc_z_modelu)) %>% 
  select(Marka,Kolor) %>% 
  View()

## Odp: Ford - srebrny-metallic ; Volkswagen - srebrny-metallic ; Opel - srebrny-metallic ; 
##      BMW - srebrny-metallic ; Renault - srebrny-metallic ; Mercedes-Benz - srebrny-metallic ; 
##      Audi - czarny-metallic