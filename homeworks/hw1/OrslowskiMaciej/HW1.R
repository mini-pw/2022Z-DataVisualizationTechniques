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
  summarise(Liczba = n()) %>% 
  arrange(-Liczba)
## Odp: 2011 - 17418 aut


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(Liczba = n()) %>% 
  arrange(-Liczba)
## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011 &
           str_detect(Rodzaj.paliwa, "diesel")) %>% 
  group_by(Rodzaj.paliwa) %>%
  summarise(Liczba = n())
## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2011 & str_detect(Rodzaj.paliwa, "diesel")) %>% 
  group_by(Marka) %>% 
  summarise(Srednia.cena = mean(Cena)) %>% 
  arrange(-Srednia.cena)
## Odp: Volvo


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena)) %>% 
  arrange(Srednia.cena)
## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  mutate(Stosunek.PLN.do.KM = Cena.w.PLN / KM) %>% 
  filter(Stosunek.PLN.do.KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(Liczba = n()) %>% 
  arrange(-Liczba)
## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda") %>% 
  filter(Rodzaj.paliwa == 'benzyna' | str_detect(Rodzaj.paliwa, "diesel")) %>% 
  group_by(Model) %>% 
  summarise(
    Srednia.cena.diesel = mean(
      ifelse(str_detect(Rodzaj.paliwa, 'diesel'), Cena, NA), 
      na.rm = TRUE),
    Srednia.cena.benzyna = mean(
      ifelse(Rodzaj.paliwa == 'benzyna', Cena, NA),
      na.rm = TRUE)) %>% 
  mutate(Roznica.srednich.cen = abs(Srednia.cena.benzyna - Srednia.cena.diesel)) %>% 
  arrange(Roznica.srednich.cen)
## Odp: Praktik


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
wyposazenia_lamborgnini <- str_split(
  filter(auta2012, Marka == 'Lamborghini')$Wyposazenie.dodatkowe,
  pattern = ', '
) %>% unlist() %>% 
  data.frame(Wyposazenie = .) %>% 
  group_by(Wyposazenie) %>% 
  summarise(Liczba = n())

wyposazenia_lamborgnini %>% 
  arrange(Liczba)
wyposazenia_lamborgnini %>% 
  arrange(-Liczba)
## Odp: Najrzadziej występujące: 
##        - blokada skrzyni biegow
##        - klatka
##      Najczęściej występujące:
##        - ABS
##        - alufelgi
##        - wspomaganie kierownicy


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka == "Audi" & !is.na(KM)) %>% 
  mutate(Model.grupa = str_replace_all(Model, "\\d.*", "")) %>% 
  select(Model.grupa, KM) %>% 
  filter(Model.grupa %in% c("A", "S", "RS")) %>%
  group_by(Model.grupa) %>% 
  summarise(Srednia.moc.KM = mean(KM), Mediana.mocy.KM = median(KM))
## Odp: RS > S > A (dotyczy to i średnich i median)


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
marki_10000 <- auta2012 %>% 
  group_by(Marka) %>% 
  summarise(Liczba = n()) %>% 
  filter(Liczba > 10000)
najpopularniejsze_modele <- auta2012 %>% 
  filter(Marka %in% marki_10000$Marka) %>% 
  group_by(Marka, Model) %>% 
  summarise(Liczba = n()) %>% 
  top_n(1)
najpopularniejsze_modele %>% 
  left_join(auta2012) %>% 
  filter(Kolor != '') %>% 
  group_by(Marka, Model, Kolor) %>% 
  summarise(Liczba = n()) %>% 
  top_n(1)
## Odp: Audi A4 -> czarny-metallic
##      BWM 320, Ford Focus, Mercedes-Benz C 220, Opel Astra, Renault Megane,
##        Volkswagen Passat -> srebrny-metallic
