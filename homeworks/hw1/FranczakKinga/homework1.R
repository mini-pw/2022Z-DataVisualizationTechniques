library(PogromcyDanych)
library(dplyr)
library(stringi)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  select(Rok.produkcji) %>% 
  group_by(Rok.produkcji) %>% 
  summarise(Liczba = n()) %>% 
  arrange(-Liczba)

## Odp: Rok: 2011, liczba samochodów: 17 418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  select(Marka) %>% 
  group_by(Marka) %>% 
  summarise(Liczba = n()) %>% 
  arrange(-Liczba)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  select(Rodzaj.paliwa, Rok.produkcji) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  filter(Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>% 
  summarise(Liczba = n())


## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  select(Marka, Rok.produkcji, Cena.w.PLN, Rodzaj.paliwa) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
  arrange(-Srednia.cena)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  select(Marka, Rok.produkcji, Cena.w.PLN, Model) %>% 
  filter(Marka == "Skoda", Rok.produkcji==2011) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
  arrange(Srednia.cena)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  select(Cena.w.PLN, KM, Liczba.drzwi, Skrzynia.biegow) %>% 
  filter(is.na(KM) == FALSE, Liczba.drzwi == "2/3") %>% 
  mutate(Cena.do.KM = Cena.w.PLN/KM) %>% 
  filter(Cena.do.KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(Liczba.skrzyni = n()) %>% 
  arrange(-Liczba.skrzyni)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta2012 %>% 
  select(Cena.w.PLN, Marka, Rodzaj.paliwa, Model) %>% 
  filter(Marka == "Skoda") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" | Rodzaj.paliwa == "benzyna") %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) -> auta_pom
auta_ben <- auta_pom %>% 
  filter(Rodzaj.paliwa == "benzyna") %>% 
  select(Model, Srednia.cena)
colnames(auta_ben)[2] <- "Srednia.cena.benzyna"
auta_disel <- auta_pom %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  select(Model, Srednia.cena)
colnames(auta_disel)[2] <- "Srednia.cena.disel"
auta <- inner_join(auta_ben, auta_disel, by = "Model")
auta %>% 
  mutate(Roznica.cen = abs(Srednia.cena.benzyna - Srednia.cena.disel)) %>% 
  select(Model, Roznica.cen) %>% 
  arrange(Roznica.cen)

## Odp: Skoda Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

a <- auta2012 %>% 
  select(Marka, Wyposazenie.dodatkowe) %>% 
  filter(Marka == "Lamborghini") 

a <- a %>% 
  transmute(Wyposazenie.dodatkowe = stri_split_fixed(Wyposazenie.dodatkowe, ", "))
Wyposazenie <- unlist(a)
auta <- data.frame(Wyposazenie)
auta <- auta %>% 
  group_by(Wyposazenie) %>% 
  summarise(Ilosc.wystepowania = n()) %>% 
  filter(Wyposazenie != "")
auta %>% 
  arrange(-Ilosc.wystepowania)
auta %>% 
  arrange(Ilosc.wystepowania)

## Odp: Najczęściej występujące: ABS, alufelgi i wspomaganie kierownicy (18 razy)
##      Najrzadziej występujące: blokada skrzyni biegów i klatka (1 raz)


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  select(Marka, Model, KM) %>% 
  filter(Marka == "Audi") %>% 
  select(-Marka) %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(substring(Model, 1, 1) == "A" | substring(Model, 1, 1) == "S" | 
           substring(Model, 1, 2) == "RS") %>% 
  transmute(Model = substring(Model, 1, 1), KM = KM) %>% 
  filter(!is.na(KM)) %>% 
  group_by(Model) %>% 
  summarise(Srednia.KM = mean(KM), Mediana.KM = median(KM))

## Odp: Modele od najmniejszej do największej średniej mocy: A , S, RS
##      Mediany są uszeregowane w tej samej kolejności
##      Dla modeli A i RS średnia mocy jest większa od mediany mocy, dla modelu S jest na odwrót


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
w <- auta2012 %>% 
  select(Marka) %>% 
  group_by(Marka) %>% 
  summarise(Liczba.aut = n()) %>% 
  filter(Liczba.aut > 10000) %>% 
  arrange(Liczba.aut) %>% 
  select(Marka)

Marka1 <- as.character(unlist(w))
Model <- NULL
Kolor <- NULL

for(i in 1:length(Marka1)){
  auta <- auta2012 %>% 
    select(Marka, Model) %>% 
    mutate_if(is.factor, as.character) %>% 
    filter(Marka == Marka1[i]) %>% 
    group_by(Model) %>% 
    summarise(Liczba.modeli = n()) %>% 
    arrange(-Liczba.modeli)
  model <- unlist(auta[1,1])
  
  Model <- c(Model, model)
  
  auta <- auta2012 %>% 
    select(Marka, Model, Kolor) %>% 
    mutate_if(is.factor, as.character) %>% 
    filter(Model == model, Marka == Marka1[i]) %>% 
    filter(Kolor != "") %>% 
    group_by(Kolor) %>% 
    summarise(Liczba.sam.koloru = n()) %>% 
    arrange(-Liczba.sam.koloru) 
  
  kolor <- unlist(auta[1,1])
  Kolor <- c(Kolor, kolor)
}
Marka <- Marka1
wynik <- data.frame(Marka, Model, Kolor)
## Odp: dane w ramce wynik
