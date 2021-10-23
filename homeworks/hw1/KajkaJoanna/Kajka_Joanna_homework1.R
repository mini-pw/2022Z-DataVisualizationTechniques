install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
# dodatkowe pakiety
library(tidyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwiêcej aut i ile ich jest?
auta2012 %>% 
  count(Rok.produkcji, sort = TRUE) %>% 
  head(1)

## Odp: Rocznik: 2011; iloœæ: 17418;


## 2. Która marka samochodu wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji==2011) %>% 
  count(Marka, sort = TRUE) %>% 
  head(1)

## Odp: Marka: Skoda;


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(2005 <= Rok.produkcji & Rok.produkcji <= 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count()

## Odp: 59534


## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest œrednio najdro¿sza?
auta2012 %>% 
  filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  top_n(1)

## Odp: Porsche


## 5. Spoœród aut marki Skoda wyprodukowanych w 2011 roku, który model jest œrednio najtañszy?
auta2012 %>% 
  filter(Marka == "Skoda"& Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(srednia_cena) %>% 
  head(1)

## Odp: Fabia


## 6. Która skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut, których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3" & Cena.w.PLN/KM > 600) %>% 
  count(Skrzynia.biegow, sort = TRUE) %>% 
  top_n(1)

## Odp: automatyczna


## 7. Spoœród aut marki Skoda, który model ma najmniejsz¹ ró¿nicê œrednich cen miêdzy samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda" & (Rodzaj.paliwa == "olej napedowy (diesel)" | Rodzaj.paliwa == "benzyna")) %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>%  
  group_by(Model) %>% 
  mutate(diff = abs(srednia_cena - lag(srednia_cena))) %>% 
  arrange(diff) %>% 
  head(1)

## Odp: Felicia


## 8. ZnajdŸ najrzadziej i najczêœciej wystêpuj¹ce wyposa¿enie/a dodatkowe samochodów marki Lamborghini
ramka <- auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe)
nowa_ramka = separate_rows(ramka, 1, sep= ",") %>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  count() %>% 
  arrange(n)
# najrzadziej wystêpuj¹ce wyposa¿enie
nowa_ramka %>% 
  arrange(n)
# najczêœciej wystêpuj¹ce wyposa¿enie
nowa_ramka %>% 
  arrange(-n)

## Odp: Najczêœciej: alufelgi, wspomaganie kierownicy, ABS; Najrzadziej: blokada skrzyni biegów, klatka;


## 9. Porównaj œredni¹ i medianê mocy KM miêdzy grupami modeli A, S i RS samochodów marki Audi
auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(X = case_when(str_starts(Model, "A") ~ "A",
                       str_starts(Model, "RS") ~ "RS", 
                       str_starts(Model, "S") ~ "S",
                       TRUE ~ "else")) %>% 
  filter(X == "A" | X == "S" | X == "RS") %>% 
  group_by(X) %>% summarise(meanKM = mean(KM, na.rm = T), medianKM = median(KM, na.rm = T)) %>% 
  mutate(diff = abs(meanKM - medianKM))
  

## Odp: Dla modelu A ró¿nica wynosi 19.6, dla modelu S 50.0, a dla modelu RS 0.263.


## 10. ZnajdŸ marki, których auta wystêpuj¹ w danych ponad 10000 razy. Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka¿dej z tych marek.

auta2012 %>% 
  group_by(Marka) %>% 
  count() %>% 
  filter(n > 10000)

def_model_kolor <- function(marka) {
  model = auta2012 %>% 
    filter(Marka == marka) %>% 
    group_by(Model) %>% 
    count() %>% 
    arrange(-n) %>% 
    head(1)
  kolor = auta2012 %>% 
    filter(Marka == marka & Model == model$Model) %>% 
    group_by(Kolor) %>% 
    count() %>% 
    arrange(-n) %>% 
    head(1)
  paste(model$Model, kolor$Kolor)
}

def_model_kolor("Audi")
def_model_kolor("BMW")
def_model_kolor("Ford")
def_model_kolor("Mercedes-Benz")
def_model_kolor("Opel")
def_model_kolor("Renault")
def_model_kolor("Volkswagen")

## Odp: Marka :      Audi       :       BMW       :     Ford        :   Mercedes-Benz  :      Opel        :     Renault     :   Volkswagen
##      Model :       A4        :       320       :     Focus       :     C 220        :      Astra       :     Megane      :     Passat
##      Kolor : czarny-metallic :srebrny-metallic : srebrny-metallic: srebrny-metallic : srebrny-metallic : srebrny-metallic: srebrny-metallic