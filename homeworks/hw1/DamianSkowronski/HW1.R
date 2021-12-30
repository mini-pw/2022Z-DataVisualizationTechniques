library(PogromcyDanych)
library(dplyr)
library(stringr) #w zadaniu 7 uzywam funkcji str_split
library(tidyr) #w zadaniu 8 uzywam funkcji seperate_rows 

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
  
## Odp: Najwiecej aut jest z rocznika 2011 i jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

## Odp: Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rok.produkcji<=2011, Rok.produkcji>=2005, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(n=n())
  
## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(Srednia.Cena = mean(Cena.w.PLN)) %>% 
  arrange(-Srednia.Cena)

## Odp: Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Rok.produkcji == 2011, Marka == "Skoda") %>% 
  group_by(Model) %>% 
  summarise(Srednia.Cena = mean(Cena.w.PLN)) %>% 
  arrange(Srednia.Cena)

## Odp: Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  mutate(PLNdoKM = Cena.w.PLN/KM) %>% 
  filter(PLNdoKM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)

## Odp: Automatyczna.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

meanBenzyna <-  auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna") %>%  
  group_by(Model) %>% 
  summarise(meanB = mean(Cena.w.PLN))
  
meanDiesel <- auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa =="olej napedowy (diesel)") %>%  
  group_by(Model) %>% 
  summarise(meanD = mean(Cena.w.PLN))

full_join(meanBenzyna,meanDiesel, by = "Model") %>% 
  mutate(Roznica = abs(meanD - meanB)) %>% 
  arrange(Roznica)

## Odp: Felicia.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

wyposazenia <- auta2012 %>% 
  filter(Marka== "Lamborghini") %>%
  select(Wyposazenie.dodatkowe) %>% 
  separate_rows(Wyposazenie.dodatkowe, sep = ", ") %>% 
  count(Wyposazenie.dodatkowe,name = "liczba") %>% 
  arrange(-liczba)

head(wyposazenia,5)
tail(wyposazenia,5)
  
## Odp: Najczęściej występujące wyposażenia to: ABS, alufelgi, wspomaganie kierownicy.
##      Najrzadziej występujące wyposażenia to blokada skrzyni biegów i klatka.


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

KM_sr_med <- auta2012 %>% 
  filter(Marka == "Audi") %>%
  mutate(Model_new = case_when(substr(Model,1,1) == "A" ~ "A",
                               substr(Model,1,1) == "S" ~ "S",
                               substr(Model,1,2) == "RS" ~ "RS")) %>% 
  filter(Model_new == "A" | Model_new == "S" | Model_new == "RS") %>% 
  group_by(Model_new) %>% 
  summarise(Srednia = mean(KM, na.rm = TRUE), Mediana = median(KM, na.rm = TRUE))

KM_sr_med %>% 
  arrange(-Srednia)

KM_sr_med %>% 
  arrange(-Mediana)

## Odp: Srednia: RS > S > A. Mediana RS > S > A, czyli tak samo jak średnia.


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

popularMarki <- auta2012 %>%
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  filter(n > 10000)

popularModele <- auta2012 %>%  #wydaje mi się, że jakoś mocno naokoło to zrobiłem 
  filter(Marka == as.character(popularMarki$Marka)) %>% 
  group_by(Marka,Model) %>% 
  summarise(n = n()) %>% 
  mutate(Max = max(n)) %>% 
  filter(n == Max)

auta2012 %>%  # tu robie w sumie to same co w popularModele
  filter(Model == as.character(popularModele$Model)) %>% 
  group_by(Marka,Model,Kolor) %>%
  summarise(n = n()) %>% 
  mutate(Max = max(n)) %>% 
  filter(n == Max)
  
 
## Odp: Marki, których auta występują ponad 10000 razy to: Audi, BMW, Ford, Mercedes-Benz, Opel, Renault, Volkswagen.
##      Najpolularniejsze kolory najlpopularniejszych modeli to:
##      
##      Marka         Model  Kolor
##
##      Audi          A4     czarny-metallic 
##      BMW           320    srebrny-metallic 
##      Ford          Focus  srebrny-metallic
##      Mercedes-Benz C 220  srebrny-metallic
##      Opel          Astra  srebrny-metallic
##      Renault       Megane srebrny-metallic
##      Volkswagen    Passat srebrny-metallic
