#Zadanie domowe 1
#Kacper Skonieczka 313505
library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>%
  group_by(Rok.produkcji) %>%
  summarise(n = n()) %>%
  top_n(1, n)

## Odp: 2011, jest ich 17418



## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>%
  filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  top_n(1,n)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>%
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2021 
         & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  summarise(n = n()) 

## Odp: 59888


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>%   
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>%
  summarise(SredniaCena = mean(Cena.w.PLN)) %>%
  top_n(1,SredniaCena)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>%   
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  group_by(Model) %>%
  summarise(SredniaCena = mean(Cena.w.PLN)) %>%
  top_n(1,-SredniaCena)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  filter(Liczba.drzwi == "2/3" & Cena.w.PLN/Przebieg.w.km > 600) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(n = n()) %>%
  top_n(1,n)
  

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
library(tidyr)
auta2012 %>%   
  filter(Marka == "Skoda" & (Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)")) %>%
  select(Model, Rodzaj.paliwa, Cena.w.PLN) %>%
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(SredniaCena = mean(Cena.w.PLN)) %>%
  group_by(Model) %>%
  mutate(CzyObieWesje = ifelse(n() == 2, TRUE, FALSE)) %>%
  filter(CzyObieWesje == TRUE) %>%
  pivot_wider(names_from = Rodzaj.paliwa, values_from = SredniaCena)  %>%
  `colnames<-`(c("Model", "CzyObieWersje", "SredniaCenaBenzynowych", "SredniaCenaDieselowych")) %>%
  mutate(RoznicaCen = abs(SredniaCenaBenzynowych - SredniaCenaDieselowych)) %>%
  arrange(RoznicaCen)
  
## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
library(stringr)
auta2012 %>%
  filter(Marka == "Lamborghini" & Wyposazenie.dodatkowe != "") %>%
  select(Wyposazenie.dodatkowe) %>% 
  .$Wyposazenie.dodatkowe %>%
  str_split(pattern = ", ", simplify = TRUE) %>%
  as.vector() %>%
  as.data.frame() %>%
  `colnames<-`("WyposazenieDodatkowe") %>%
  filter(WyposazenieDodatkowe != "") %>% 
  group_by(WyposazenieDodatkowe) %>%
  summarise(n= n(), .groups = "drop") %>%
  filter(n == min(n) | n == max(n)) %>%
  arrange(n)
    

## Odp:  Najczesciej występujące wyposażenie/a dodatkowe: ABS, alufelgi, wspomaganie kierownicy, 
##       najrzadziej występujące wyposażenie/a dodatkowe: klatka, blokada skrzyni biegow


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>%
  filter(Marka == "Audi") %>%
  select(Model, KM) %>%
  filter(str_detect(Model,"^A") | str_detect(Model,"^RS") | str_detect(Model,"^S")) %>%
  mutate(GrupaModeli = ifelse(str_detect(Model,"A"), "A", ifelse(str_detect(Model,"RS"),"RS","S"))) %>%
  group_by(GrupaModeli) %>%
  summarise(SredniaKM = mean(KM, na.rm = TRUE), MedianaKM = median(KM, na.rm = TRUE)) 

## Odp: Najwyzsza srednia moc KM i zarazem jej mediane ma grupa modeli RS,
##      grupa modeli A ma z kolei najniższe te wartości



## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012 %>% 
  group_by(Marka) %>%
  summarise(n = n()) %>%
  filter(n > 10000) %>%
  select(Marka) %>%
  transmute(as.character(Marka)) %>%
  pull(1) -> marki

auta2012 %>%
  filter(Marka %in% marki) %>%
  group_by(Marka, Model) %>%
  summarise(n = n())  %>%
  top_n(1,n) %>%
  transmute(as.character(Model)) %>%
  pull(2) -> modele

auta2012 %>%
  filter((Model %in% modele) & !(Marka == "Mercedes-Benz" & Model == "320")) %>%      ## Model 320 to najpopularniejszy model BMW
  group_by(Marka, Model, Kolor) %>%
  summarise(n = n()) %>%
  top_n(1,n) 

  

## Odp: Podam odpowiedz w nastepujacy sposob (marka, model, kolor), gdzie marki beda tych aut
## ktore wystepuja w danych ponad 10000 razy, modele to najpopularniejsze modele tych marek, 
## a kolory to te najpopuarniejsze tych modeli:
## (Audi, A4,    czarny-metallic)
## (BMW, 320,    srebrny-metallic)
## (Ford, Focus,  srebrny-metallic)
## (Mercedes-Benz, C 220,  srebrny-metallic)
## (Opel, Astra,  srebrny-metallic)
## (Renault, Megane, srebrny-metallic)
## (Volkswagen, Passat, srebrny-metallic)