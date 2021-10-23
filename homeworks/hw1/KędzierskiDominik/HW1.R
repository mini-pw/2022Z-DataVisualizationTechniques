library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwiêcej aut i ile ich jest?
auta2012 %>%
  group_by(Rok.produkcji) %>%
  summarise(count = n()) %>%
  arrange(-count)
  

## Odp: Najwiêcej jest z 2011 roku, jest ich 17418 


## 2. Która marka samochodu wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku?
auta2012 %>%
  filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>%
  summarise(count = n()) %>%
  arrange(-count)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>%
  filter(Rok.produkcji > 2004 & 
           Rok.produkcji < 2012 &
           Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  dim()
  

## Odp: 59534


## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest œrednio najdro¿sza?
auta2012 %>%
  filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(srednia = mean(Cena.w.PLN, na.rm = TRUE)) %>%
  arrange(-srednia)


## Odp: Porsche


## 5. Spoœród aut marki Skoda wyprodukowanych w 2011 roku, który model jest œrednio najtañszy?
auta2012 %>%
  filter(Rok.produkcji == 2011 & Marka == "Skoda") %>%
  group_by(Model) %>%
  summarise(srednia = mean(Cena.w.PLN, na.rm = TRUE)) %>%
  arrange(srednia)


## Odp: Fabia


## 6. Która skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  filter(Liczba.drzwi == "2/3" & Cena.w.PLN/KM > 600) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(count = n()) %>%
  arrange(-count)
## Odp: automatyczna 


## 7. Spoœród aut marki Skoda, który model ma najmniejsz¹ ró¿nicê œrednich cen 
##    miêdzy samochodami z silnikiem benzynowym, a diesel?
library(tidyr)
auta2012 %>%
  filter(Marka == "Skoda" &  
           (Rodzaj.paliwa == "benzyna" | 
            Rodzaj.paliwa == "olej napedowy (diesel)")) %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(srednia = mean(Cena.w.PLN, na.rm = TRUE)) %>%
  pivot_wider(names_from = Rodzaj.paliwa,
              values_from = srednia) %>%
  rename("diesel" = "olej napedowy (diesel)") %>%
  mutate(roznica = abs(benzyna - diesel)) %>%
  arrange(roznica)  


## Odp: Felicia 


## 8. ZnajdŸ najrzadziej i najczêœciej wystêpuj¹ce wyposa¿enie/a dodatkowe 
##    samochodów marki Lamborghini
library(stringr)
auta2012 %>%
  filter(Marka == "Lamborghini") %>%
  select(Wyposazenie.dodatkowe) %>%
  mutate(Wyposazenie.dodatkowe = str_split(.$Wyposazenie.dodatkowe, ", ")) %>%
  unnest() %>%
  group_by(Wyposazenie.dodatkowe) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  #head(3)
  tail(4)      

## Odp: Najczeœciej wspomaganie kierownicy, alufelgi i ABS, najrzadziej blokada skrzyni biegów i klatka


## 9. Porównaj œredni¹ i medianê mocy KM miêdzy grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>%
  filter(Marka == "Audi") %>%
  group_by(Model = str_sub(Model, start = 1, end = 1)) %>%
  summarise(srednia = mean(KM, na.rm = TRUE), 
            mediana = median(KM, na.rm = TRUE)) %>%
  filter(Model == "A" | Model == "S" | Model == "R")
  

## Odp: A: srednia 160 mediana 140
##      S: srednia 344 mediana 344
##     RS: srednia 482 mediana 450  

## 10. ZnajdŸ marki, których auta wystêpuj¹ w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka¿dej z tych marek.
TopBrand <- auta2012 %>%
  group_by(Marka) %>%
  summarise(count = n()) %>%
  filter(count > 10000)
TopBrandModel <- auta2012 %>%
  filter(Marka == TopBrand$Marka) %>%
  group_by(Marka, Model) %>%
  summarise(count = n()) %>%
  arrange(Marka, -count) 

TopModelCount <- TopBrandModel %>%
  group_by(Marka) %>%
  summarise(max = max(count))
  
TopModel <- TopBrandModel %>%
  filter(Marka %in% TopModelCount$Marka & count %in% TopModelCount$max )
  
TopModelColor <- auta2012 %>%
  filter(Marka %in% TopModel$Marka & Model %in% TopModel$Model) %>%
  group_by(Model, Kolor, Marka) %>%
  summarise(count = n()) %>%
  arrange(Model, -count)
  
  
## Odp: 1. BMW 320 srebrny-metalic
##      2. Audi A4 czarny-metalic
##      3. Opel Astra srebrny-metalic
##      4. Mercedes c220 srebrny-metalic
##      5. Ford Focus srebrny-metalic
##      6. Renault Megane srebrny-metalic
##      7. Volkswagen Passat srebrny-metalic

