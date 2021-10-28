library(PogromcyDanych)
library(dplyr)
colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

View(auta2012)
## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(suma = n()) %>% 
  top_n(1,suma)

## Odp: 
#rok: 2011, ilość: 17418 

## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(suma = n()) %>% 
  top_n(1,suma)

## Odp:
#Skoda

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji <= 2011, Rok.produkcji >= 2005, Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  summarise(suma = n())

## Odp:
#59534

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Marka) %>% 
  summarise(średnia = mean(Cena.w.PLN)) %>% 
  top_n(1,średnia)

## Odp:
#Porsche (średnia cena to 345669zł)

## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011, Marka == 'Skoda') %>% 
  group_by(Model) %>% 
  summarise(średnia = mean(Cena.w.PLN)) %>% 
  arrange(średnia) %>% 
  head(1)

## Odp:
#Fabia (średnia cena to 42015zł)

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  mutate(Cena.do.KM = Cena.w.PLN/KM) %>% 
  filter(Liczba.drzwi == '2/3', Cena.do.KM > 600 ) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(suma = n()) %>% 
  top_n(1,suma)


## Odp: 
#automatyczna (708 samochodów z tą skrzynią)

## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == 'Skoda') %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(średnia.cena = mean(Cena.w.PLN)) %>% 
  filter(Rodzaj.paliwa == 'benzyna' | Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Model) %>% 
  summarise(różnica = sd(średnia.cena)) %>% 
  arrange(różnica) %>% 
  head(1)

## Odp: 
#Felicia

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini


autaL <- auta2012 %>% 
  filter(Marka == 'Lamborghini') 
  
autaDod <- as.character(autaL$Wyposazenie.dodatkowe) %>% 
    strsplit(split = ", ", T) %>% 
    unlist()

table(autaDod)

## Odp: 
# najmniej (1 wystąpienie): klatka, blokada skrzyni biegów;
# najwięcej (18 wystąpień): wspomaganie kierownicy, ABS, alufelgi;

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

audiAuta <- auta2012 %>% 
              filter(Marka == 'Audi')  

audiAuta[grep("A", audiAuta$Model),] %>%
  summarise(średnia = mean(KM, na.rm = TRUE), mediana = median(KM, na.rm = TRUE)) 

audiAuta[grep("S", audiAuta$Model),] %>%
  summarise(średnia = mean(KM, na.rm = TRUE), mediana = median(KM, na.rm = TRUE))  

audiAuta[grep("RS", audiAuta$Model),] %>%
  summarise(średnia = mean(KM, na.rm = TRUE), mediana = median(KM, na.rm = TRUE))   
    
## Odp:
# model A  - średnia: 159.5799 mediana: 140
# model S  - średnia: 376.3714 mediana: 354
# model RS - średnia: 493.2703 mediana: 450

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
  popAuta <- auta2012 %>% 
              group_by(Marka) %>% 
              summarise(suma = n()) %>% 
              filter(suma > 10000)  
  
  for (i in c(1:dim(popAuta)[1])){
    
    popModel <- auta2012 %>% 
                  filter(Marka == unlist(popAuta[i,1])) %>% 
                  group_by(Model) %>% 
                  summarise(suma = n()) %>% 
                  top_n(1, suma) 
    
    popKolor <- auta2012 %>% 
      filter(Marka == unlist(popAuta[i,1]), Model == unlist(popModel[1,1])) %>% 
      group_by(Kolor) %>% 
      summarise(suma = n()) %>% 
      top_n(1, suma)
    
    print(c(popKolor[1,1],popModel[1,1]))
  }

## Odp: 
# Popularne marki: Audi, BMW, Ford, Mercedes-Benz, Opel, Renault, Volkswagen
# Audi A4 - czarny-metallic, BMW 320 - srebrny-metallic, Ford Focus - srebrny-metallic, 
# Mercedes-Benz C 220 - srebrny-metallic, Opel Astra - srebrny-metallic, Renault Megane srebrny-metallic,
# Volkswagen Passat - srebrny-metallic
  