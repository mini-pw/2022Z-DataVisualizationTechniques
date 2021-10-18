library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% select(Rok.produkcji) %>% group_by(Rok.produkcji) %>% 
  summarise(ilosc = n()) %>% arrange(-ilosc) %>% head(1)

## Odp: Rok produkcji 2011, jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>% summarise(ilosc = n()) %>% arrange(desc(ilosc)) %>% head(1)
  
## Odp: Najczesciej występująca marka Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011) %>%  
  filter( Rodzaj.paliwa == "olej napedowy (diesel)") %>% count()

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% summarise(SredniaCena = mean(Cena.w.PLN)) %>% arrange(desc(SredniaCena)) %>%  head(1)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% filter(Marka == "Skoda", Rok.produkcji == 2011) %>% group_by(Model) %>% 
  summarise(SredniaCena = mean(Cena)) %>% head(1);

## Odp:Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% filter(Liczba.drzwi == "2/3") %>% mutate(Stosunek = Cena.w.PLN/KM) %>%
  filter(Stosunek > 600) %>% group_by(Skrzynia.biegow) %>% 
  summarise(Ilosc.Skrzyni.biegow = n()) %>% arrange(-Ilosc.Skrzyni.biegow) %>%
  head(1)
  
## Odp: automatyczna 


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

benzyna <- auta2012 %>% filter(Marka == "Skoda") %>% group_by(Model, Rodzaj.paliwa) %>% 
  summarise(SredniaCena = mean(Cena.w.PLN)) %>% group_by(Model) %>% 
  filter(Rodzaj.paliwa == "benzyna")
diesel <- auta2012 %>% filter(Marka == "Skoda") %>% group_by(Model, Rodzaj.paliwa) %>% 
  summarise(SredniaCena = mean(Cena.w.PLN)) %>% group_by(Model) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)")
roznica <- inner_join(by = "Model", benzyna, diesel) %>% 
  mutate(RoznicaCen = abs(SredniaCena.x - SredniaCena.y)) %>% arrange(RoznicaCen) %>% 
  head(1)
roznica$Model

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
wyp <- auta2012 %>% filter(Marka == "Lamborghini") %>% select(Wyposazenie.dodatkowe)
wyp <- str_split(wyp$Wyposazenie.dodatkowe, ", ") 
wyp <- data.frame(wyposazenie = unlist(wyp2))
wyp <- wyp %>% group_by(wyposazenie) %>% summarise(ile = n())
slice_max(wyp, ile)
slice_min(wyp, ile)

## Odp: 


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
rs <- auta2012 %>% filter(Marka == "Audi") %>% filter(str_detect(Model, "^RS[0-9]")) %>% 
  summarise(Srednia = mean(KM, na.rm = TRUE), Mediana = median(KM, na.rm = TRUE))
s <- auta2012 %>% filter(Marka == "Audi") %>% filter(str_detect(Model, "^S[0-9]")) %>% 
  summarise(Srednia = mean(KM, na.rm = TRUE), Mediana = median(KM, na.rm = TRUE))
a <- auta2012 %>% filter(Marka == "Audi") %>% filter(str_detect(Model, "^A[0-9]")) %>% 
  summarise(Srednia = mean(KM, na.rm = TRUE), Mediana = median(KM, na.rm = TRUE))
rs
s
a
## Odp:> rs
# Srednia    Mediana
# 500.0282     450
# s
# Srednia    Mediana
# 343.7371     344
# a
# Srednia    Mediana
# 159.5799     140


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012
marki <- auta2012 %>% group_by(Marka) %>% summarise(ile = n()) 
new <- left_join(auta2012, marki, by.x = Marka, by.y = Marka)
new1 <- new %>% filter(ile>10000) %>% group_by(Marka, Model) %>% summarise(ile_model = n()) 
new2 <- new1 %>% group_by(Marka) %>% summarise(najczesciej = max(ile_model))
najczestsze_modele <- inner_join(new2, new1, by = c('Marka' = 'Marka', 'najczesciej' = 'ile_model'))
najczestsze_modele 
kolory <- inner_join(najczestsze_modele, auta2012, by = c("Marka" = "Marka", "Model" = "Model")) %>% 
  group_by(Marka, Model, Kolor) %>% summarise(ile = n()) 
naj_kolory <- inner_join(najczestsze_modele, auta2012, by = c("Marka" = "Marka", "Model" = "Model")) %>% 
  group_by(Marka, Model, Kolor) %>% summarise(ile = n()) %>% group_by(Marka, Model) %>% 
  summarise(najczestszy_kolor = max(ile))
inner_join(naj_kolory, kolory, by = c('Marka' = 'Marka','Model' = 'Model', "najczestszy_kolor" = "ile"))
## Odp: 
#Audi          A4                   853 czarny-metallic 
#BMW           320                  441 srebrny-metallic
#Ford          Focus               1711 srebrny-metallic
#Mercedes-Benz C 220                205 srebrny-metallic
#Opel          Astra               1539 srebrny-metallic
#Renault       Megane               733 srebrny-metallic
#Volkswagen    Passat              1466 srebrny-metallic