# Piotr Wilczyński
# Techniki Wizualizacji danych
# Politechnika Warszawska
# Wydział Matematyki i Nauk Informacyjnych
# Inżynieria i Analiza Danych
# 20/10/2012
# HW 1

library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))
apply(auta2012, 2, function(x) sum(is.na(x)))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(Licznik = n()) %>% 
  arrange(-Licznik)

## Odp: Z 2011, jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>% 
  summarise(Licznik = n()) %>% 
  arrange(-Licznik)

## Odp: Skoda (1288)


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
nrow(auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & 
           Rok.produkcji >=2005 & 
           Rok.produkcji <= 2011))
  

## Odp: Jest ich 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" 
         & Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(Sredni.koszt = mean(Cena.w.PLN)) %>% 
  arrange(-Sredni.koszt)

## Odp: Porsche (345669 PLN)


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(Sredni.koszt.modelu = mean(Cena.w.PLN)) %>% 
  arrange(Sredni.koszt.modelu)

## Odp: Fabia (42015)


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  mutate(Stosunek.cena.KM = Cena.w.PLN/KM) %>% 
  filter(is.na(auta2012$KM) == F & Liczba.drzwi == "2/3" & Stosunek.cena.KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(Licznik = n()) %>% 
  arrange(-Licznik)


## Odp: Automatyczna (708)


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
benzyna <- auta2012 %>% 
  filter(Marka == "Skoda" & (Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "benzyna+LPG")) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena.benzyna = mean(Cena.w.PLN))

diesel <- auta2012 %>% 
  filter(Marka == "Skoda" & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena.diesel = mean(Cena.w.PLN))

df <- merge(benzyna, diesel, by = "Model")
df %>% 
  mutate(Roznica.cen = abs(Srednia.cena.benzyna - Srednia.cena.diesel)) %>% 
  arrange(Roznica.cen)


## Odp: Favorit (131.57 PLN)


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
Lamborghini <- auta2012 %>% 
  filter(Marka == "Lamborghini") 

wektor.wyposarzenie <- as.character(Lamborghini[["Wyposazenie.dodatkowe"]])
wektor.wyposarzenie.splitted <- unlist(strsplit(wektor, ", "))
sort(table(wektor.wyposarzenie.splitted))

## Odp: Najczęściej: ABS, alufelgi, wspomaganie kierownicy (18)
##      Najrzadziej: blokada skrzyni biegów, klatka (1)


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
View(auta2012 %>% 
  filter(Marka =="Audi") %>% 
  group_by(Marka, Model) %>% 
  summarise())

# wszystkie Audi z grupy A, mają 'A' jako pierwszą literę modelu
# do grupy S i RS należą modele, które mają odpowiednio na początku 'S' i 'RS'
# oraz odpowiednio TT S i TT RS

auta2012 %>% 
  filter(Marka == "Audi" & grepl("(A|S|RS)", Model) & !is.na(KM)) %>% 
  mutate(Grupa.modeli = ifelse(substr(Model, 1, 2) == "RS" | Model == "TT RS","RS",
                               ifelse(substr(Model, 1, 1) == "A", "A", "S"))) %>%
  group_by(Grupa.modeli) %>% 
  summarise(srednia = mean(KM), mediana = median(KM)) %>% 
  mutate(roznica = srednia - mediana)

## Odp:
# Grupa.modeli srednia mediana roznica
# 1 A               160.     140  19.6  
# 2 RS              493.     450  43.3  
# 3 S               340.     340   0.477


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
marki <- as.character(
    unlist(
      auta2012 %>% 
      group_by(Marka) %>% 
      summarise(count = n()) %>% 
      filter(count > 10000) %>% 
      select(Marka)
      )
    )

modele <- auta2012 %>% 
  group_by(Marka, Model) %>% 
  summarise(count = n()) %>% 
  filter(Marka %in% marki & count == max(count)) %>% 
  select(!count)

modele.unlisted <- as.character(modele$Model)

auta2012 %>% 
  filter(Marka %in% marki & Model %in% modele.unlisted & Kolor != "") %>% 
  group_by(Marka, Kolor) %>% 
  summarise(count = n()) %>% 
  filter(count == max(count)) %>% 
  select(Marka, Kolor) %>% 
  merge(x = modele, y = ., by = "Marka")

## Odp: 
#           Marka  Model            Kolor
# 1          Audi     A4  czarny-metallic
# 2           BMW    320 srebrny-metallic
# 3          Ford  Focus srebrny-metallic
# 4 Mercedes-Benz  C 220 srebrny-metallic
# 5          Opel  Astra srebrny-metallic
# 6       Renault Megane srebrny-metallic
# 7    Volkswagen Passat srebrny-metallic