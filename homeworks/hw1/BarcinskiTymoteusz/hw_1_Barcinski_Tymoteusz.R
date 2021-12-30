library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))
View(auta2012)

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

## Odp: 2011 - 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1) %>% 
  select(Marka)

## Odp: SKODA


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Rok.produkcji) %>% 
  summarise(count = n()) %>% 
  filter(Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>% 
  summarise(sum = sum(count))

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(mean = mean(Cena)) %>%
  arrange(-mean) %>% 
  head(1) %>% 
  select(Marka) %>% 
  View(.)

## Odp: Volvo


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(min = min(Cena)) %>% 
  arrange(min) %>% 
  head(1) %>% 
  select(Model) %>% 
  View(.)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3", Cena.w.PLN/KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1) %>% 
  select(Skrzynia.biegow) %>% 
  View(.)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

result <- auta2012 %>% 
  filter(Marka == "Skoda" & (Rodzaj.paliwa == "olej napedowy (diesel)" | Rodzaj.paliwa == "benzyna")) %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(mean = mean(Cena)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = mean)
colnames(result)[3] <- "Diesel"

result %>% 
  mutate(difference = abs(benzyna - Diesel)) %>% 
  arrange(difference) %>% 
  head(1) %>% 
  select(Model)

## Odp: Praktik

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

str_split(filter(auta2012, Marka == "Lamborghini")$Wyposazenie.dodatkowe,
          pattern = ", ", n = Inf) %>% 
  unlist() %>% 
  data.frame(Wyposazenie = .) %>% 
  group_by(Wyposazenie) %>% 
  summarise(count = n()) -> result
  
result %>% arrange(count) %>% head(5)
result %>% arrange(-count) %>% head(5)

  
## Odp: Najrzadziej:
#         - blokada skrzyni biegAlw
#         - klatka
#       Najczęściej:
#         - ABS
#         - alufelgi  
#         - wspomaganie kierownicy


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
result <- auta2012 %>% 
  filter(Marka == "Audi") %>%
  select(Model)  

auta2012 %>%
  filter(Marka == "Audi") %>% 
  select(KM, Model) %>% 
  mutate(Model_new = case_when(
    str_detect(result$Model, "RS") ~"RS",
    str_detect(result$Model, "A") ~ "A",
    str_detect(result$Model, "S") ~"S")) %>%
  filter(!is.na(Model_new)) %>% 
  filter(!is.na(KM)) %>% 
  group_by(Model_new) %>% 
  summarise(mean_KM = mean(KM), median_KM = median(KM)) %>% 
  View()

## Odp: Model | mean      | mediana
#         A   | 159.5799  | 140
#         RS  | 493.2703  | 450
#         S   | 340.4772  | 340

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  filter(count > 10000) -> vector_Marka

for (marka in vector_Marka$Marka){
  
  auta2012 %>% 
    filter(Marka == marka) %>% 
    group_by(Model) %>% 
    summarise(count = n()) %>%
    arrange(-count) -> result
  
  print(marka)
  print(result$Model[[1]])
  
  auta2012 %>% 
    filter(Marka == marka, Model == result$Model[1]) %>%
    group_by(Kolor) %>% 
    summarise(count = n()) %>% 
    arrange(-count) %>% 
    head(1)[1] %>% 
    print()
}

## Odp: Audi A4 - czarny-metallic
#       BWM 320, Ford Focus, Mercedes-Benz C 220, Opel Astra, Renault Megane,
#       Volkswagen Passat - srebrny-metallic 