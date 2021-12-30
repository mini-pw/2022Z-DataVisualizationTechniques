library(PogromcyDanych)
library(dplyr)
library(stringr)


colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))


## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>%
  group_by(Rok.produkcji) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% head(1)

## Odp: rok 2011, 17418 aut


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>%
  filter(Rok.produkcji == 2011) %>% select(Marka) %>%
  group_by(Marka) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% head(1)

## Odp: Skoda 1288 aut


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>%
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(count = n())

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)" & Marka != "") %>% 
  group_by(Marka) %>% summarise(mean_price = mean(Cena.w.PLN)) %>% 
  arrange(desc(mean_price)) %>% head(1)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011 & Marka == "Skoda" & Model != "") %>% 
  group_by(Model) %>% summarise(mean_price = mean(Cena.w.PLN)) %>% 
  arrange(mean_price) %>% head(1)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  mutate(pln_per_hp = (Cena.w.PLN) / (KM)) %>% 
  filter( pln_per_hp > 600 & Liczba.drzwi == "2/3" & Skrzynia.biegow != "") %>%
  group_by(Skrzynia.biegow) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% head(1)

## Odp: automatyczna 708


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

# Podczas wyszukiwania samochodów z silnikiem benzynowy uwzględniłem takżę ropdzaj paliwa
# benzyna+LPG, bowiem to oznacza, że silnik w takim samochodzie też jest benzynowy.
skoda_gasoline <- auta2012 %>% 
  filter(Marka == "Skoda" & ( Rodzaj.paliwa == ("benzyna") | Rodzaj.paliwa ==  ("benzyna+LPG") )) %>%
  group_by(Model) %>% summarise(mean_price_gasoline = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(Model)

skoda_diesel <- auta2012 %>% 
  filter(Marka == "Skoda" & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% summarise(mean_price_diesel = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(Model)

joined <- inner_join(skoda_gasoline, skoda_diesel, by = c("Model" = "Model"))

final_table <- joined %>%
  group_by(Model) %>% 
  summarise(difference_in_price = abs(mean_price_diesel - mean_price_gasoline) ) %>% 
  arrange(difference_in_price) %>% head(1)

## Odp: Favorit 132


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
car_equipment <- auta2012 %>% 
  filter(Marka == "Lamborghini" & Wyposazenie.dodatkowe != "") %>%
  select(Wyposazenie.dodatkowe)

car_equipment$Wyposazenie.dodatkowe <- as.character(car_equipment$Wyposazenie.dodatkowe)
car_equipment <- as.character(car_equipment[,1])
car_equipment <- str_split(car_equipment,  ", ")
car_equipment <- as.data.frame(table(unlist(car_equipment)))
car_equipment <- car_equipment %>% arrange(desc(Freq))

## Odp:
      # Najczęściej występujące wyposażenie:
      # * ABS
      # * alufelgi
      # * wspomaganie kierownicy

      # Najżadziej występujące wyposażenie:
      # * blokada skrzyni biegów
      # * klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
model_A <- auta2012 %>% 
  filter(Marka == "Audi") %>%
  filter(str_detect(Model, "^A")) %>%
  mutate(mean = mean(KM, na.rm=TRUE)) %>% mutate(median = median(KM, na.rm=TRUE))

model_S <- auta2012 %>% 
  filter(Marka == "Audi") %>%
  filter(str_detect(Model, "^S")) %>%
  mutate(mean = mean(KM, na.rm=TRUE)) %>% mutate(median = median(KM, na.rm=TRUE))

model_RS <- auta2012 %>% 
  filter(Marka == "Audi") %>%
  filter(str_detect(Model, "^RS")) %>%
  mutate(mean = mean(KM, na.rm=TRUE)) %>% mutate(median = median(KM, na.rm=TRUE))

## Odp: Grupy modeli:
#       *  A: median = 140 HP, mean = 159.5799 HP
#       *  S: median = 344 HP, mean = 343.7371 HP
#       * RS: median = 450 HP, mean = 500.0282 HP


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
car_brands <- auta2012 %>% 
  group_by(Marka) %>% summarise(count = n()) %>% 
  filter(count > 10000) %>% arrange(count) %>% 
  select(Marka)

car_brands$Marka <- as.character(car_brands$Marka)
class(car_brands) <- c("data.frame")
as.character(car_brands[,1]) -> car_brands_vector


car_models <- auta2012 %>% 
  filter(Marka %in% car_brands_vector) %>% 
  group_by(Marka, Model) %>% 
  mutate(count = n()) %>% group_by(Marka) 


car_models_top <- NULL
temp <- NULL

for(i in car_brands_vector){
  temp <- car_models %>% filter(Marka == i & Kolor != "", count == max(count)) %>% 
    select(Marka, Model, Kolor)
  
  temp <- temp %>% group_by(Kolor) %>% 
    mutate(count = n()) %>%
    arrange(desc(count)) %>% head(1)
  
  car_models_top <- rbind(car_models_top, temp)
  
}

## Odp: (ramka danych car_models_top)

# Marka         Model  Kolor            
# BMW           320    srebrny-metallic
# Mercedes-Benz C 220  srebrny-metallic
# Audi          A4     czarny-metallic 
# Renault       Megane srebrny-metallic
# Ford          Focus  srebrny-metallic
# Opel          Astra  srebrny-metallic
# Volkswagen    Passat srebrny-metallic
