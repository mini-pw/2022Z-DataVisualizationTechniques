library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z kt贸rego rocznika jest najwi臋cej aut i ile ich jest?

auta2012 %>% group_by(Rok.produkcji) %>% summarise(Ilosc = n()) %>% 
  arrange(-Ilosc) %>% head(1)

## Odp: 2011, 17418


## 2. Kt贸ra marka samochodu wyst臋puje najcz臋艣ciej w艣r贸d aut wyprodukowanych w 2011 roku?

auta2012 %>% filter(Rok.produkcji == 2011) %>% group_by(Marka) %>% 
  summarise(Ilosc = n()) %>% arrange(-Ilosc) %>% head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% filter(Rok.produkcji>=2005,Rok.produkcji<=2011, 
                    Rodzaj.paliwa == "olej napedowy (diesel)") %>% count()

## Odp: 59534


## 4. Spo艣r贸d aut z silnikiem diesla wyprodukowanych w 2011 roku, kt贸ra marka jest 艣rednio najdro偶sza?

auta2012 %>% filter(Rok.produkcji == 2011, 
                    Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% summarise(Srednia_cena = mean(Cena.w.PLN, na.rm = T)) %>% 
  arrange(-Srednia_cena) %>% head(1)

## Odp: Porsche


## 5. Spo艣r贸d aut marki Skoda wyprodukowanych w 2011 roku, kt贸ry model jest 艣rednio najta艅szy?

auta2012 %>% filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  group_by(Model) %>% summarise(Srednia_cena = mean(Cena.w.PLN, na.rm = T)) %>% 
  arrange(Srednia_cena) %>% head(1)

## Odp: Fabia


## 6. Kt贸ra skrzynia bieg贸w wyst臋puje najcz臋艣ciej w艣r贸d 2/3-drzwiowych aut,
##    kt贸rych stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% filter(Liczba.drzwi == "2/3", Cena.w.PLN/KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% summarise(Ilosc = n()) %>% arrange(-Ilosc) %>% 
  head(1)

## Odp: automatyczna


## 7. Spo艣r贸d aut marki Skoda, kt贸ry model ma najmniejsz膮 r贸偶nic臋 艣rednich cen 
##    mi臋dzy samochodami z silnikiem benzynowym, a diesel?

auta2012df <- auta2012

auta2012df$Rodzaj.paliwa[auta2012df$Rodzaj.paliwa == "benzyna+LPG"] <- "benzyna"

auta2012df %>% filter(Marka == "Skoda") %>% 
  group_by(Model,Rodzaj.paliwa) %>% 
  summarise(Srednia_cena = mean(Cena.w.PLN, na.rm = T)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from =  Srednia_cena) %>%
  select(benzyna, `olej napedowy (diesel)`) %>% 
  na.omit() %>% 
  mutate(Roznica =  abs(benzyna -`olej napedowy (diesel)`)) %>% 
  arrange(Roznica) %>% head(1)

## Odp: Favorit


## 8. Znajd藕 najrzadziej i najcz臋艣ciej wyst臋puj膮ce wyposa偶enie/a dodatkowe 
##    samochod贸w marki Lamborghini

auta2012 %>% filter(Marka == "Lamborghini") %>% select(Wyposazenie.dodatkowe) %>% 
   unlist() %>% str_split(", ") %>% unlist() %>% table() %>% sort()

  

## Odp: najrzadziej wyst阷uj筩e (1): blokada skrzyni bieg體, klatka 
#       najcz隃ciej wyst阷uj筩e (18): ABS, alufelgi, wspomaganie kierownicy


## 9. Por贸wnaj 艣redni膮 i median臋 mocy KM mi臋dzy grupami modeli A, S i RS 
##    samochod贸w marki Audi


auta2012 %>% filter(Marka == "Audi") %>% 
  mutate(Grupa_Modeli = case_when(str_starts(Model, "A") ~ "A",
                                  str_starts(Model, "S") ~ "S",
                                  str_starts(Model, "RS") ~ "RS",
                                  str_detect(Model, " A") ~ "A",
                                  str_detect(Model, " S") ~ "S",
                                  str_detect(Model, " RS") ~ "RS")) %>% 
  group_by(Grupa_Modeli) %>% summarise(Srednia_mocy = mean(KM,na.rm = T),
                                       Mediana_mocy = median(KM, na.rm = T)) %>% 
  na.omit()

## Odp:
# A  Srednia moc: 160 Mediana mocy: 140
# RS Srednia moc: 493 Mediana mocy: 450
# S  Srednia moc: 340 Mediana mocy: 340


## 10. Znajd藕 marki, kt贸rych auta wyst臋puj膮 w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka偶dej z tych marek.

auta2012 %>% select(Marka,Model,Kolor) %>% group_by(Marka) %>% 
  mutate(Marka_ilosc = n()) %>% filter(Marka_ilosc > 10000) %>%
  group_by(Marka,Model) %>% mutate(Model_ilosc=n()) %>% group_by(Marka) %>% 
  filter(Model_ilosc == max(Model_ilosc)) %>% group_by(Marka,Model,Kolor) %>% 
  mutate(Kolor_ilosc = n()) %>% group_by(Marka,Model) %>% 
  filter(Kolor_ilosc == max(Kolor_ilosc)) %>% unique() 


## Odp:   Marka         Model  Kolor           
#         -------------------------------------
#         Ford          Focus  srebrny-metallic
#         Volkswagen    Passat srebrny-metallic
#         Opel          Astra  srebrny-metallic
#         BMW           320    srebrny-metallic
#         Renault       Megane srebrny-metallic
#         Mercedes-Benz C 220  srebrny-metallic
#         Audi          A4     czarny-metallic 
