library(PogromcyDanych)
library(dplyr)
install.packages("dplyr")
install.packages("stringr")
library(stringr)
install.packages("tidyr")
library(tidyr)
colnames(auta2012)

dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

install.packages("PogromcyDanych")
auta2012

## 1. Z którego rocznika jest najwiêcej aut i ile ich jest?

auta2012 %>%
  group_by(Rok.produkcji)%>%
  summarise(count = n())%>%
  arrange(-count)%>% 
  head(1)

## Odp: Najwiêcej aut jest z rocznika 2011 i jest ich 17418

## 2. Która marka samochodu wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku?

auta2012 %>%
  filter(Rok.produkcji == 2011)%>%
  group_by(Marka)%>%
  summarise(count = n())%>%
  arrange(-count)%>%
  head(1)

## Odp: Najczêœciej wystêpuje mark Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>%
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011 & Rodzaj.paliwa == "olej napedowy (diesel)")%>%
  nrow

## Odp: Jest ich 59534



## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest œrednio najdro¿sza?
auta2012 %>%
  filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)")%>%
  group_by(Marka)%>%
  summarise(srednia_cena = mean(Cena))%>%
  slice_max(srednia_cena)

## Odp: Marka Volvo


## 5. Spoœród aut marki Skoda wyprodukowanych w 2011 roku, który model jest œrednio najtañszy?

auta2012 %>%
  filter(Rok.produkcji == 2011 & Marka == "Skoda")%>%
  group_by(Model)%>%
  summarise(srednia_cena = mean(Cena))%>%
  arrange(srednia_cena)%>%
  head(1)

## Odp:Skoda Fabia


## 6. Która skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  filter((Cena.w.PLN /KM) > 600 & Liczba.drzwi == "2/3") %>%
  group_by(Skrzynia.biegow)%>%
  summarise(count = n())%>%
  slice_max(count)
  
## Odp: Najczêœciej wystêpuje skrzynia automatyczna


## 7. Spoœród aut marki Skoda, który model ma najmniejsz¹ ró¿nicê œrednich cen 
##    miêdzy samochodami z silnikiem benzynowym, a diesel?

auta2012 %>% 
  filter(Marka == "Skoda" & Rodzaj.paliwa =="olej napedowy (diesel)"| Rodzaj.paliwa == "benzyna")%>%
  group_by(Model,Rodzaj.paliwa) %>%
  summarise(srednia_cena = mean(Cena)) %>%
  mutate(roznica = abs(srednia_cena-lead(srednia_cena))) %>%
  ungroup %>%
  slice_min(roznica)

## Odp: Practik

## 8. ZnajdŸ najrzadziej i najczêœciej wystêpuj¹ce wyposa¿enie/a dodatkowe 
##    samochodów marki Lamborghini ??


auta2012 %>%
  filter(Marka =="Lamborghini")%>%
  select(Wyposazenie.dodatkowe)%>%
  mutate(Wyposazenie.dodatkowe = str_split(Wyposazenie.dodatkowe, ",")) %>% 
  unnest()%>%
  group_by(Wyposazenie.dodatkowe) %>%
  summarise(liczba = n())%>%
  arrange(liczba)%>%
  head()
  #arrange(-liczba)%>%
  #head()
 

## Odp: Najczesciej wystepuje abs, wpsomaganie keirownicy, alufelgi a najrzadziej blokada skrzyni i klatka
## 9. Porównaj œredni¹ i medianê mocy KM miêdzy grupami modeli A, S i RS 
##    samochodów marki Audi


auta2012 %>%
  filter(Marka == "Audi")%>%
  mutate(new_col = str_extract( Model, "[A-Za-z]+"))%>%
  group_by(new_col)%>%
  select(new_col, KM, Model)%>%
  summarise(mediana = median(KM, na.rm=TRUE),
            srednia = mean(KM, na.rm=TRUE))%>%
  filter(new_col == "A" | new_col == "S"  | new_col == "RS" )%>%
  head()
  

## Odp: Dla grupy RS i A: mediana < œrednia, dla S mediana jest równa œrdniej 
#new_col mediana srednia
#1 A           140    160.
#2 RS          450    500.
#3 S           344    344.


## 10. ZnajdŸ marki, których auta wystêpuj¹ w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka¿dej z tych marek.


auta2012 %>%
  select(Marka, Kolor, Model)%>%
  group_by(Marka) %>%
  mutate(liczba_aut = n())%>%
  filter(liczba_aut > 10000 & !(Marka == "") & !(Model == ""))%>%
  group_by(Marka, Model)%>%
  mutate(liczba_modeli = n())%>%
  group_by(Marka)%>%
  top_n(liczba_modeli)%>%
  ungroup %>%
  group_by(Marka, Model, Kolor)%>%
  mutate(count = n())%>%
  ungroup %>%
  group_by(Marka, Model)%>%
  top_n(count)%>%
  distinct()

## Odp:1 Ford          srebrny-metallic Focus      
      #2 Volkswagen    srebrny-metallic Passat
      #3 Opel          srebrny-metallic Astra      
      #4 BMW           srebrny-metallic 320         
      #5 Renault       srebrny-metallic Megane      
      #6 Mercedes-Benz srebrny-metallic C 220       
      #7 Audi          czarny-metallic  A4  
