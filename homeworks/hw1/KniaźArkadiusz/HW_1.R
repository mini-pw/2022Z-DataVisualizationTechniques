library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyselect)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?


## Odp: 
Odp1<-auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(N_rok=n()) %>% 
  top_n(1,N_rok)
# 2011 17418
## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?


## Odp:
Odp2<-auta2012 %>% 
  select(Marka,Rok.produkcji) %>% 
  filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>% 
  summarise(N_marka=n()) %>%
  top_n(1,N_marka)
#skoda
## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?


## Odp:
Odp3<-auta2012 %>% 
  select(Rodzaj.paliwa,Rok.produkcji) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji>= 2005 & Rok.produkcji<= 2011) %>% 
  summarise(N_diesel=n()) 
#59534
## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?


## Odp:
# przez kolumne "Brutto.netto" pytanie jest nie doprecyzowane o tyle że nie każdy musi się na podatkach 
##i ile podatku doliczyć do ceny netto, ja np się nie znam i wziąłem 23% 
Odp4<-auta2012 %>% 
  select(Cena.w.PLN,Brutto.netto,Marka,Rodzaj.paliwa,Rok.produkcji) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2011) %>% 
  mutate(cena_por = ifelse(Brutto.netto == "brutto", Cena.w.PLN, Cena.w.PLN*1.23 )) %>% 
  group_by(Marka) %>%
  summarise(cena_sr = mean(cena_por, na.rm = TRUE)) %>% 
  top_n(1,cena_sr)
#porsche
## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?


## Odp:
Odp5<-auta2012 %>% 
  select(Cena.w.PLN,Brutto.netto,Marka,Model,Rodzaj.paliwa,Rok.produkcji) %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>% 
  mutate(cena_por = ifelse(Brutto.netto == "brutto", Cena.w.PLN, Cena.w.PLN*1.23 )) %>% 
  group_by(Model) %>%
  summarise(cena_sr = mean(cena_por, na.rm = TRUE)) %>%   
  top_n(-1,cena_sr)
#Fabia

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?


## Odp: 
Odp6<-auta2012 %>% 
  select(Cena.w.PLN,KM,Skrzynia.biegow,Liczba.drzwi) %>% 
  mutate(Ratio = Cena.w.PLN/KM) %>%
  filter(Liczba.drzwi == "2/3",Ratio>600) %>%
  group_by(Skrzynia.biegow) %>% 
  summarise(N_skrzynia=n()) %>%
  top_n(1,N_skrzynia)
#automatyczna
## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?


## Odp: 
Odp7<-auta2012 %>% 
  select(Cena.w.PLN,Brutto.netto,Marka,Model,Skrzynia.biegow,Rodzaj.paliwa) %>% 
  filter(Marka == "Skoda"&(Rodzaj.paliwa=="olej napedowy (diesel)" | Rodzaj.paliwa=="benzyna")) %>%
  mutate(cena_por = ifelse(Brutto.netto == "brutto", Cena.w.PLN, Cena.w.PLN*1.23 )) %>%
  mutate(paliwo = ifelse(Rodzaj.paliwa=="benzyna", "benzyna", "diesel")) %>% 
  group_by(Model,paliwo) %>%
  summarise(cena_sr = mean(cena_por, na.rm = TRUE)) %>% 
  spread(paliwo,cena_sr) %>% 
  na.omit() %>% 
  mutate(dif = abs(diesel-benzyna)) %>% 
  ungroup() %>% 
  top_n(-1,dif)
#felicia
## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

  

## Odp: 
Odp8<-auta2012 %>% 
  select(Marka,Wyposazenie.dodatkowe) %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe)  
Odp8<-unlist(str_split(Odp8$Wyposazenie.dodatkowe,", "))
Odp8<-data.frame(as.list(Odp8))
Odp8<-Odp8 %>% 
  gather(key = "w",value="wyp") %>% 
  select(wyp) %>% 
  group_by(wyp) %>%
  summarise(N_wyp = n())
Odp8t<-top_n(Odp8,3,N_wyp)
Odp8b<-top_n(Odp8,-2,N_wyp)

#najrzadziej  blokada skrzyni biegAlw, klatka
#i najczęściej ABS, 	alufelgi, wspomaganie kierownicy

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi


## Odp:
Odp9<-auta2012 %>% 
  select(kW,Marka,Model) %>% 
  filter(Marka == "Audi") %>% 
  mutate( typ = ifelse(grepl('^A', Model), "A",
                ifelse(grepl('^S', Model), "S",
                ifelse(grepl('^RS', Model), "RS", "Other")))) %>% 
  filter(typ != "Other") %>%
  group_by(typ) %>%
  summarise(moc_sr = mean(kW, na.rm = TRUE),moc_M=median(kW,na.rm = TRUE))


View(Odp9) 
#Modele średnia    mediana
#A      117.3447     103
#S      367.9296     331
#RS     252.7845     253
## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.


## Odp:
marki<-auta2012 %>%
  select(Marka,Model,Kolor) %>% 
  group_by(Marka) %>% 
  mutate(N_Marka=n()) %>%
  filter(N_Marka > 10000) %>% 
  ungroup() 


modele<-marki %>% 
  group_by(Marka,Model) %>% 
  mutate(N_Model=n()) %>%
  ungroup() 


naj_modele<-modele %>% 
  group_by(Marka) %>%
  summarise(naj_N_Model=max(N_Model))


kolory<-inner_join(modele,naj_modele,by = c("Marka" = "Marka", "N_Model" = "naj_N_Model")) %>%
  group_by(Marka,Model,Kolor) %>%
  mutate(N_Kolor=n()) %>%
  ungroup()
  
naj_kolory<-kolory %>% 
  group_by(Marka,Model) %>%
  summarise(naj_N_Kolor=max(N_Kolor))

wynik<-inner_join(kolory,naj_kolory,by = c("Marka" = "Marka", 
                                           "Model" = "Model",
                                           "N_Kolor" = "naj_N_Kolor")) %>% 
  distinct()

#marka          model    kolor
#Ford           Focus    srebrny-metallic
#Volkswagen     Passat   srebrny-metallic
#Opel           Astra    srebrny-metallic
#BMW            320      srebrny-metallic
#Renault        Megane   srebrny-metallic
#Mercedes-Benz  C 220    srebrny-metallic
#Audi           A4       czarny-metallic
#
#Arkadiusz Kniaź 305713

