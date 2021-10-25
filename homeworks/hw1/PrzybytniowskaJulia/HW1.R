library(PogromcyDanych)
library(dplyr)
library(stringr)
# colnames(auta2012)
# dim(auta2012)
# head(auta2012[,-ncol(auta2012)])
# sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n = n()) %>% 
  top_n(1, n)

## Odp: 
#Z rocznika 2011, 17418 sztuk.

## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  top_n(1, count) %>% 
  select(Marka)

## Odp:
#Skoda

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji >= 2005, Rok.produkcji <= 2011, Rodzaj.paliwa =="olej napedowy (diesel)") %>%
  summarise(count = n())

## Odp:
#59634

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji ==2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(mean = mean(Cena.w.PLN)) %>% 
  top_n(1, mean)

## Odp:
# Porshe

## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji ==2011 & Marka=="Skoda") %>% 
  group_by(Model) %>% 
  summarise(mean = mean(Cena.w.PLN)) %>% 
  top_n(1, -mean)

## Odp:
# Fabia

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi=="2/3" & Cena.w.PLN/KM > 600) %>%
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n()) %>% 
  top_n(1, count)

## Odp: 
# Automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

benzyna <- auta2012 %>% 
  filter(Rodzaj.paliwa == "benzyna" & Marka == "Skoda") %>% 
  group_by(Model) %>% 
  summarise(mean_benzyna = mean(Cena.w.PLN))

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Marka == "Skoda") %>% 
  group_by(Model) %>% 
  summarise(mean_disel = mean(Cena.w.PLN)) %>% 
  inner_join(benzyna, by.x = Model, by.y = Model) %>% 
  group_by(Model) %>% 
  summarise(difference = abs(mean_disel - mean_benzyna)) %>% 
  top_n(1, -difference)

## Odp: 
# Felicia

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
help1 <- auta2012 %>%
  filter(Marka =="Lamborghini")

help2 <- str_split_fixed(help1$Wyposazenie.dodatkowe, ", ", n= Inf) %>% 
  as.vector() %>% 
  data.frame() %>% 
  select(group = ".") %>% 
  group_by(group) %>% 
  summarise(count = n()) %>%  
  filter(group != "")
  
help2 %>% 
  top_n(2, count)

help2 %>% 
  top_n(1, -count)


## Odp: 
# Najczesciej: ABS, alufelgi, kierownicy
# Najrzadziej: blokada skrzyni biegow, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

pomoc <- filter(auta2012, Marka =="Audi") 
pomoc2<- str_split_fixed(pomoc$Model, "", n = 3)

final <- cbind(pomoc, pomoc2) %>%  
  select(c("KM", model_1 = "1", model_2 = "2") ) %>% 
  filter(model_1 == "A" | model_1 == "S" | (model_1 == "R" & model_2 == "S")) %>% 
  group_by(Model = model_1) %>% 
  summarise(Mean = mean(KM,  na.rm = TRUE), Median = median(KM, na.rm = TRUE))

final[1] <- c("A", "RS", "S")
# final

## Odp:
# 1 A      160.    140
# 2 RS     500.    450
# 3 S      344.    344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  filter(count>10000) %>% 
  left_join(auta2012, by = "Marka") %>% 
  group_by(Marka, Model) %>% 
  summarise(n = n()) %>% 
  group_by(Marka) %>% 
  top_n(1,n) %>% 
  left_join(auta2012, by = c("Marka","Model")) %>% 
  group_by(Marka,Model,Kolor) %>% 
  summarise(count = n()) %>% 
  top_n(1,count)

## Odp: 
# 1 Audi          A4     czarny-metallic    853
# 2 BMW           320    srebrny-metallic   441
# 3 Ford          Focus  srebrny-metallic  1711
# 4 Mercedes-Benz C 220  srebrny-metallic   205
# 5 Opel          Astra  srebrny-metallic  1539
# 6 Renault       Megane srebrny-metallic   733
# 7 Volkswagen    Passat srebrny-metallic  1466