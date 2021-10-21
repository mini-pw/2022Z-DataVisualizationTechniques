library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>%
  select(Rok.produkcji, Model) %>% 
  group_by(Rok.produkcji) %>% 
  summarise(liczba = n()) %>% 
  arrange(-liczba) %>% 
  head(1)


## Odp: z rocznika 2011, jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut 
## ```wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji==2011) %>% 
  select(Marka, Model) %>% 
  group_by(Marka) %>% 
  summarise(liczba = n()) %>% 
  arrange(-liczba) %>% 
  head(1)
  


## Odp: Skoda (1288 razy)


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji>2004, Rok.produkcji<2012)%>% 
  select(Rodzaj.paliwa) %>% 
  group_by(Rodzaj.paliwa)%>% 
  summarise(n = n())


## Odp: olej napedowy (diesel) - 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, 
## ```która marka jest średnio najdroższa?

##
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>% 
  select(Marka, Cena.w.PLN) %>% 
  group_by(Marka) %>% 
  summarise(sredniaCenaPLN = mean(Cena.w.PLN)) %>% 
  arrange(-sredniaCenaPLN)
  

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>% 
  select(Marka, Cena) %>% 
  group_by(Marka) %>% 
  summarise(sredniaCena = mean(Cena)) %>% 
  arrange(-sredniaCena) 
  


## Odp: Zależy w jakiej walucie (nie ma przeliczników dla innych walut niż PLN).
##      Zakładam że chodzi o cenę w PLN (kolumna Cena.w.PLN). 
##      W takim wypadku: Porsche
##      Jeśli chodzi o po prostu kolumnę "Cena", to: Volvo


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, 
## ```który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda", Rok.produkcji==2011) %>% 
  select(Marka, Cena.w.PLN, Model) %>% 
  group_by(Model) %>% 
  summarise(sredniaCenaPLN = mean(Cena.w.PLN)) %>% 
  arrange(sredniaCenaPLN)

auta2012 %>% 
  filter(Marka == "Skoda", Rok.produkcji==2011) %>% 
  select(Marka, Cena, Model) %>% 
  group_by(Model) %>% 
  summarise(sredniaCena = mean(Cena)) %>% 
  arrange(sredniaCena)


## Odp: Znowu zakladam, że chodzi o Cena.w.PLN. Wtedy: Fabia
##      Ale jeśli chodzi o po prostu kolumnę "Cena", to: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  select(Liczba.drzwi, Cena.w.PLN, KM, Skrzynia.biegow) %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  mutate(StosunekCenyDoKM = Cena.w.PLN/KM) %>% 
  filter(StosunekCenyDoKM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  tally() %>% 
  arrange(-n)
  


## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę 
##    średnich cen między samochodami z silnikiem benzynowym, 
##    a diesel?
diesel <- 
  auta2012 %>% 
  filter(Marka=="Skoda") %>% 
  select(Cena.w.PLN, Model, Rodzaj.paliwa) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(sredniaCena = mean(Cena.w.PLN))


benzin <- 
  auta2012 %>% 
  filter(Marka=="Skoda") %>% 
  select(Cena.w.PLN, Model, Rodzaj.paliwa) %>% 
  filter(Rodzaj.paliwa == "benzyna") %>% 
  group_by(Model) %>% 
  summarise(sredniaCena = mean(Cena.w.PLN))

  
oba = inner_join(diesel, benzin, "Model")

oba %>% 
  mutate(roznica = abs(sredniaCena.x-sredniaCena.y)) %>% 
  arrange(roznica)



diesel2 <- 
  auta2012 %>% 
  filter(Marka=="Skoda") %>% 
  select(Cena, Model, Rodzaj.paliwa) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(sredniaCena = mean(Cena))


benzin2 <- 
  auta2012 %>% 
  filter(Marka=="Skoda") %>% 
  select(Cena, Model, Rodzaj.paliwa) %>% 
  filter(Rodzaj.paliwa == "benzyna") %>% 
  group_by(Model) %>% 
  summarise(sredniaCena = mean(Cena))


oba2 <-  inner_join(diesel2, benzin2, "Model")

oba2 %>% 
  mutate(roznica = abs(sredniaCena.x-sredniaCena.y)) %>% 
  arrange(roznica)
  


## Odp: Dla Cena.w.PLN: Felicia
##      Dla Cena:       Praktik 


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
auta2012 %>% 
  select(Marka, Wyposazenie.dodatkowe) %>% 
  filter(Marka == "Lamborghini") %>% 
  separate_rows(Wyposazenie.dodatkowe, sep = ", ") %>% 
  select(Wyposazenie.dodatkowe) %>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  tally() %>% 
  arrange(n)

auta2012 %>% 
  select(Marka, Wyposazenie.dodatkowe) %>% 
  filter(Marka == "Lamborghini") %>% 
  separate_rows(Wyposazenie.dodatkowe, sep = ", ") %>% 
  select(Wyposazenie.dodatkowe) %>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  tally() %>% 
  arrange(-n)


## Odp: najwiecej - ABS, alufelgi, wspomaganie kierownicy; 
##      najmmniej - blokada skrzyni biegów, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka =="Audi") %>% 
  select(Marka, Model, KM) %>% 
  separate(Model, c("ModelA", "ModelB")) %>% 
  select(ModelA, KM)%>% 
  filter(grepl("^A|^S|^RS", ModelA)) %>% 
  mutate(grupy = str_sub(ModelA, 1, nchar(ModelA)-1)) %>% 
  group_by(grupy) %>% 
  na.omit %>% 
  summarise(sredniaKM = mean(KM), medianaKM = median(KM))


auta2012 %>% 
  auta2012 %>% 
  filter(Marka =="Audi") %>% 
  select(Marka, Model, KM) %>% 
  separate(Model, c("ModelA", "ModelB")) %>% 
  select(ModelA, KM)%>% 
  filter(grepl("^A|^S|^RS", ModelA)) %>% 
  group_by(ModelA) %>% 
  tally() %>% 
  View()

## Odp:
#   grupy sredniaKM medianaKM
#   <chr>     <dbl>     <dbl>
#  1 A          160.       140
#  2 RS         500.       450
#  3 S          344.       344
  

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla 
##     każdej z tych marek.

colnames(auta2012)
wiecejNiz10000 <- 
  auta2012 %>% 
  select(Marka) %>% 
  group_by(Marka) %>% 
  tally() %>% 
  filter(n>10000) %>% 
  rename(IleAutTejMarki = n)


autaZWiecejniz10k <- inner_join(auta2012, wiecejNiz10000, "Marka")

wiecejNiz10kIPodzialnaModel <- 
  autaZWiecejniz10k %>% 
  select(Marka, Model) %>% 
  group_by(Marka, Model) %>% 
  tally() %>% 
  rename(IleDanegoModelu = n)

wiecejNiz10kIPodzialnaKolor <- 
  autaZWiecejniz10k %>% 
  select(Marka, Model, Kolor) %>% 
  group_by(Marka, Model, Kolor) %>% 
  tally() %>% 
  rename(IleDanegoKoloru = n)

temp <- inner_join(wiecejNiz10kIPodzialnaKolor, wiecejNiz10kIPodzialnaModel, c("Model", "Marka"))

temp2 <- 
  temp %>% 
  group_by(Marka) %>% 
  summarise(najwiekszyModelDlaMarki = max(IleDanegoModelu))

temp3 <- 
  inner_join(temp, temp2, x.IleDanegoModelu = y.NajwiekszyModelDlaMarki)

temp4 <- 
  temp3 %>% 
  select(Model, Marka, IleDanegoModelu, IleDanegoKoloru, Kolor, najwiekszyModelDlaMarki) %>% 
  filter(najwiekszyModelDlaMarki == IleDanegoModelu) %>% 
  select(Marka,Model,Kolor, IleDanegoKoloru) %>% 
  group_by(Marka, Model) %>% 
  summarise(najwiekszyKolorDlaModelu = max(IleDanegoKoloru))

temp5 <- 
  inner_join(temp3, temp4, x.IleDanegoKoloru = y.najwiekszyKolorDlaModelu)

temp5 %>% 
  select(Marka, Model, Kolor, najwiekszyKolorDlaModelu, IleDanegoKoloru) %>% 
  filter(najwiekszyKolorDlaModelu == IleDanegoKoloru) %>% 
  select(Marka, Model, Kolor)



## Odp: 

#   Marka         Model  Kolor 
# 1 Audi          A4     czarny-metallic 
# 2 BMW           320    srebrny-metallic
# 3 Ford          Focus  srebrny-metallic
# 4 Mercedes-Benz C 220  srebrny-metallic
# 5 Opel          Astra  srebrny-metallic
# 6 Renault       Megane srebrny-metallic
# 7 Volkswagen    Passat srebrny-metallic

