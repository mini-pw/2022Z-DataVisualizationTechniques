###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzący.
# Hubert Baniecki/Anna Kozak
# Kontakt: MS Teams lub mail
# a.kozak@mini.pw.edu.pl

## 1) Materiały
# Repozytorium na GitHub
# https://github.com/mini-pw/2022Z-DataVisualizationTechniques 

## 2) Jak działa GitHub?
# Jak zgłosić pracę domową/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 3) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)
tail(mtcars)
str(mtcars)
D <- mtcars
?mtcars
class(D)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
D[,'mpg']
D[["mpg"]]
# Pierwszy wiersz, pierwsza kolumna?
D[1,1]
D[4,6]
# 10 pierszych wierszy, 2 i 3 kolumna?
head(D[c(2,3)],10)
D[1:10,2:3]
D[1:10, c(2,5)]
# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
D[,c('am','mpg','wt')]
# Jak wybierać jedną kolumnę?

# Pytania

# 1. Wymiar ramki danych
dim(D)
# 2. Jakie są typy zmiennych?
str(D)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unikalne <- unique(D[,'cyl'])
length(unikalne)
table(D$cyl, D$gear, D$drat)
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(D[D$cyl == 4, 'drat'])

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot


plot(D$mpg, D$hp)
# Zmienna "cyl" - barplot
barplot(D$cyl)
barplot(table(D$cyl))
## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
login <- employees[employees$surname == 'Insecure', 3]
proton(action = "login", login = "johnins")
i = 1
czy_poprawne <- 1:1000
for (i in 1:1000){
  czy_poprawne[i] = proton(action = "login", login=login, password=top1000passwords[i])
}
login_pietraszko <- employees[employees$surname == 'Pietraszko', 3]
logs
logs_pietr <- logs[logs$login == login_pietraszko,]
logs_pietr
tabela <- (table(logs_pietr$host))
maks <-which.max(table(logs_pietr$host))
tabela[maks]
proton(action = 'server',host="194.29.178.16")
library(stringr)
komendy <- stringr::str_split(bash_history, " ", simplify = T) 
komendy1 <- komendy[,1]
for(i in 1:length((komendy1))){
  proton(action = "login", login = login_pietraszko, password = komendy1[i])
}
## 5) Umieszczamy rozwiązanie na repozytorium.