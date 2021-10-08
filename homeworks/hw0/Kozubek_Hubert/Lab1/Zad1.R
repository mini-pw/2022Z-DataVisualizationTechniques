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

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[2:3, 4:5]
# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
head(mtcars[,2:3], 10)
# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am","wt","mpg")]
# Jak wybierać jedną kolumnę?
mtcars$hp

# Alt + -   <-
# Crtl + Shift + m   %>% 

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
# 2. Jakie są typy zmiennych?
str(mtcars)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
table(mtcars$cyl)
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat)
mean(mtcars$drat[mtcars$cyl==4])
mean(mtcars[mtcars$cyl==4,"drat"])
# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(table(mtcars$cyl))

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees[employees$name == "John" & employees$surname=="Insecure", "login"]
proton(action = "login", login = "johnins")
for (pass in top1000passwords) {
  proton(action = "login", login = "johnins", password = pass)
}


employees[employees$surname=="Pietraszko", "login"]
newdata<- table(logs[logs$login == "slap","host"])
newdata
dim(newdata)
newdata[order(newdata[])]

proton(action = "server", host = "194.29.178.16")

splited <- strsplit(bash_history, " ")

for (p in splited) {
  if(length(p) == 1)
  {
    print(p)
  }
}


password <- "DHbb7QXppuHnaXGN"

proton(action = "login", login = "slap", password = "DHbb7QXppuHnaXGN")

## 5) Umieszczamy rozwiązanie na repozytorium.