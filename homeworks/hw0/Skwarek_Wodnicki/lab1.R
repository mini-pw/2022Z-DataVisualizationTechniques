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
tail(mtcars, 2)
str(mtcars)

D <- mtcars

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars["cyl"]
mtcars[, "cyl"]
mtcars[["cyl"]]
mtcars$cyl
mtcars[1,]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars["cyl"]
mtcars[, "cyl"]

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
table(mtcars$cyl)
table(mtcars$cyl, mtcars$gear)
table(mtcars$cyl, mtcars$gear, mtcars$drat)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

for (x in top1000passwords) {
  proton(action = "login", login = "johnins", password=x)
}



## 5) Umieszczamy rozwiązanie na repozytorium.

## 1
employees[employees$name == "John",]
proton(action = 'login', login='johnins')

## 2
for (x in top1000passwords) {
  proton(action = "login", login = "johnins", password=x)
}

## 3
employees[employees$surname == "Pietraszko",]
unique(logs[logs$login=="slap", "host"])
sort(table(logs[logs$login=="slap", "host"]))
proton(action = "server", host="194.29.178.16")

## 4
library(stringr)
df <- strsplit(bash_history, " ", simplify = T)
comands <- df[,1]
# itp








