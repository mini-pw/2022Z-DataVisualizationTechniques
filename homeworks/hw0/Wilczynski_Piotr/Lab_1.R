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

mtcars["cyl"] # ramka danych
mtcars[,"cyl"] # wektor
mtcars[["cyl"]] # wektor
mtcars$cyl # wektor
mtcars[1,] # lista

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, c(2,3)]

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
table(mtcars$cyl, mtcars$gear)
table(mtcars$cyl, mtcars$gear, mtcars$drat)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4, "drat"])


# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(table(mtcars$cyl))

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()


## 5) Umieszczamy rozwiązanie na repozytorium.

# 1
johnsLogin <- employees[employees$name == "John" & employees$surname == "Insecure" , "login"]
proton(action = "login", login=johnsLogin)

# 2
for (val in top1000passwords)
{
  proton(action = "login", login = johnsLogin, password=val)
}

# 3
pietraszkosLogin <- employees[employees$surname == "Pietraszko", "login"]
pietraszkosLogs <- logs[logs$login == pietraszkosLogin, ]
sort(table(pietraszkosLogs$host))
proton(action = "server", host = "194.29.178.16")

# 4

noSpaces <- bash_history[!grepl("[' ']+", bash_history)]
unique(noSpaces)
proton(action = "login", login=pietraszkosLogin, password = "DHbb7QXppuHnaXGN")

