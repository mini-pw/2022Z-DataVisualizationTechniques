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

## 3) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars
mtcars
data(mtcars)
head(mtcars, 10)
tail(mtcars, 10)
dim(mtcars)
str(mtcars)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[2:3,4:5]
# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
head(mtcars[,2:3], 10)

mtcars[, c("mpg", "cyl")]
mtcars[, c("cyl", "mpg")]


# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$mpg


# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
# 2. Jakie są typy zmiennych?
str(mtcars)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mtcars$cyl == 4
mean(mtcars[mtcars$cyl == 4, c("drat")])
median(mtcars[mtcars$cyl == 4, c("drat")])

mtcars$am
table(mtcars$am)

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




## Etap 1

employees[employees$surname == "Insecure", ]

proton(action = "login", login = "johnins")

## Etap 2

top1000passwords #wektor

for (pass in top1000passwords) {
  response <- proton(action = "login", login="johnins", password= pass)
  if (response == 'Success! User is logged in!'){
    cat(pass)
  }
}

## Etap 3

employees[employees$surname == "Pietraszko",]

table(logs[logs$login == "slap", c("host") ]) -> tmp
data.frame(tmp)

proton(action = "server", host = "194.29.178.16")


## Etap 4

bash_history #lista

split_bash_history <- strsplit(bash_history, " ") # rozdziela napisy względem podanego znaku, tutaj względem spacji " "

comands <-  c()

for (x in split_bash_history){ # wybieramy pierwszy napis po podzieleniu komendy z bash_history
  comands <- c(comands, x[[1]])
}

for (comand in unique(comands)){ # wykonujemy w pętli logowanie używając po kolei wartości comands 
  proton(action = "login", login = "slap", password = comand)
  
}

## 5) Umieszczamy rozwiązanie na repozytorium.