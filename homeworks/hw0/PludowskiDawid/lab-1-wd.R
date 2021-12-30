###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzacy.
# Hubert Baniecki/Anna Kozak
# Kontakt: MS Teams lub mail
# a.kozak@mini.pw.edu.pl

## 1) Materialy
# Repozytorium na GitHub
# https://github.com/mini-pw/2022Z-DataVisualizationTechniques 

## 2) Jak dziala GitHub?
# Jak zglosic prace domowa/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 3) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars["mpg"]
mtcars[1]
mtcars[1,]

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

head(mtcars, 10)[2:3]

# Wszytskie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?

mtcars[c("am", "wt", "mpg")]

# Jak wybierac jedna kolumne?

mtcars$mpg

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)

# 2. Jakie sa typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?

x <- unique(mtcars$cyl)
length(x)

# 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres

# Zaleznosc "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(table(mtcars$cyl)) 

## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

summary(employees)
employees[employees$name == "John" & employees$surname == "Insecure", ]

proton(action = "login", login = "johnins")

proton(action = "login", login="johnins", password="ABC")

for(i in 1:length(top1000passwords)) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

summary(logs)
d <- table(logs[logs$login == "slap", ]$host)
max(d)
d[d == 112]

employees[employees$surname == "Pietraszko", ]

proton(action = "server", host="194.29.178.16")

summary(bash_history)
head(bash_history)

d <- strsplit(bash_history, " ")
s <- sapply(d, length)
unique(d[s == 1])


proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")


## 5) Umieszczamy rozwiazanie na repozytorium.