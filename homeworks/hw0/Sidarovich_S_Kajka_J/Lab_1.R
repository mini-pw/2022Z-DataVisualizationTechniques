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
mtcars[1, ]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, c(2, 3) ]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$cyl

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
table(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "drat"])


# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)



# Zmienna "cyl" - barplot
barplot(mtcars$cyl)

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

#ZADANIE 1
employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login="johnins")

# Zadanie 2
for (pass in top1000passwords) {
  if (proton(action = "login", login="johnins", password=pass) == "Success! User is logged in!") {
    print(pass)
  }
}

# Zadanie 3
login <- employees[employees$surname == "Pietraszko", "login"]
which.max(table(logs[logs$login == login, ]$host))
proton(action = "server", host="194.29.178.16")

# Zadanie 4
library(stringr)
commands <- stringr::str_split(bash_history, " ", simplify = T)[, 1]

for (command in unique(commands)) {
  proton(action = "login", login = "slap", password = command)
}

## 5) Umieszczamy rozwiązanie na repozytorium.