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

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,]
mtcars[,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, c(2, 3)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$am



# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
length(unique(mtcars$cyl))
table(mtcars$cyl)

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

head(employees)
employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login="johnins", hint=TRUE)

head(top1000passwords)
for (pass in top1000passwords) {
  if (proton(action = "login", login="johnins", password=pass) == "Success! User is logged in!") {
    print(pass)
    break
  }
}

proton(action = "login", login="johnins", password="q1w2e3r4t5")

head(logs)
employees[employees$surname == "Pietraszko",]
tail(sort(table(logs[logs$login == "slap", "host"])))
proton(action = "server", host="194.29.178.16")

library(stringi)
head(bash_history)

library(stringr)
df <- stringr::str_split(bash_history, " ", simplify = TRUE)
commands <- df[,1]
for (pass in unique(commands)) {
  proton(action = "login", login="slap", password=pass)
}

## 5) Umieszczamy rozwiązanie na repozytorium.