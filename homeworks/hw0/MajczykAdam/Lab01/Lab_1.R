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
mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
head(mtcars[, 2:3], 10)

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$hp



# Alt + -  <-
# Ctrl + Shitf + m %>%

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)


# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat)
mean(mtcars$drat[mtcars$cyl == 4])
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


## 5) Umieszczamy rozwiązanie na repozytorium.

install.packages("styler")
head(employees)
employees[employees$name == "John", ]
employees[employees$surname == "Pietraszko", ]
proton(action = "login", login = "johnins")

top1000passwords


for (pass in top1000passwords) {
  string <- proton(action = "login", login = "johnins", password = pass)
  if (string == "Success! User is logged in!") {
    print("--------------------------------------")
    cat(pass)
  }
}


head(logs)

temp <- logs[logs$login == "slap", ]
temp
temp <- temp[, c("host")]
temp


View(data.frame(table(temp)))

proton(action = "server", host = "194.29.178.16")


head(bash_history)

a <- c()
for (com in bash_history) {
  temp <- strsplit(com, split = " ")
  print(temp[[1]][[1]])
  a <- c(a, temp[[1]][[1]])
}

table(a)
proton(action = "login", login = "slap", password = "DHbb7QXppuHnaXGN")
