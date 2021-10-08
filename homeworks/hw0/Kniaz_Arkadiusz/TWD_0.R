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

# 10 pierszych wierszy, 2 i 3 kolumna?

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?

# Jak wybierać jedną kolumnę?

# Pytania

# 1. Wymiar ramki danych

# 2. Jakie są typy zmiennych?

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat[mtcars$cyl==4])

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg,mtcars$hp)
barplot(table(mtcars$cyl))

# Zmienna "cyl" - barplot


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()


## 5) Umieszczamy rozwiązanie na repozytorium.