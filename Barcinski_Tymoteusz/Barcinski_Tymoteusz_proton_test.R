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
mtcars
tail(mtcars, 10)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[1, ]
mtcars[, 1]
mtcars[1, 1]

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
mtcars[1:10, c(2,4)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$am

# Pytania

# 1. Wymiar ramki danych
dim(mtcars[1:10, ])

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
table(mtcars$cyl) ## jest to przydatna funkcja i warto ja ogarniac

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "dart"])

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

## etap 1
employees[employees$name == "John" & employees$surname == "Insecure", ] ## do rozkminienia jak podaje sie dwie wartosci
x <- vector(employees[employees$surname == "Insecure", ][3])
view("employees")
proton(action = "login", login = "johnins")

## etap 2
head(top1000passwords)

for(i in top1000passwords){
  proton(action = "login", login = "johnins", password=i)
}

## etap 3
employees[employees$surname == "Pietraszko", ]
table(logs[logs$login == "slap", c("host")]) -> x
count <- data.frame(x)
count[which.max(count$Freq), ]



proton(action = "server", host="194.29.178.16")

## etap 4





## 5) Umieszczamy rozwiązanie na repozytorium.