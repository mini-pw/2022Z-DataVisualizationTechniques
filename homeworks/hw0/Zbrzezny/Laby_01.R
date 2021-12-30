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
mtcars$mpg
# Pierwszy wiersz, pierwsza kolumna?
mtcars[1, 1]
# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
# Wszytskie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]
# Jak wybierac jedna kolumne?

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
# 2. Jakie sa typy zmiennych?

# 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?

# 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres

# Zaleznosc "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
barplot(mtcars$mpg, mtcars$hp)


table(mtcars$cyl)
barplot(table(mtcars$cyl))
## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()


data("employees")
imionka <- employees[employees$name == "John", ]
login <- employees[employees$surname == "Pietraszko", ]
login[, 3]


proton(action = "login", login="johnins")


data("top1000passwords")
top1000passwords

szukajHasla <- function(){
  haslo <- "brak"
  for(i in 1:1000){
    a <- top1000passwords[i]
    if (proton(action = "login", login="johnins", password=a) == "Success! User is logged in!"){
      haslo <- top1000passwords[i]
    }
  }
  
  haslo
}

szukajHasla()

for(i in 1:10){
  if (proton(action = "login", login="johnins", password=top1000passwords[i]) == "Success! User is logged in!"){
    top1000passwords[i]
  }
  else{
    haslo
  }
}
a <- top1000passwords[20]
proton(action = "login", login="johnins", password=a)

data("logs")
head(logs)
dane <- data.frame(table(logs[logs$login == "slap", ]$host))

dane[order(-dane$Freq), ][1, 1]


proton(action = "server", host="194.29.178.16")

data("bash_history")
bash_history

sbh <- strsplit(bash_history, " ")
comads <- ""
for(i in sbh){
  comads <- c(comads, i[[1]])
}

for (i in comads){
  proton(action = "login", login="johnins", password=i)
}



## 5) Umieszczamy rozwiazanie na repozytorium.