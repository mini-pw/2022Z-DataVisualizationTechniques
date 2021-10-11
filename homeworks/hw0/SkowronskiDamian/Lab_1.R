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




# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[1,7]
# Pierwszy wiersz, pierwsza kolumna?  
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
head(mtcars[,c(2,3)])
# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
head(mtcars[,c("am","wt","mpg")])
# Jak wybierać jedną kolumnę?
mtcars$hp
# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
# 2. Jakie są typy zmiennych?
str(mtcars)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat[mtcars$cyl == 4])
mean(mtcars[mtcars$cyl ==4,"drat"])

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg,mtcars$hp)


# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))
## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()



## 5) Umieszczamy rozwiązanie na repozytorium.
john_login = employees[employees$surname == 'Insecure',3]
proton(action = "login", login=johnlogin)

for(pass in top1000passwords){
  proton(action = "login", login = johnlogin, password=pass)
}
pietr_login = employees[employees$surname == "Pietraszko","login"]

tmp <- logs[logs$login == login,]
which.max(table(tmp$host))
proton(action = "server", host="194.29.178.16")
v <- unlist(unique(strsplit(bash_history, " ")))
#tu to haslo jest ostatnie po prostu pewnie dla wygody, ale legitnie robie petla
#"DHbb7QXppuHnaXGN"

for(pass in v){
  proton(action = "login", login = pietr_login, password=pass)
}


