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
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
head(mtcars[,2:3], 10)
# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]
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
mean(mtcars$drat[mtcars$cyl==4])
mean(mtcars[mtcars$cyl == 4, "drat"])
# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))
## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library("proton")
proton()
#1
emp <- employees
log <- emp[emp$surname=="Insecure", 'login']
proton(action = "login", login = login)

#2
for (pass in top1000passwords){
  proton(action = "login", login=log, password = pass) 
}
  
#3
pietLog = emp[emp$surname == "Pietraszko", "login"]
goodHost <- names(tail(sort(table(logs[logs$login == pietLog, "host"])), 1))
proton(action = "server", host=goodHost)


#4
bash <- bash_history
maybe = c()
for (com in bash_history){
  maybe = c(maybe, strsplit(com, split = " ")[[1]])
}
maybe.



## 5) Umieszczamy rozwiązanie na repozytorium.
 
 