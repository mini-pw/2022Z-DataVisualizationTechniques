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
mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am","wt","mpg")]

# Jak wybierać jedną kolumnę?
mtcars[,2]
mtcars[,"am"]
mtcars$carb


# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl==4, "drat"])

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

data("employees")

employees[employees$name == "John",]

login<- "johnins"

proton(action = "login", login=login)

for(i in 1:1000){
  password <- top1000passwords[i]
  if(proton(action = "login", login= login, password=password) == "Success! User is logged in!"){
    i
    password
  }
}

employees[employees$surname == "Pietraszko",]


logs
logs2<- logs[logs$login == "slap",]
maxim<- max(table(logs2$host))
maxim
proton(action = "server", host='194.29.178.16')


bash_history
split_bash_history <- strsplit(bash_history, " ")
for (x in split_bash_history){
  comands <- c(comands, x[[1]])
}

for (comads in unique(comads)){
  proton(action = "login", login = "slap", password = comand)
}


## 5) Umieszczamy rozwiązanie na repozytorium.
