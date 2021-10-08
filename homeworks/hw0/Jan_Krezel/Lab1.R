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
head(mtcars[, 2:3])

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$hp

# Pytania
# Alt + -    <-
# Ctrl + Shift + m    %>% 

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
# hist(mtcars$cyl)
barplot(table(mtcars$cyl))

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees[employees$name == "John", ] # login johnins
proton(action = "login", login="johnins")

for(i in 1:1000)
{
  out <- proton(action="login", login="johnins", password=top1000passwords[i])
  if(out == "Success! User is logged in!")
    print(top1000passwords[i])
}

proton(action="login", login="johnins", password="q1w2e3r4t5")
# password q1w2e3r4t5
# login pietraszko slap

logs[logs$login == "slap",]
head(sort(table(logs[logs$login == "slap","host"]), decreasing = TRUE))
# 194.29.178.16
proton(action = "server", host="194.29.178.16")
tmp <- strsplit(bash_history, " ")

cmds = c("ls", "cd", "mcedit", "rm", "cp", "vim", "vi", "cat", "pwd", "mc", "ps", "whoiam", "top", "service")

for(i in 1:length(tmp))
{
  cmd <- tmp[[i]][1]
  if(!(cmd %in% cmds))
    print(cmd)
}

# DHbb7QXppuHnaXGN

proton(action="login", login="slap", password="DHbb7QXppuHnaXGN")

## 5) Umieszczamy rozwiązanie na repozytorium.