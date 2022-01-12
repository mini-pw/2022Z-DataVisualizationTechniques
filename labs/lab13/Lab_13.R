###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 13           ###
###########################################

library(tidyverse)
?diamonds
# na podstawie: https://r4ds.had.co.nz/exploratory-data-analysis.html

library(dplyr)
library(ggplot2)

# Eksploracyjna analiza danych jest cyklem iteracyjnym:
# 1) Generuj pytania dotyczące Twoich danych
# 2) Znajdź odpowiedź poprzez wizualizację, przkształcenie lub modelowanie danych
# 3) Zdobytą wiedzę wykorzystaj do dopracowania pytań i/lub wygenerowania nowych.

# EDA nie jest formalnym procesem o ścisłym zestawie reguł, ale jest ważną częścią
# ponieważ pozwala na zbadanie jakości danych.

# Zrozumienie zbioru danych:
#   - wyodrębnienie ważnych zmiennych i pozostawienie bezużytecznych zmiennych,
#   - identyfikacja wartości odstających, brakujących wartości lub błędów ludzkich,
#   - zrozumienie zależności lub ich braku pomiędzy zmiennymi.

## 0) Kilka informacji o zbiorze danych

## wymiar ramki danych

## kilka pierwszych wierszy

## jakie mamy kolumny?

## liczność, średnia, odchylenie, min, max dla każdej zmiennej, liczba braków danych

## ile mamy unikalnych wartości dla każdej zmiennej


## 1) Rozkłady zmiennych

# -- zmienne jakościowe
#cut, color, clarity

## wykres dla wybranej zmiennej - jaki typ wykresu?


## tabela dla wybranej zmiennej


# -- zmienne ilościowe
#carat, depth, table, price, x, y, z 

## wykres dla wybranej zmiennej - jaki typ wykresu?


## tabela dla wybranej zmiennej


# -- zależności zmiennych
#carat ~ cut


# ? Które wartości są najczęstsze? 
# ? Które wartości są rzadkie?
# ? Czy widzisz jakieś nietypowe wzorce? 


  
## Outliers





## Zadanie
# a) Zbadaj rozkład każdej ze zmiennych x, y i z.
# b) Zbadaj rozkład ceny.
# c) Ile diamentów ma masę 0,99 karata? 
#    Ile jest takich, które mają 1 karat? Co wpływa na różnicę?



## Zależność dwóch zmiennych - jak badać?
# a) Jakościowa i ilościowa:
#     - freqpoly()
#     - geom_boxplot()
#     - geom_violin()
# b) Dwie zmienne jakościowe:
#     - geom_count()
#     - geom_tile()
# c) Dwie zmienne ilościowe:
#     - geom_point()
#     - geom_bin2d()
#     - geom_hex()


## *) R Markdown

# 1) R Markdown - możliwości

# https://bookdown.org/yihui/rmarkdown/
# https://bookdown.org/yihui/rmarkdown-cookbook/

# a) theme & highlight

# theme: cerulean, journal, flatly, darkly, readable, spacelab, united, cosmo, lumen, paper, sandstone, simplexand yeti
# highlight: espresso, tango, pygments, kate, monochrome, zenburn, haddock and textmate

# https://www.datadreaming.org/post/r-markdown-theme-gallery/

# b) parameters 

# toc: true 
# number_sections: true
# toc_depth: 4
# toc_float: true 

# c) tabels

# knitr::kable()
# https://www.tablesgenerator.com/latex_tables

# kableEkstra
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

# d) message, error, warning

# e) figure



## *) dlookr
# https://github.com/choonghyunryu/dlookr
install.packages("dlookr")
library(dlookr)
library(dplyr)
summary(mtcars)

describe(mtcars)

mtcars %>% 
  describe() %>% 
  select(variable, mean, IQR, p50)

normality(mtcars)
plot_normality(mtcars, mpg)

plot_normality(mtcars, cyl)

plot_correlate(mtcars)


## *) dataReporter
# https://github.com/ekstroem/dataMaid
# https://github.com/ekstroem/dataReporter
install.packages("dataReporter")
library("dataReporter")

makeDataReport(mtcars)
data("mtcars")


## *) DataExplorer
# https://github.com/boxuancui/DataExplorer
install.packages("DataExplorer")
library("DataExplorer")

titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

plot_histogram(titanic)
plot_bar(titanic)


## *) visdat
# https://github.com/ropensci/visdat
install.packages("visdat")
library("visdat")

vis_miss(airquality, sort_miss = TRUE, cluster = TRUE)
