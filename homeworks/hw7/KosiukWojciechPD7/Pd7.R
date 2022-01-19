

#############


library(extrafont)
library(png)
library(dplyr)
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2) 

### Poniżej należy wprowadzić ścieżkę do czcionek w systemie
font_import(paths = "C:/Users/wojtek/AppData/Local/Microsoft/Windows/Fonts")

loadfonts(device="win")


lyrics <- c("Last Christmas I gave you my heart
But the very next day you gave it away
This year to save me from tears
I'll give it to someone special Last Christmas I gave you my heart
But the very next day you gave it away
This year to save me from tears
I'll give it to someone special")


lyrics <- gsub("[\r\n]", " ", lyrics)
words <- c()
words <- append(words,strsplit(lyrics," "))
freq <- append(rep(3,2),append(rep(2,5),rep(1,51)))

df <- data.frame(words,freq)
df <- rename(df,word=c..Last....Christmas....I....gave....you....my....heart....But...)


c1 <- rep("gold",2)
c2 <- rep("silver",56)
c <- append(c1,c2)


df <- df %>% mutate(col = c)

tree <- wordcloud2(data=df,
           size=0.6,
           figPath = "C:/Users/wojtek/Desktop/pw/Techniki wizualizacji danych/Pd7/KosiukWojciechPD7/tree3.png",
           minRotation = 0,maxRotation = 0,
           color = df$col,
           fontFamily = "Christmas Dream",
           backgroundColor = "#9b2226"
          )
tree
