library(dplyr)
library(stringr)
library(fmsb)

C6H6 <- read.csv("./data/2019_C6H6_1g.csv", sep = ';')
NOx <- read.csv("./data/2019_NOx_1g.csv", sep = ';')
O3 <- read.csv("./data/2019_O3_1g.csv", sep = ';')
SO2 <- read.csv("./data/2019_SO2_1g.csv", sep = ';')
As <- read.csv("./data/2019_As_24g.csv", sep = ';')
BaP <- read.csv("./data/2019_BaP_24g.csv", sep = ';')


As <- As %>% mutate_all(na_if,"")
As <- data.frame(lapply(As, function(x) {gsub(",", ".", x)}))
As[,-1] <- lapply(As[-1], function(x) {
  as.numeric(x)
})
As$Miesiac <- as.numeric(substring(As$Data, 4, 5))
As$PoraRoku <- lapply(As$Miesiac, function(x) {
  if (x == 1) {return("zima")}
  if (x == 2) {return("zima")}
  if (x == 3) {return("wiosna")}
  if (x == 4) {return("wiosna")}
  if (x == 5) {return("wiosna")}   
  if (x == 6) {return("lato")}
  if (x == 7) {return("lato")}   
  if (x == 8) {return("lato")}
  if (x == 9) {return("jesien")}   
  if (x == 10) {return("jesien")}
  if (x == 11) {return("jesien")}   
  if (x == 12) {return("zima")}
})


BaP <- BaP %>% mutate_all(na_if,"")
BaP <- data.frame(lapply(BaP, function(x) {gsub(",", ".", x)}))
BaP[,-1] <- lapply(BaP[-1], function(x) {
  as.numeric(x)
})
BaP$Miesiac <- as.numeric(substring(BaP$Data, 4, 5))
BaP$PoraRoku <- lapply(BaP$Miesiac, function(x) {
  if (x == 1) {return("zima")}
  if (x == 2) {return("zima")}
  if (x == 3) {return("wiosna")}
  if (x == 4) {return("wiosna")}
  if (x == 5) {return("wiosna")}   
  if (x == 6) {return("lato")}
  if (x == 7) {return("lato")}   
  if (x == 8) {return("lato")}
  if (x == 9) {return("jesien")}   
  if (x == 10) {return("jesien")}
  if (x == 11) {return("jesien")}   
  if (x == 12) {return("zima")}
})


C6H6 <- C6H6 %>% mutate_all(na_if,"")
C6H6 <- data.frame(lapply(C6H6, function(x) {gsub(",", ".", x)}))
C6H6[,-1] <- lapply(C6H6[-1], function(x) {
  as.numeric(x)
})
C6H6$Miesiac <- as.numeric(substring(C6H6$Data, 4, 5))
C6H6$PoraRoku <- lapply(C6H6$Miesiac, function(x) {
  if (x == 1) {return("zima")}
  if (x == 2) {return("zima")}
  if (x == 3) {return("wiosna")}
  if (x == 4) {return("wiosna")}
  if (x == 5) {return("wiosna")}   
  if (x == 6) {return("lato")}
  if (x == 7) {return("lato")}   
  if (x == 8) {return("lato")}
  if (x == 9) {return("jesien")}   
  if (x == 10) {return("jesien")}
  if (x == 11) {return("jesien")}   
  if (x == 12) {return("zima")}
})


NOx <- NOx %>% mutate_all(na_if,"")
NOx <- data.frame(lapply(NOx, function(x) {gsub(",", ".", x)}))
NOx[,-1] <- lapply(NOx[-1], function(x) {
  as.numeric(x)
})
NOx$Miesiac <- as.numeric(substring(NOx$Data, 4, 5))
NOx$PoraRoku <- lapply(NOx$Miesiac, function(x) {
  if (x == 1) {return("zima")}
  if (x == 2) {return("zima")}
  if (x == 3) {return("wiosna")}
  if (x == 4) {return("wiosna")}
  if (x == 5) {return("wiosna")}   
  if (x == 6) {return("lato")}
  if (x == 7) {return("lato")}   
  if (x == 8) {return("lato")}
  if (x == 9) {return("jesien")}   
  if (x == 10) {return("jesien")}
  if (x == 11) {return("jesien")}   
  if (x == 12) {return("zima")}
})


O3 <- O3 %>% mutate_all(na_if,"")
O3 <- data.frame(lapply(O3, function(x) {gsub(",", ".", x)}))
O3[,-1] <- lapply(O3[-1], function(x) {
  as.numeric(x)
})
O3$Miesiac <- as.numeric(substring(O3$Data, 4, 5))
O3$PoraRoku <- lapply(O3$Miesiac, function(x) {
  if (x == 1) {return("zima")}
  if (x == 2) {return("zima")}
  if (x == 3) {return("wiosna")}
  if (x == 4) {return("wiosna")}
  if (x == 5) {return("wiosna")}   
  if (x == 6) {return("lato")}
  if (x == 7) {return("lato")}   
  if (x == 8) {return("lato")}
  if (x == 9) {return("jesien")}   
  if (x == 10) {return("jesien")}
  if (x == 11) {return("jesien")}   
  if (x == 12) {return("zima")}
})


SO2 <- SO2 %>% mutate_all(na_if,"")
SO2 <- data.frame(lapply(SO2, function(x) {gsub(",", ".", x)}))
SO2[,-1] <- lapply(SO2[-1], function(x) {
  as.numeric(x)
})
colnames(SO2)[1] <- "Data"
SO2$Miesiac <- as.numeric(substring(SO2$Data, 4, 5))
SO2$PoraRoku <- lapply(SO2$Miesiac, function(x) {
  if (x == 1) {return("zima")}
  if (x == 2) {return("zima")}
  if (x == 3) {return("wiosna")}
  if (x == 4) {return("wiosna")}
  if (x == 5) {return("wiosna")}   
  if (x == 6) {return("lato")}
  if (x == 7) {return("lato")}   
  if (x == 8) {return("lato")}
  if (x == 9) {return("jesien")}   
  if (x == 10) {return("jesien")}
  if (x == 11) {return("jesien")}   
  if (x == 12) {return("zima")}
})


n <- 10

siarka <- SO2[,2:(length(SO2)-2)]
siarka <- as.data.frame(colMeans(na.omit(siarka)))
colnames(siarka) <- c('pomiar')
siarka <- head(siarka %>% arrange(-pomiar), n)

benzen <- C6H6[,2:(length(C6H6)-2)]
benzen <- as.data.frame(colMeans(na.omit(benzen)))
colnames(benzen) <- c('pomiar')
benzen <- head(benzen %>% arrange(-pomiar), n)


tlen <- O3[,2:(length(O3)-2)]
tlen <- as.data.frame(colMeans(na.omit(tlen)))
colnames(tlen) <- c('pomiar')
tlen <- head(tlen %>% arrange(-pomiar), n)


azot <- NOx[,2:(length(NOx)-2)]
azot <- as.data.frame(colMeans(na.omit(azot)))
colnames(azot) <- c('pomiar')
azot <- head(azot %>% arrange(-pomiar), n)


arsen <- As[,2:(length(As)-2)]
arsen <- as.data.frame(colMeans(na.omit(arsen)))
colnames(arsen) <- c('pomiar')
arsen <- head(arsen %>% arrange(-pomiar), n)

benzoapiren <- BaP[,2:(length(BaP)-2)]
benzoapiren <- as.data.frame(colMeans(na.omit(benzoapiren)))
colnames(benzoapiren) <- c('pomiar')
benzoapiren <- head(benzoapiren %>% arrange(-pomiar), n)


l <- list(benzen, benzoapiren, arsen, azot, tlen, siarka)
names <- c('C6H6', 'BaP', 'As', 'NOx', 'O3', 'SO2')


i = 1

for (df in list(C6H6, BaP, As, NOx, O3, SO2)) {
  df <- filter(df, PoraRoku == 'zima')
  df <- select(df, rownames(as.data.frame(l[i])))
  assign(paste("Top_zima_", names[i], sep = ''), mean(na.omit(as.double(as.matrix(df)))))
  i <- i + 1
}

i = 1

for (df in list(C6H6, BaP, As, NOx, O3, SO2)) {
  df <- filter(df, PoraRoku == 'lato')
  df <- select(df, rownames(as.data.frame(l[i])))
  assign(paste("Top_lato_", names[i], sep = ''), mean(na.omit(as.double(as.matrix(df)))))
  i <- i + 1
}

Top_zima_lato_radar <- data.frame(C6H6 = c(10, 0, `Top_zima_C6H6`, `Top_lato_C6H6`),
                                  BaP = c(2, 0, `Top_zima_BaP`, `Top_lato_BaP`),
                                  As = c(12, 0, `Top_zima_As`, `Top_lato_As`),
                                  NOx = c(160, 0, `Top_zima_NOx`, `Top_lato_NOx`),
                                  O3 = c(240, 0, `Top_zima_O3`, `Top_lato_O3`),
                                  SO2 = c(40, 0, `Top_zima_SO2`, `Top_lato_SO2`))

row.names(Top_zima_lato_radar) = c('Max', 'Min', 'Value_zima', 'Value_lato')


radarchart(Top_zima_lato_radar,
           plty = 1,
           pcol = c('#2b8cbe', '#fd8d3c'),
           pfcol = c(rgb(166,189,219, max = 255, alpha = 150), rgb(254,217,118, max = 255, alpha = 120)),
           plwd = 3,
           vlcex = 1.6,
           cglcol = "grey26",
           cglty = 1,
           caxislabels = c('0%', '', '100%', '', '200%'),
           cglwd = 2,
           axislabcol = 'grey26',
           calcex = 1.7,
           vlabels = c("", "", "", "", "", ""),
)

legend(x=1, y=1.25, legend = c('zima', 'lato'), 
       pch=20, col=c(rgb(166,189,219, max = 255), rgb(254,217,118, max = 255)) , 
       text.col = "grey26", cex=1.3, pt.cex=5, text.font = 15)
