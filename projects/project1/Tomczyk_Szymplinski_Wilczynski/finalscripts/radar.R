# data: https://ourworldindata.org/grapher/electricity-prod-source-stacked

library(fmsb)
library(dplyr)

df <- read.csv("../plotsData/countries_power_source2.csv")

colnames(df) <- c("country","code","year","coal","gas","hydro","other_renewables","solar","oil","wind","nuclear")

df <- df %>%
  filter((df$year == 2019) & (df$country %in% c("Poland", "France", "Norway", "Italy"))) %>% 
  mutate(renewable = hydro + other_renewables + solar + wind)

rownames(df) <- df$country
df <- df %>% 
  select(c("renewable", "gas", "coal", "oil", "nuclear"))

sums <- (df$coal + df$gas + df$oil + df$nuclear + df$renewable)
df <- 100*df/sums

df <- rbind(rep(100,5) , rep(0,5) , df)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.6,0.2,0.7,0.7) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4), rgb(0.6,0.2,0.7,0.2) )

radarchart(df, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           caxislabels=seq(0,100,25), cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
           vlcex=0.8)

legend(x=0.7, y=1.4,
       legend = rownames(df[-c(1,2),]),
       bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
