library(ggplot2)
library(dplyr)
library(hrbrthemes)

rainforests_deforestation <- read.csv("~/work-temp/3 sem/techniki wizualizacji danych/data/def_area_2004_2019.csv")
head(rainforests_deforestation)
rainforests_deforestation

df <- rainforests_deforestation %>%
  select(c(Ano.Estados, AMZ.LEGAL))
df

ggthemr_reset()

diagram <- df %>%
  ggplot( aes(x=Ano.Estados, y=AMZ.LEGAL)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ylab("Total deforested area (km²)") +
  xlab(NULL) +
  ggtitle("Brazilian Amazon Rainforest - Total Deforested Area per Year") +
  labs(caption = "Source: PRODES")

diagram
install.packages('https://cran.r-project.org/src/contrib/Archive/hrbrthemes/hrbrthemes_0.1.0.tar.gz', type='source', repos=NULL)


## WYKRESIK 2

pozary <- read.csv("~/work-temp/3 sem/techniki wizualizacji danych/data/inpe_brazilian_amazon_fires_1999_2019.csv")
pozary

df2 <- pozary %>%
  select(c(year, firespots)) %>%
  group_by(year) %>%
  summarise(firespots = sum(firespots))

diagram2 <- df2 %>%
  ggplot( aes(x=year, y=firespots)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ylab("Total fire outbreaks") +
  xlab(NULL) +
  ggtitle("Brazilian Amazon Rainforest - Fire Outbreaks Amount") +
  scale_y_continuous(labels = scales::comma) +
  labs(caption = "Source: BDQ")

diagram2


## WYKRESIK 3

df3 <- df
df3
df_temp <- df2 %>%
  filter(year > 2003)
df3 <- cbind(df3, firespots = df_temp$firespots)
df3

diagram3 <- df3 %>%
  ggplot() +
  geom_bar(aes(x = Ano.Estados, y = AMZ.LEGAL),
           position = "stack",
           stat = "identity",
           color = "darkgrey",
           fill = "darkgrey") +
  geom_line(aes(x = Ano.Estados, y = firespots/10),
            stat = "identity",
            color = "black") +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~.*10, labels = scales::comma, ylab("Total fire outbreaks")),
    labels = scales::comma
  ) +
  theme_ipsum() +
  ylab("Total deforested area (km²)") +
  xlab(NULL) +
  ggtitle("Brazilian Amazon Rainforest - Deforested Area vs Fire Outbreaks Amount") +
  labs(caption = "Source: PRODES, BDQ")
  



diagram3



