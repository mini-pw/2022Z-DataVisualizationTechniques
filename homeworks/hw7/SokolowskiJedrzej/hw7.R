# TWD HW7 Jędrzej Sokołowski
# Nr. albumu 313507

library(ggplot)
library(dplyr)

# Dane

df <- data.frame()

for (i in 100:0){
  x = c(runif(i,-i, i)) 
  y = c(rep(-i,i))
  temp_df = data.frame(x,y)
  df<- rbind(df, temp_df)

}

# Wizualizacja

fig <- ggplot(df, aes(x=x,y=y, group=x)) + geom_point(color = 'darkgreen', shape = 23)  + labs(title="Wesołych Świąt!") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y=element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

x <- 0
y <- 0

gwiazda <- data.frame(x,y)

fig <- fig + geom_point(data=gwiazda,aes(x=x, y = y), color ="yellow", size = 12, shape="star")

bombki_czerwone = df[sample(nrow(df), 30), ]

fig <- fig + geom_point(data=bombki_czerwone,aes(x=x, y = y), color ="red", size = 5, shape=19)

bombki_niebieskie = df[sample(nrow(df), 30), ]

fig <- fig + geom_point(data=bombki_niebieskie,aes(x=x, y = y), color ="blue", size = 5, shape=19)

anim_line <- fig +
  transition_reveal(x)

anim_line

