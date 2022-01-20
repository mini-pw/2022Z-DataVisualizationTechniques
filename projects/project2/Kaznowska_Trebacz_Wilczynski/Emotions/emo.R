library(emojifont)
library(ggplot2)
load.emojifont("EmojiOne.ttf")
quartz()
ggplot(foo, aes(name_emoji, n, label = emoji)) + 
  geom_bar(stat = "identity") +
  geom_text(family = "EmojiOne", size = 6, vjust = -.5) +
  scale_x_discrete(breaks = foo$name_emoji, labels = foo$emoji) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())