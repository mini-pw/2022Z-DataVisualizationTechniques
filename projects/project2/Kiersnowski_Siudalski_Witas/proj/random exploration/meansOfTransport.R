
activity <- read.csv("data/dataCSV/ActivitySegmenttestData.csv")

library(dplyr)
library(ggplot2)


act <- activity %>% 
  group_by(User, ActivityType) %>% 
  summarise(odl = sum(Distance)/1000, czas = (sum(EndtimeStampInMS)-sum(StartingtimeStampInMS))/(1000*60*60)  )

library(ggrepel)

act %>% 
  ggplot( aes(x = odl, y=czas, label = ActivityType, color=User) ) +
  geom_point() +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(limits = c(0, 18)) +
  geom_label_repel() +
  labs(title= "Distance and time spent in means of transport",
       y="Time[h]",
       x="Distance[km]")
