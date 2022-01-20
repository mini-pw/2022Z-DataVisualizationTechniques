#df to twoja ramka, who to np "Michał Mazuryk"
word_df<- df %>%  filter(sender_name==who) %>%
  select(content,timestamp_ms) %>%
  mutate(year = format(timestamp_ms, format="%Y")) %>%  
  unnest_tokens(input=content,output='word',format='text',drop=TRUE, to_lower=TRUE) %>%
  mutate(ni= nchar(word)) %>% filter(ni>4,year>2015) %>% 
  group_by(year,word) %>% count() %>%
  ungroup() %>% 
  group_by(year)%>%
  mutate(rank = rank(-n),
         Value_rel = n/n[rank==1],
         Value_lbl = paste0(" ",n)) %>%
  group_by(word) %>%
  filter(rank <= 10)
#poprawcie już ręcznie tam gdzie rank jest z ułamkiem
View(word_df)

word_df[16,]$rank<-3
word_df[20,]$rank<-2
word_df[5,]$rank<-7
word_df[8,]$rank<-8

anim<-ggplot(word_df, aes(rank, group = word))+
  geom_tile(aes(y = n/2,
                height = n,
                width = 0.9), alpha = 0.8, fill = "red") +
  geom_text(aes(y = 0, label = paste(word, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the Nationlity label
  geom_text(aes(y=n,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  dark_theme_gray(base_family = "Arial") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.grid.major.x = element_line( size=.1, color="black" ),
        panel.grid.minor.x = element_line( size=.1, color="black" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold",     colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="red"),
        plot.margin = margin(1,4, 1, 8, "cm")) +
  transition_states(year, transition_length = 5, state_length = 2) +
  ease_aes('sine-in-out') +
  labs(title = "Most used words by year: {closest_state}")
#zmien nazwe
animate(anim, nframes = 350,fps = 25,  width = 1200, height = 1000, 
        renderer = gifski_renderer("Magic Mike3.gif"))



