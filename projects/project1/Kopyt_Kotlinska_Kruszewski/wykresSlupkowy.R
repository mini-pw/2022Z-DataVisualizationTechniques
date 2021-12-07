library(readxl)
library(dplyr)
library(ggplot2)

#przygotowanie ramki danych
kwiatyapszczoly <- read_excel("/Users/Admin/Documents/kwiatyapszczoly.xlsx")
View(df)
df <- kwiatyapszczoly %>% 
  select("Plant cultivar", "Total bees/m2") %>% 
  arrange("Total bees/m2") %>% 
  top_n(6)
colnames(df) <- c("nazwa", "ilosc")
df$nazwa = c("Calamintha nepeta", "Common sneezeweed", "Cranesbill", "Sahin’s early flowerer","Borage","Oregano")

#przygotowanie wykresu
p <- ggplot(df, aes(x = reorder(nazwa, -ilosc), y = ilosc, fill = nazwa)) +
  geom_bar(stat="identity") +
  coord_flip()+
  guides(fill = "none") +
  theme_minimal()+
  scale_fill_manual(values = c("#feb24c", "#feb24c", "#feb24c", "#feb24c", "#feb24c","#feb24c"))+
  theme(
    text = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.y = element_blank(),
    axis.ticks.x = element_line(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
    )

#zapisanie wykresu
png('wykres_pszczoly_7.png',width=1100,height=500,units="px",bg = "transparent")
print(p)
dev.off()