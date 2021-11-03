kraj <- c("Wielka Brytania", "Norwegia", "Szwecja", "Litwa", "Holandia",
         "Polska", "Niemcy", "Węgry", "Bułgaria", "Grecja",
         "Włochy", "Francja", "Hiszpania")

pb95 <- c(7.68, 8.00, 8.00, 6.41, 9.40,
          5.91, 7.62, 6.00, 5.47, 8.04,
          8.22, 7.69, 6.90)

olej <- c(7.93, 8.69, 8.69, 6.03, 7.88,
          5.93, 7.13, 6.17, 5.47, 6.86,
          7.59, 7.34, 6.34)

df <- data.frame(
  Kraj = kraj,
  Pb95 = pb95,
  Olej = olej
)

df %>%
  mutate(Kraj = forcats::fct_reorder(Kraj, Pb95)) %>% 
  pivot_longer(c(Pb95, Olej), names_to = "Rodzaj.paliwa") %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = Kraj, y = value, fill = Rodzaj.paliwa),
    stat = "identity",
    position = "dodge"
    ) +
  labs(
    title = "Ceny paliw w Europie",
    subtitle = "Średnie ceny detaliczne benzyny Pb95 i oleju napędowego w wybranych krajach europejskich",
    y = "Cena jednego litra paliwa podana w złotych",
    x = "Kraj"
  ) + 
  scale_fill_manual(
    values = c("black", "darkgreen"),
    labels = c("Olej napędowy", "Benzyna Pb95"),
    name = "Rodzaj paliwa"
  ) +
  scale_x_discrete(guide = guide_axis(angle = 40))

ggsave("~/Projekty/twd/prace_domowe/hw2/hw2.png", scale = 1.7)
