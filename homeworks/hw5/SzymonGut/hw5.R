library(plotly)
library(dplyr)
library(stringr)
library(tidyr)
df <- read.csv("complete.csv")

df$gender <- str_to_title(df$gender)
df$gender <- factor(df$gender, levels = c('Male', 'Female'))
df_fm <- df %>% 
  drop_na(gender) %>% 
  group_by(gender,category) %>% 
  summarize(n = n())
#stworzenie odpowiednich datasetow ktore wczytuje wybierajac odpowiednie pole 
#na wykresie (female, male)
df_f <- df_fm %>% filter(gender == "Female")

df_m <- df_fm %>% filter(gender == "Male")

df_together <- df %>% 
  drop_na(gender) %>% 
  group_by(category) %>% 
  summarize(n = n())

steps <- list(
  list(args = list(list("marker.color" = list("#756bb1")
                        )),
       label = "Purple Bars",
       method = "restyle"
  ),
  list(args = list(list("marker.color" = list("#dd1c77")
                       )),
       label = "Pink Bars",
       method = "restyle"
  ),
  list(args = list(list("marker.color" = list("#d95f0e")
                        )), 
       label = "Orange Bars", 
       method = "restyle"
  )
)


plot_ly(
  data = df_f,
  x = ~n,
  y = ~category,
  marker =  list(color =  "#756bb1")
) %>% layout(
  title = "Gender wise no. of winners",
  updatemenus = list( #lista obiektow
    list(#obiekt
      x = 1, y = 1,
      buttons = list(#lista obiektow
        list(method = "update",
             args = list(list(x = list(df_f$n),y = list(df_f$category))),
             label = "Female"),#obiekt/wartosc
        list(method = "update",
             args = list(list(x = list(df_m$n),y = list(df_m$category))),
             label = "Male"),
        list(method = "update",
             args = list(list(x = list(df_together$n),y = list(df_together$category))),
             label = "Together")
      ))
  )) %>% 
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE) %>% 
  layout( plot_bgcolor='#e5ecf6',
          xaxis = list(
            title = "number of winners",
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(
            title = "",
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          sliders = list(
            list(
              active = 0,
              currentvalue = list(prefix = "Palette: "),
              pad = list(t = 60),
              steps = steps
            )
          ))