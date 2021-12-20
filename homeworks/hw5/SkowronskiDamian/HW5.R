library(readr)
library(dplyr)
library(plotly)
library(tidyr)


df <- read_csv("complete.csv", col_types = cols(awardYear = col_date(format = "%Y"))) %>% 
  select("awardYear","category", "birth_countryNow", "birth_continent")


#ramka z liczbą laaureatów w zależności od kontynentu, na którym się urodzili
cont <- df %>% 
  select(birth_continent,awardYear) %>% 
  filter(!is.na(birth_continent),!is.na(awardYear)) %>% 
  group_by(birth_continent,awardYear) %>% 
  count() %>% 
  #takie zagranie robię, żeby wykres nie był skrócony na końcach, chodzi o to, że uzupełniam wszystkie lata, w których nie było nobla na 0 i wtedy jak się kumuluje to jest ciągłe do końca
  pivot_wider(names_from = awardYear, values_from = n,values_fill = 0) %>%  
  pivot_longer(cols = -birth_continent, names_to = "awardYear" ,values_to = "n") %>% 
  group_by(birth_continent) %>% 
  arrange(awardYear, .by_group = TRUE) %>% 
  mutate(cumulative = cumsum(n), awardYear = as.Date(awardYear,"%Y-%m-%d")) %>% 
  mutate(birth_continent = forcats::fct_reorder(birth_continent,n,.desc =TRUE))


#ramka pomocnicza, żeby wybrać 6 państw, w których jest najwięcej lauretów
#akurat 6 państw, bo się polska mieści  
countries <- df %>% 
  filter(!is.na(birth_countryNow)) %>% 
  group_by(birth_countryNow) %>% 
  count() %>%
  arrange(desc(n)) %>% 
  head(6)


#ramka z liczbą laaureatów w zależności od państwa, w którym się urodzili
ctr <- df %>% 
  select(birth_countryNow,awardYear) %>% 
  filter(!is.na(birth_countryNow),!is.na(awardYear),birth_countryNow %in% countries$birth_countryNow) %>% 
  group_by(birth_countryNow,awardYear) %>% 
  count() %>% 
  pivot_wider(names_from = awardYear, values_from = n,values_fill = 0) %>% 
  pivot_longer(cols = -birth_countryNow, names_to = "awardYear" ,values_to = "n") %>% 
  group_by(birth_countryNow) %>% 
  arrange(awardYear, .by_group = TRUE) %>% 
  mutate(cumulative = cumsum(n), awardYear = as.Date(awardYear,"%Y-%m-%d")) 

#ramka z liczbą laureatów w zależności od kategorii
categ <- df %>% 
  select(category,awardYear) %>% 
  filter(!is.na(category),!is.na(awardYear)) %>% 
  group_by(category,awardYear) %>% 
  count() %>% 
  pivot_wider(names_from = awardYear, values_from = n,values_fill = 0) %>% 
  pivot_longer(cols = -category, names_to = "awardYear" ,values_to = "n") %>% 
  group_by(category) %>% 
  arrange(awardYear, .by_group = TRUE) %>% 
  mutate(cumulative = cumsum(n), awardYear = as.Date(awardYear,"%Y-%m-%d")) 
  

#plot kontynentów
plt <- plot_ly() %>% 
  add_lines(data = cont, x = ~awardYear,y = ~cumulative, split = ~birth_continent,
            text = paste0("Year: ", format(as.Date(cont$awardYear, format="%d/%m/%Y"),"%Y"), "<br>Nr of laureates: ", cont$cumulative),
            hovertemplate = paste('<b>%{text}</b>')) 
#plot państw
plt <- plt %>% 
  add_lines(data = ctr, x = ~awardYear,y = ~cumulative, split = ~birth_countryNow, visible = F,
            text = paste0("Year: ", format(as.Date(ctr$awardYear, format="%d/%m/%Y"),"%Y"), "<br>Nr of laureates: ", ctr$cumulative),
            hovertemplate = paste('<b>%{text}</b>')) 
#plot kategorii
plt <- plt %>% 
  add_lines(data = categ, x = ~awardYear,y = ~cumulative, split = ~category, visible = F,
            text = paste0("Year: ", format(as.Date(categ$awardYear, format="%d/%m/%Y"),"%Y"), "<br>Nr of laureates: ", categ$cumulative),
            hovertemplate = paste('<b>%{text}</b>')) 

plt <- plt %>% layout(
  title = list(text = "Nobel Prize laureates by ", xanchor = "right"),
  xaxis = list(
    title = "Year",
    
    #przyciski z wyborem zakresu
    rangeselector = list(
      buttons = list(
        list(
          count = 10,
          label = "10 years",
          step = "year",
          stepmode = "backward"),
        list(
          count = 20,
          label = "20 years",
          step = "year",
          stepmode = "todate"),
        list(
          count = 50,
          label = "50 years",
          step = "year",
          stepmode = "backward"),
        list(step = "all"))
      ),
    
    #slider na dole
    rangeslider = list(type = "date")),
  yaxis = list(title = "Number of laureates"),
  
  #dropdown z prawej tytułu
  updatemenus = list(
    list(
      y = 1.08, #tutaj akurat takie są numery na ten przycisk, bo tak mi pasowało na ekranie, na innych moze być nierówno
      x = 0.62,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(T,T,T,T,T,T,F,F,F,F,F,F,F,F,F,F,F,F)), # czemu to nie jest dokadniej opisane???
             label = "Continent"),
        
        list(method = "restyle",
             args = list("visible", list(F,F,F,F,F,F,T,T,T,T,T,T,F,F,F,F,F,F)), # chodzi o to, że wybieram, które linie mają być widoczne
             label = "Country"),
        list(method = "restyle",
             args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,T,T,T,T,T,T)), 
             label = "Category")
        ))
  )
  )

plt

#zapis
#htmlwidgets::saveWidget(as_widget(plt), "nobelLaureates.html")