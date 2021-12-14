library(plotly)
library(dplyr)
library(tidyr)
library(countrycode)
setwd
df_raw <- read.csv('complete.csv', encoding="UTF-8")



df <- df_raw %>% 
  select(awardYear, category, portion, name, gender, birth_date, birth_country=birth_countryNow) %>% 
  filter(birth_country != "") %>% 
  # Nie wszystko się da zautomatyzowac
  mutate(birth_country = case_when(
    birth_country %in% c("Scotland","Northern Ireland") ~ "United Kingdom",
    birth_country == "Faroe Islands (Denmark)" ~ "Denmark",
    TRUE ~ birth_country)) 


df_wide <- df %>% 
  group_by(birth_country, awardYear,category) %>% 
  summarise(count=n()) %>% 
  pivot_wider(
    id_cols = c(birth_country,awardYear),
    names_from = category,
    values_from = count
    # ,
    # values_fill = 0
  ) 

aux_df <- expand.grid(birth_country = unique(df_wide$birth_country),
                      awardYear = unique(df_wide$awardYear))
df_wide <- merge(aux_df, df_wide, all=T) %>% 
  # Mamy kraj pochodzenia, potrzebujemy kodu
  mutate(country_code= countrycode(birth_country, origin= 'country.name', destination = 'iso3c'))
df_wide[is.na(df_wide)] <- 0


df_final_wide <- df_wide %>% 
  arrange(birth_country,awardYear) %>% 
  ungroup() %>% 
  group_by(country_code, birth_country) %>% 
  mutate(across(Literature:`Economic Sciences`, cumsum))



fig_chem <- plot_ly(df_final_wide, type='choropleth', locations=~country_code,
                    z = ~Chemistry, text= ~birth_country,
                    frame = ~awardYear, colorscale="Blues", reversescale=T) %>% 
  colorbar(title = "Liczba laureatów", x = 1.05, y =0.7, limits = c(0,max(df_final_wide$Chemistry))) %>% 
  layout(title="Liczba laureatów nagrody Nobla z chemii do danego roku \n urodzonych w danym kraju") %>% 
  animation_opts(1000) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Rok: ", font = list(color="purple")))



fig_econ <- plot_ly(df_final_wide, type='choropleth', locations=~country_code,
                    z = ~`Economic Sciences`, text= ~birth_country,
                    frame = ~awardYear, colorscale="Blues", reversescale=T) %>% 
  colorbar(title = "Liczba laureatów", x = 1.05, y =0.7, limits = c(0,max(df_final_wide$`Economic Sciences`))) %>% 
  layout(title="Liczba laureatów nagrody Nobla z ekonomii do danego roku \n urodzonych w danym kraju") %>% 
  animation_opts(1000) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Rok: ", font = list(color="purple")))
#
fig_lit <- plot_ly(df_final_wide, type='choropleth', locations=~country_code,
                   z = ~Literature, text= ~birth_country,
                   frame = ~awardYear, colorscale="Blues", reversescale=T) %>%
  colorbar(title = "Liczba laureatów", x = 1.05, y =0.7, limits = c(0,max(df_final_wide$Literature))) %>%
  layout(title="Liczba laureatów nagrody Nobla z literatury do danego roku \n urodzonych w danym kraju") %>%
  animation_opts(1000) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Rok: ", font = list(color="purple")))


fig_peace <- plot_ly(df_final_wide, type='choropleth', locations=~country_code,
                    z = ~Peace, text= ~birth_country,
                    frame = ~awardYear, colorscale="Blues", reversescale=T) %>% 
  colorbar(title = "Liczba laureatów", x = 1.05, y =0.7, limits = c(0,max(df_final_wide$Peace))) %>% 
  layout(title="Liczba laureatów pokojowej nagrody Nobla do danego roku \n urodzonych w danym kraju") %>% 
  animation_opts(1000) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Rok: ", font = list(color="purple")))


fig_phys <- plot_ly(df_final_wide, type='choropleth', locations=~country_code,
                    z = ~Physics, text= ~birth_country,
                    frame = ~awardYear, colorscale="Blues", reversescale=T) %>% 
  colorbar(title = "Liczba laureatów", x = 1.05, y =0.7, limits = c(0,max(df_final_wide$Physics))) %>% 
  layout(title="Liczba laureatów nagrody Nobla z fizyki do danego roku \n urodzonych w danym kraju") %>% 
  animation_opts(1000) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Rok: ", font = list(color="purple")))


fig_med <- plot_ly(df_final_wide, type='choropleth', locations=~country_code,
                   z = ~`Physiology or Medicine`, text= ~birth_country,
                   frame = ~awardYear, colorscale="Blues", reversescale=T) %>% 
  colorbar(title = "Liczba laureatów", x = 1.05, y =0.7, limits = c(0,max(df_final_wide$`Physiology or Medicine`))) %>% 
  layout(title="Liczba laureatów nagrody Nobla z medycyny do danego roku \n urodzonych w danym kraju") %>% 
  animation_opts(1000) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Rok: ", font = list(color="purple")))

# 
# # Nie dziala, bo jakies bugi, nie znalazlem odpowiedzi w internecie jak zrobic
# fig <- subplot(fig_chem, fig_econ, fig_lit, fig_peace, fig_phys, fig_med, nrows=3) 
# 
# 
