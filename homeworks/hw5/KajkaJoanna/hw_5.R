library(plotly)
library(dplyr)
library(patchwork)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
nobel <- read.csv("complete.csv")
nobel <- nobel %>% filter(ind_or_org == "Individual") %>% rename(COUNTRY = birth_countryNow)

nobel$COUNTRY[nobel$COUNTRY == "Democratic Republic of the Congo"] <- "Congo, Democratic Republic of the"
nobel$COUNTRY[nobel$COUNTRY == "East Timor"] <- "Timor-Leste"
nobel$COUNTRY[nobel$COUNTRY == "Faroe Islands (Denmark)"] <- "Faroe Islands"
nobel$COUNTRY[nobel$COUNTRY == "Myanmar"] <- "Burma"
nobel$COUNTRY[nobel$COUNTRY == "North Macedonia"] <- "Macedonia"
nobel$COUNTRY[nobel$COUNTRY == "Northern Ireland"] <- "Ireland"
nobel$COUNTRY[nobel$COUNTRY == "Scotland"] <- "United Kingdom"
nobel$COUNTRY[nobel$COUNTRY == "South Korea"] <- "Korea, South"
nobel$COUNTRY[nobel$COUNTRY == "the Netherlands"] <- "Netherlands"
nobel$COUNTRY[nobel$COUNTRY == "USA"] <- "United States"

all <- nobel %>% group_by(COUNTRY) %>% summarise(amount = n())
Chemistry <- nobel %>% filter(category == "Chemistry") %>% group_by(COUNTRY) %>% summarise(amount = n())
Economic_Sciences <- nobel %>% filter(category == "Economic Sciences") %>% group_by(COUNTRY) %>% summarise(amount = n())
Literature <- nobel %>% filter(category == "Literature") %>% group_by(COUNTRY) %>% summarise(amount = n())
Peace <- nobel %>% filter(category == "Peace") %>% group_by(COUNTRY) %>% summarise(amount = n())
Physics <- nobel %>% filter(category == "Physics") %>% group_by(COUNTRY) %>% summarise(amount = n())
Physiology_or_Medicine <- nobel %>% filter(category == "Physiology or Medicine") %>% group_by(COUNTRY) %>% summarise(amount = n())

all <- left_join(df, all, by = "COUNTRY")
df_1 <- left_join(df, Chemistry, by = "COUNTRY")
df_2 <- left_join(df, Economic_Sciences, by = "COUNTRY")
df_3 <- left_join(df, Literature, by = "COUNTRY")
df_4 <- left_join(df, Peace, by = "COUNTRY")
df_5 <- left_join(df, Physics, by = "COUNTRY")
df_6 <- left_join(df, Physiology_or_Medicine, by = "COUNTRY")


fig <- plot_ly(all, type='choropleth', locations=all$CODE, z=all$amount, text=all$COUNTRY, colorscale="Blue") %>% layout(title = "All categories")
fig_1 <- plot_ly(df_1, type='choropleth', locations=df_1$CODE, z=df_1$amount, text=df_1$COUNTRY, colorscale = "Blue") %>% layout(title = "Chemistry")
fig_2 <- plot_ly(df_2, type='choropleth', locations=df_2$CODE, z=df_2$amount, text=df_2$COUNTRY, colorscale="Blue") %>%  layout(title = "Economic Sciences")
fig_3 <- plot_ly(df_3, type='choropleth', locations=df_3$CODE, z=df_3$amount, text=df_3$COUNTRY, colorscale="Blue") %>%  layout(title = "Literature")
fig_4 <- plot_ly(df_4, type='choropleth', locations=df_4$CODE, z=df_4$amount, text=df_4$COUNTRY, colorscale="Blue") %>%  layout(title = "Peace")
fig_5 <- plot_ly(df_5, type='choropleth', locations=df_5$CODE, z=df_5$amount, text=df_5$COUNTRY, colorscale="Blue") %>%  layout(title = "Physics")
fig_6 <- plot_ly(df_6, type='choropleth', locations=df_6$CODE, z=df_6$amount, text=df_6$COUNTRY, colorscale="Blue") %>%  layout(title = "Physiology or Medicine")


fig
fig_1
fig_2
fig_3
fig_4
fig_5
fig_6

