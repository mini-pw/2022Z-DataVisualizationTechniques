# Analysis of amount of money given each year for Nobel prizes
# Piotr Wilczynski, PW MiNI Iiad
# data: https://www.kaggle.com/imdevskp/nobel-prize

# adding libraries
library(plotly)
library(dplyr)

# prepering data
df <- read.csv("NobelPrizes.csv")

## data for "Total" tab
dfGrouped <- df %>%
  select(awardYear, prizeAmountAdjusted) %>% 
  rbind(data.frame(awardYear = 1901:2019,
                   prizeAmountAdjusted = rep(0, 119))) %>% 
  group_by(awardYear) %>% 
  summarise(prizeAmountAdjusted = sum(prizeAmountAdjusted))

## data for "Genders/Organization" tab
dfGenders <- df %>%
  select(awardYear, prizeAmountAdjusted, gender)

### adding data for years wihout Nobel Prizes  
for (gen in unique(dfGenders$gen)) {
  dfGenders <- rbind(dfGenders, data.frame(awardYear = 1901:2019,
                   prizeAmountAdjusted = rep(0, 119), gender = gen))
}

dfGenders <- dfGenders %>%
  group_by(awardYear, gender) %>% 
  summarise(prizeAmountAdjusted = sum(prizeAmountAdjusted))

### details  
dfGenders$gender[which(dfGenders$gender == "")] <- "Organization"
dfGenders$gender[which(dfGenders$gender == "male")] <- "Male"
dfGenders$gender[which(dfGenders$gender == "female")] <- "Female"

## data for "Categories" tab
dfCategories <- df %>% 
  select(awardYear, prizeAmountAdjusted, category)

### adding data for years wihout Nobel Prizes 
for (cat in unique(dfCategories$category)) {
  dfCategories <- rbind(dfCategories, data.frame(awardYear = 1901:2019,
                   prizeAmountAdjusted = rep(0, 119), category = cat))
}

dfCategories <- dfCategories %>% 
  group_by(awardYear, category) %>% 
  summarise(prizeAmountAdjusted = sum(prizeAmountAdjusted))

## data for "Continents" tab
dfContinents <- df %>% 
  select(awardYear, prizeAmountAdjusted, birth_continent)

### adding data for years wihout Nobel Prizes 
for (con in unique(dfContinents$birth_continent)) {
  dfContinents <- rbind(dfContinents, data.frame(awardYear = 1901:2019,
                   prizeAmountAdjusted = rep(0, 119), birth_continent = con))
}

dfContinents <- dfContinents %>% 
  group_by(awardYear, birth_continent) %>% 
  summarise(prizeAmountAdjusted = sum(prizeAmountAdjusted)) 

dfContinents$birth_continent[which(dfContinents$birth_continent == "")] <- "Organization"

# creating updatemenus
updatemenusList <- list(list(
  active = -1,
  type = "buttons",
  x = -0.15,
  y = 1,
  buttons = list(
    list(method = "update",
         args = list(list(visible = c(F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)), list(showlegend = F)),
         label = "Summary"),
    list(method = "update",
         args = list(list(visible = c(F,F,T,T,T,F,F,F,F,F,F,F,F,F,F,F,F)), list(showlegend = T)),
         showlegend = TRUE,
         legend = list(x = -0.53, y = 0.4),
         label = "Genders/Organization   "),
    list(method = "update",
         args = list(list(visible = c(F,F,F,F,F,T,T,T,T,T,T,F,F,F,F,F,F)), list(showlegend = T)),
         showlegend = T,
         label = "Categories"),
    list(method = "update",
         args = list(list(visible = c(T,F,F,F,F,F,F,F,F,F,F,T,T,T,T,T,T)), list(showlegend = T)),
         showlegend = T,
         label = "Continents")
  )))

# creating plot
fig <- plot_ly(type = 'bar')

fig <- fig %>% add_trace(data = dfGrouped,
                         x = ~awardYear,
                         y = ~prizeAmountAdjusted,
                         name = "Summary",
                         hovertemplate = paste0(paste0("Total prize amount: ", dfGrouped$prizeAmountAdjusted, "<br>",
                                                       "Year: ", dfGrouped$awardYear, "<br>"),
                                                "<extra></extra>")
                         )

fig <- fig %>% add_trace(data = dfGenders,
                         x = ~awardYear,
                         y = ~prizeAmountAdjusted,
                         split = ~gender,
                         name = paste(dfGenders$gender),
                         visible = FALSE,
                         hovertemplate = paste0(paste0("Total prize amount: ", dfGenders$prizeAmountAdjusted, "<br>",
                                                       "Year: ", dfGenders$awardYear, "<br>",
                                                       "Gender/Organization: ", dfGenders$gender),
                                                "<extra></extra>")
                         )

fig <- fig %>% add_trace(data = dfCategories,
                         x = ~awardYear,
                         y = ~prizeAmountAdjusted,
                         split = ~category,
                         name = paste(dfCategories$category),
                         visible = FALSE,
                         hovertemplate = paste0(paste0("Total prize amount: ", dfCategories$prizeAmountAdjusted, "<br>",
                                                       "Year: ", dfCategories$awardYear, "<br>",
                                                       "Category: ", dfCategories$category),
                                                "<extra></extra>")
                         )

fig <- fig %>% add_trace(data = dfContinents,
                         x = ~awardYear,
                         y = ~prizeAmountAdjusted,
                         split = ~birth_continent,
                         name = paste(dfContinents$birth_continent),
                         visible = FALSE,
                         hovertemplate = paste0(paste0("Total prize amount: ", dfContinents$prizeAmountAdjusted, "<br>",
                                                       "Year: ", dfContinents$awardYear, "<br>",
                                                       "Continent: ", dfContinents$birth_continent),
                                                "<extra></extra>")
)

fig <- fig %>% layout(title = "How much money did Nobel Prize winners get each year? ",
                      legend = list(x = -0.5, y = 0.4),
                      showlegend = FALSE,
                      yaxis = list(
                        title = "Total prize amount (SEK)",
                        range = c(0,200000001),
                        tick0 = 0,
                        dtick = 25000000),
                      xaxis = list(title = "Year", range = c(1901,2019)),
                      updatemenus=updatemenusList,
                      barmode = 'stack')


fig <- fig %>% config(displayModeBar = FALSE)

# display plot
fig
