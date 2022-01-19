library(plotly)
library(dplyr)

source("nobel_utils.R")


nobel_boxplot <- function(df, year_start = -Inf, year_end = Inf) {
  
  df_temp <- df[(df$awardYear >= year_start) & (df$awardYear <= year_end), ]
  df_temp$age = nobel_age(df_temp)
  
  fig <- plot_ly(type = "box")
  fig <- fig %>% add_trace(y = df_temp$age, name = "All")
  fig <- fig %>% layout(title = paste("Age of Nobel Laureates from", max(year_start, min(df$awardYear)), "to", min(year_end, max(df$awardYear))))
  
  for (category in nobel_categories(df_temp)) {
    fig <- fig %>% add_trace(y = df_temp[df_temp$category == category, ]$age, name = category)
  }
  
  fig
  
}

nobel_barplot <- function(df, year_start = -Inf, year_end = Inf, top_n = 10, category = "All") {
  
  if(is.na(top_n)) top_n <- 10
  
  df_temp <- df[(df$awardYear >= year_start) & (df$awardYear <= year_end), ]
  
  data <- nobel_top_countries(df_temp, category, top_n)
  
  category_title <- paste("(category: ", category, ")", sep="")
  if(category == "All") {
    category_title <- "(all categories)"
  }
  
  fig <- plot_ly(
    x = data$x,
    y = data$y,
    name = category,
    type = "bar"
  )
  
  fig <- fig %>% layout(title = paste("Top", top_n, "countries with the biggest total number of Nobel Prizes from", max(year_start, min(df$awardYear)), "to", min(year_end, max(df$awardYear)), category_title))
  
  fig
  
}