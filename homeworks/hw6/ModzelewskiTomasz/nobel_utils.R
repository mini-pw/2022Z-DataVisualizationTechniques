nobel_birth_year <- function(df) {
  as.integer(gsub("-.*", "", df$birth_date))
}

nobel_age <- function(df) {
  df$awardYear - nobel_birth_year(df)
}

nobel_categories <- function(df) {
  sort(unique(df$category))
}

nobel_top_countries <- function(df, category = "All", top_n = 10) {
  df_temp <- df[df$birth_country != "", ]
  if(category != "All") {
    df_temp <- df[df$category == category, ]
  }
  result <- as.data.frame(sort(table(df_temp$birth_country), decreasing = TRUE))[1:max(top_n, 1),]
  if(is.vector(result)) {
    result <- data.frame()
  } else {
    colnames(result) <- c("x", "y")
    result <- na.omit(result)
  }
  result
}