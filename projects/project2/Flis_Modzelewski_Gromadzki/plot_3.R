library(tidyr)


top_singers_of_person <- function(df, person = 1, limit = 8) {
  singers <- df$subtitles.0.name[endsWith(df$subtitles.0.name, "VEVO")]
  singers <- gsub("VEVO", "", singers)
  tibble(singers) %>% 
    group_by(singers) %>% 
    count() %>% arrange(-n) %>% 
    head(limit) %>% 
    mutate(person = paste("Person", person, sep = ""))
}

top_singers <- function(..., limit = 8) {
  dataframes <- list(...)
  resultdf <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(resultdf) <- c("singers", "n", "person")
  for (i in c(1:length(dataframes))) {
    resultdf <- rbind(resultdf, top_singers_of_person(dataframes[[i]], i, limit = limit))
  }
  resultdf
}


top_singers_plot <- function(dfperson1, dfperson2, dfperson3, date_start, date_end) {
  df1 <- dfperson1[(dfperson1$time >= date_start) & (dfperson1$time <= date_end), ]
  df2 <- dfperson2[(dfperson2$time >= date_start) & (dfperson2$time <= date_end), ]
  df3 <- dfperson3[(dfperson3$time >= date_start) & (dfperson3$time <= date_end), ]
  ds <- top_singers(df1, df2, df3)
  dw <- ds %>% pivot_wider(names_from = 'person', values_from = 'n', values_fill = 0)
  dw$singers <- factor(dw$singers, ds %>% group_by(singers) %>% summarize(sum = sum(n)) %>% arrange(-sum) %>% pull(singers))
  fig <- plot_ly(data = dw, x = ~singers, y = ~Person3, name = 'Person 3', type = 'bar') %>%
    add_trace(y = ~Person2, name = 'Person 2') %>% 
    add_trace(y = ~Person1, name = 'Person 1') %>% 
    layout(
      title = list(pad = list(b = 90, l = 130, r = 50, t = 200 ), text = paste("Who do we listen to the most? From", date_start, "to", date_end)),
      yaxis = list(title = 'Number of views'), 
      barmode = 'stack'
    )
  fig
}
