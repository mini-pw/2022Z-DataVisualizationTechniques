library(plotly)

nobel = read.csv("nobel.csv")

# https://plotly.com/r/cumulative-animations/
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# Filter and prepare the data.
df <- nobel %>% 
  filter(category != "Economic Sciences") %>% 
  group_by(awardYear,category) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(cumsum = cumsum(count))

# Use shared accumulate_by function.
df <- df %>% accumulate_by(~awardYear)

# Create and custom animated plot.
fig <- plot_ly(
  data = df, 
  x = ~awardYear, 
  y = ~cumsum,
  frame = ~frame,
  color = ~category,
  colors = "Set1",
  mode = "lines",
  text = paste0("Year: ", df$awardYear, "<br>Cumsum: ", df$cumsum, "<br>",df$category),
  hoverinfo = 'text'
) %>%
  layout(
    title = "Cumulative sum of Nobel awards by category", 
    xaxis = list(title = 'Year'), 
    yaxis = list(title = 'Cumulative sum'),
    legend = list(
      x = 1, y = 0.7,
      title = list(text = "Legend")
    )
  ) %>% 
  add_lines(x = df$awardYear, y = df$cumsum) %>%
  config(displayModeBar = FALSE) %>% 
  animation_opts(10) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: ", font = list(color="red")))

# Display the plot.
fig
