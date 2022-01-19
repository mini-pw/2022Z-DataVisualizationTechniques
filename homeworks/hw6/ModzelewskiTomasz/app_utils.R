nobel_slider_time_interval <- function(name, df) {
  tags$div(style = "display: table-cell;",
           
             sliderInput(
               name, 
               "Time interval:", 
               min = min(df$awardYear), 
               max = max(df$awardYear),
               value = c(1969, max(df$awardYear)),
               sep = ""
             )
           
           )
}