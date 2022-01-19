library(rjson)

# podaj sciezke do StreamingHistory0.json
read_json <- function(file_path) {
  json_data <- fromJSON(file=file_path)
  json_data <- lapply(json_data, function(x) {
    unlist(x)
  })
  as.data.frame(do.call("rbind", json_data))
}

# podaj JSON z poprzedniej funkcji
fix_streaming <- function(streaming_history_df) {
  streaming_history_df %>% 
    mutate(msPlayed = as.numeric(msPlayed)) %>% 
    tidyr::separate(endTime, c("year", "month", "day", "hour", "minute"),
                    "[: -]", remove=FALSE) %>% 
    mutate(
      year = as.numeric(year),
      month = as.numeric(month),
      day = as.numeric(day),
      hour = as.numeric(hour),
      minute = as.numeric(minute)
    )
  
}