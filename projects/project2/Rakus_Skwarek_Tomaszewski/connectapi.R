library(spotifyr)

connect_with_api <- function(id, secret) {
  
  Sys.setenv(SPOTIFY_CLIENT_ID = id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
  
  access_token <- get_spotify_access_token()
}