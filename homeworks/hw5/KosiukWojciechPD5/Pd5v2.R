

############
library(plotly)
library(dplyr)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)
my_data <- read.csv("C:/Users/wojtek/Desktop/pw/Techniki wizualizacji danych/Pd5/archive/complete.csv")


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- my_data %>%
  select(awardYear,birth_country) %>%
  filter(birth_country %in% tc) %>%
  group_by(birth_country,awardYear) %>%
  count() %>%
  ungroup()


for (country in tc){
  for (m in seq(1901,2019,1)){
    if (any(df$birth_country==country && df$awardYear==m) == F){
      df <- df %>% add_row(birth_country=country,awardYear=m,n=0)
    }
  }
}



df <- df %>%
  arrange(birth_country,awardYear)%>%
  group_by(birth_country)%>%
  mutate(suma=cumsum(n))


fig2 <- df %>%
  accumulate_by(~awardYear) %>%
  plot_ly(
    x = ~awardYear, 
    y = ~suma,
    split = ~birth_country,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines'
  ) 

fig2 <- fig2 %>% layout(
  xaxis = list(
    title = "Award Year"
  ),
  yaxis = list(
    title = "Number of Nobel Prizes"
  ),
  title = "Countries with the most Nobel Prizes",
  images = list(source = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Nobel_logo.svg/1200px-Nobel_logo.svg.png",
       xref = "paper",
       yref = "paper",
       x= 0.1,
       y= 0.95,
       sizex = 0.2,
       sizey = 0.2,
       opacity = 0.6
  )
)

fig2 <- fig2 %>%
  animation_opts(frame = 200) %>%
  animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="black")))
fig2
