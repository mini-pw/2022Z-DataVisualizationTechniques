
library(dplyr)
library(plotly)

Nobel_df<-read.csv("complete.csv")


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

tmp<-Nobel_df %>% 
  filter(category=="Chemistry") 

tmp<- tmp[!duplicated(tmp$awardYear),]
tmp<-tmp %>% accumulate_by(~awardYear)
fig<- tmp %>% arrange(awardYear)

fig<-fig %>% plot_ly(x = ~awardYear, y = ~prizeAmountAdjusted, frame=~frame, type = 'scatter', mode = ' line' )%>% 
  layout(
  xaxis=list(title="Rok"),
  yaxis=list(title="Liczba koron"),
  title=list(text = paste0('Wysokoœæ nagrody w przliczeniu na dzisiejsze korony',
                                     '<br>',
                                     '<sup>',
                                     'Nagroda Nobla z chemii 1901-2019','</sup>'))
  
)

fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = F
)

fig <- fig %>% animation_slider(
  currentvalue = list(prefix = "Rok: ")
)

fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)


fig
