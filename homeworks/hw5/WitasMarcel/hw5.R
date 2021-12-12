
library(dplyr)
library(plotly)
library(forcats)

nobel <- read.csv("complete.csv")

dane <- nobel %>% 
  filter(birth_continent != "") %>% 
  mutate(birthCont = as.factor(birth_continent)) %>% 
  group_by(category, birthCont) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(birthCont = fct_reorder(birthCont, n, .desc = TRUE))




dane <- dane %>% 
  group_by(category) %>%
  mutate(prop = n/sum(n))
#------------------------------------------------------------------




#--------------------------------------------------------------------------------

updatemenus <- list(
  list(
    buttons = list(
      list(
        label = "Number",
        method = "update",
        args = list(
          list(visible = c(F, T, T, T, T, T, T, F, F, F, F, F)
               ), 
          list(yaxis.tickformat = '.0f')
          )),
      list(
        label = "Share",
        method = "update",
        args = list(
          list(visible = c(T, F, F, F, F, F, F, T, T, T, T, T)
          ), 
          list(yaxis.tickformat = '.2%')))
      
      
      
    )
  )
)

#--------------------------------------------------------------------------------

fig <- plot_ly(data = dane,
               type = 'bar',
               colors = "Set1") %>%
  add_trace(x = ~category, 
            y = ~n, 
            visible = T,
            color = ~birthCont
            ) %>% 
  add_trace(x = ~category, 
            y = ~prop, 
            visible = F,
            color = ~birthCont) %>% 
  layout(barmode = 'stack',
         updatemenus=updatemenus,
         title = 'Nobel Prize laureates per Continent of birth',
         xaxis = list(title = 'Category'),
         yaxis = list(title = ''))
fig

