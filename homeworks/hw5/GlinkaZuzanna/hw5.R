library(plotly)
library(dplyr)
library(tidyr)
nobel <- read.csv("/Users/zuzannaglinka/AppTWD/complete.csv")
head(nobel)

nobel1 <- nobel %>% select("awardYear", "category", "prizeAmount", "gender") %>% 
  group_by(category, gender) %>% filter(gender != "") %>% count() %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate(All = sum(female, male), PercentFemale = female/All*100)


updatemenus <- list(
  list(
    buttons = list(
      list(
        label = "Females and Males",
        method = "update",
        visible = TRUE,
        args = list(
          list(visible = c(TRUE, TRUE)
          ))),
      list(
        label = "Females",
        method = "update",
        visible = TRUE,
        args = list(
          list(visible = c(TRUE, FALSE)
          ))),
      list(
        label = "Males",
        method = "update",
        visible = TRUE,
        args = list(
          list(visible = c(FALSE, TRUE)
          )))
      
      
    )
  )
)


fig <- plot_ly(data = nobel1,
               type = 'bar', barmode = 'stack') %>%
  add_trace(x = ~category, y = ~male, visible = TRUE, range = c(0, 250), color = "male") %>% 
  add_trace(x = ~category, y = ~female, visible = TRUE, color = "female") %>%
  layout(title = "Nobel Winners", showlegend=TRUE,
         xaxis=list(title="Category"),
         yaxis=list(title="Amount of winners"),
         updatemenus=updatemenus, barmode = 'stack')


fig

