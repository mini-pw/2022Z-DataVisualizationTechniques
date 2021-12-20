#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(shinycssloaders)

df2 <- read.csv("complete.csv")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot <- renderPlotly({
    years <- input$bins

    temp <-
      df2 %>%
      select(awardYear, gender) %>%
      filter(gender != "") %>%
      group_by(awardYear, gender) %>%
      count() %>%
      mutate(awardDecade = awardYear - awardYear %% years) %>%
      group_by(awardDecade, gender) %>%
      count()

    males <-
      temp %>%
      filter(gender == "male") %>%
      rename(NumberOfMales = n)

    females <-
      temp %>%
      filter(gender == "female") %>%
      rename(NumberOfFemales = n)

    both <-
      males %>%
      left_join(females, by = c("awardDecade")) %>%
      mutate(
        percentageOfFemales = ifelse(
          is.na(NumberOfFemales) == FALSE, NumberOfFemales / (NumberOfMales + NumberOfFemales) * 100, 0
        )
      ) %>%
      mutate(
        percentageOfMales = ifelse(
          is.na(NumberOfFemales) == FALSE, NumberOfMales / (NumberOfMales + NumberOfFemales) * 100, 100
        )
      ) %>%
      replace_na(list(gender.y = "female", NumberOfFemales = 0)) %>%
      rename(percentage_female = percentageOfFemales) %>%
      rename(percentage_male = percentageOfMales) %>%
      select(-NumberOfMales, -NumberOfFemales, -gender.x, -gender.y)


    bothLong <-
      both %>%
      pivot_longer(!awardDecade, names_to = c("useless", "gender"), names_sep = "_", values_to = "percent") %>%
      select(-useless)


    p <-
      ggplot(data = bothLong, aes(x = awardDecade, y = percent, fill = gender)) +
      geom_col() +
      
      labs(
          # title = "Plot1: Contriubution of Men and Women", 
          # subtitle = "in Nobel Prizes each decade", 
          x = "Start Year of Period",
          y = "Contribution [%]" 
          ) +
      scale_x_continuous(breaks = seq(min(temp$awardDecade), max(temp$awardDecade), by = years)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(plot.title = element_text(size = 20))

    if (years < 15) {
      p <-
        p + theme(axis.text.x = element_text(angle = 270))
    }
    if (years < 5) {
      p <-
        p + theme(axis.text.x = element_text(angle = 270)) +
        scale_x_continuous(breaks = seq(min(temp$awardDecade), max(temp$awardDecade), by = years * 2))
    }
    if (years == 1) {
      p <-
        p + theme(axis.text.x = element_text(angle = 270)) +
        scale_x_continuous(breaks = seq(min(temp$awardDecade), max(temp$awardDecade), by = years * 5))
    }




    plotly::ggplotly(p)
  })

  output$distPlot2 <- renderPlotly({
    cat <- input$category

    filtered <-
      df2 %>%
      filter(birth_continent != "")

    if (cat != "All categories") {
      filtered <-
        df2 %>%
        filter(category == cat) %>%
        filter(birth_continent != "")
    }

    p <-
      ggplot(data = filtered, aes(x = birth_continent, y = prizeAmountAdjusted)) +
      geom_violin() +
      labs(
        #title = "Plot2: Prize amout distribution",
       # subtitle = "adjusted for inflation in each category per continent",
        x = "Continent",
        y = "Prize [USD]"
      ) +
      theme(plot.title = element_text(size = 20))

    plotly::ggplotly(p)
  })
})
