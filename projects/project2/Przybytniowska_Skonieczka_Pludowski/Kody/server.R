library(shiny)
library(plotly)
library(dplyr)

source("./KodyDP/wordcloud_plot.R")
source("./KodyDP/minor_plots.R")
source("./KodyKS/CodesKS.R")
source("./KodyJP/SingleUserWithSomeone.R")
source("./KodyJP/Comparison.R")
source("./source.R")


shinyServer(function(input, output, session) {
  
  output$wordCountPlot <- renderPlot(
    mkCustomWordPlot(
      dfD1DP,
      customWords,
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color1
    )
  )
  
  observeEvent(input$addButton, {
    cat(customWords, "\n")
    customWords[length(customWords) + 1] <- input$customWord
    cat(customWords, "\n")
    updateTextInput(session, "customWord", value = "")
    output$wordCountPlot <- renderPlot(
      mkCustomWordPlot(dfD1DP,
                       customWords,
                       input$yearsSlider[1],
                       input$yearsSlider[2])
    )
  })
  
  output$wordCountPlot2 <- renderPlot(
    mkCustomWordPlot(
      dfD2DP,
      customWords,
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color2
    )
  )
  
  observeEvent(input$addButton2, {
    cat(customWords, "\n")
    customWords[length(customWords) + 1] <- input$customWord2
    cat(customWords, "\n")
    updateTextInput(session, "customWord2", value = "")
    output$wordCountPlot2 <- renderPlot(
      mkCustomWordPlot(dfD2DP,
                       customWords,
                       input$yearsSlider[1],
                       input$yearsSlider[2])
    )
  })
  
  output$wordCountPlot3 <- renderPlot(
    mkCustomWordPlot(
      dfD3DP,
      customWords,
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color3
    )
  )
  
  observeEvent(input$addButton3, {
    cat(customWords, "\n")
    customWords[length(customWords) + 1] <- input$customWord3
    cat(customWords, "\n")
    updateTextInput(session, "customWord3", value = "")
    output$wordCountPlot3 <- renderPlot(
      mkCustomWordPlot(dfD3DP,
                       customWords,
                       input$yearsSlider[1],
                       input$yearsSlider[2])
    )
  })
  
  
  output$wordcloud <- renderWordcloud2(
    mkWordcloud(
      dfD1DP,
      input$wordLengthSlider[1],
      input$wordLengthSlider[2],
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color1))

  output$wordcloud2 <- renderWordcloud2(
    mkWordcloud(
      dfD2DP,
      input$wordLengthSlider2[1],
      input$wordLengthSlider2[2],
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color2))
  
  output$wordcloud3 <- renderWordcloud2(
    mkWordcloud(
      dfD3DP,
      input$wordLengthSlider3[1],
      input$wordLengthSlider3[2],
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color3))
  
  
  
  output$Dcounter <- renderPlot(
    mkXDPlot(
      dfD1DP,
      input$DRadio,
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color1
    )
  )
  
  output$Dcounter2 <- renderPlot(
    mkXDPlot(
      dfD2DP,
      input$DRadio2,
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color2
    )
  )
  
  output$Dcounter3 <- renderPlot(
    mkXDPlot(
      dfD3DP,
      input$DRadio3,
      input$yearsSlider[1],
      input$yearsSlider[2],
      color = color3
    )
  )
  
  
  output$acitivityPlot <- renderPlotly(
    activityPlot2(dfD1KS,
                  input$si1,
                  input$si2,
                  input$si3,
                  input$yearsSlider[1],
                  input$yearsSlider[2]
    )
  )
  
  
  output$acitivityPlot21 <- renderPlotly(
    activityPlot2(dfD2KS,
                  input$si21,
                  input$si22,
                  input$si23,
                  input$yearsSlider[1],
                  input$yearsSlider[2]
    )
  )
  
  
  
  output$acitivityPlot2 <- renderPlotly(
    activityPlot2(dfD1KS,
                  input$si21,
                  input$si22,
                  input$si23,
                  input$yearsSlider[1],
                  input$yearsSlider[2]
    )
  )
  
  output$acitivityPlot3 <- renderPlotly(
    activityPlot2(dfD3KS,
                  input$si31,
                  input$si32,
                  input$si33,
                  input$yearsSlider[1],
                  input$yearsSlider[2]
    )
  )
  
  output$usWithSomeone <- {
    renderPlotly(usWithSomeonePlot(dfD1JP,
                                   as.numeric(input$yearsSlider[1]), 
                                   as.numeric(input$yearsSlider[2]),
                                   color1, colorMain
                                   )
    )}
  
  output$usWithSomeone2 <- {
    renderPlotly(usWithSomeonePlot(dfD2JP,
                                   as.numeric(input$yearsSlider[1]), 
                                   as.numeric(input$yearsSlider[2]),
                                   color2, colorMain
                                   )
    )}
  
  output$usWithSomeone3 <- {
    renderPlotly(usWithSomeonePlot(dfD3JP,
                                   as.numeric(input$yearsSlider[1]), 
                                   as.numeric(input$yearsSlider[2]), 
                                   color3, colorMain)
    )}
  
  
  output$acitivityPlot2D1 <- renderPlotly(
    if(input$OneofUs == "Kacper"){
      ourActivityPlot(dfD1KS2,
                      color1,
                      input$yearsSlider[1],
                      input$yearsSlider[2])
    } else if(input$OneofUs == "Dawid"){
      ourActivityPlot(dfD2KS2,
                      color2,
                      input$yearsSlider[1],
                      input$yearsSlider[2])
    }else{
      ourActivityPlot(dfD3KS2,
                      color3,
                      input$yearsSlider[1],
                      input$yearsSlider[2])
    }
  )
  
  
  output$hourDayMonthActivity <- renderPlotly(
    hourActivity(dfA3,
                 input$timeGroup,
                 input$yearsSlider[1],
                 input$yearsSlider[2])
  )
  
  
  output$dinesity <- renderPlotly(
    densityPlot(dfD1KS2,
                dfD2KS2,
                dfD3KS2,
                as.character(input$yearsSlider[1]),
                as.character(input$yearsSlider[2])
                
    )
  )
  
  output$messagesNumber <- renderText(
    countMessages(dfG)
  )
  
  output$wordsNumber <- renderText(
    countWord(dfG)
  )
  
  output$photosNumber <- renderText(
    countPhotos(dfG)
  )
  
  output$emoji <- renderPlotly(
    EmojiCount(dfD1JP, dfD2JP, dfD3JP, 
               as.numeric(input$yearsSlider[1]), 
               as.numeric(input$yearsSlider[2]), color1, color2, color3)
  )

  output$reactions <- renderPlotly(ReactionsCount(dfD1JP, dfD2JP, dfD3JP,
                                                  as.numeric(input$yearsSlider[1]),
                                                  as.numeric(input$yearsSlider[2]), color1, color2, color3))
  
  
})