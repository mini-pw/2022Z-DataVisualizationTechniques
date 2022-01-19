source("./KodyDP/wordcloud_plot.R")
source("./KodyDP/minor_plots.R")
source("./KodyKS/CodesKS.R")
source("./KodyJP/SingleUserWithSomeone.R")

library(dplyr)

# plot1 - Kacper Skonieczka 
KSBox <- tabPanel(
  title = "When, with who we are most active",
  fluidPage(fluidRow(column(4,
                            selectInput('si1',
                                        shiny::HTML("<p>Select <span style='color: #1E90FF'>friend 1</span></p>"),
                                        choices = sort(unique(dfD1KS$FriendNick)),
                                        selected =  sort(unique(dfD1KS$FriendNick))[1])),
                     column(4, selectInput('si2',
                                           shiny::HTML("<p>Select <span style='color: 	#fa3c4c'>friend 2</span></p>"),
                                           choices = sort(unique(dfD1KS$FriendNick)),
                                           selected =  sort(unique(dfD1KS$FriendNick))[2])),
                     column(4, selectInput('si3',
                                           shiny::HTML("<p>Select <span style='color: #EE82EE'>friend 3</span></p>"),
                                           choices = sort(unique(dfD1KS$FriendNick)),
                                           selected =  sort(unique(dfD1KS$FriendNick))[3]))
  ),
  plotlyOutput("acitivityPlot", height = "418px")
  )
)


# plot2 - Kacper Skonieczka
JPBox <- tabPanel(
  title = "Type of our messages",
  plotlyOutput("usWithSomeone", height = "505px")
)

### TODO:
# panel
bottomleftPanel <- tabBox(
  id = "bottomleft",
  width = NULL,
  KSBox,
  JPBox
)


# wordcloud box

topleftPanel <- box(
  hieght = NULL,
  width = NULL,
  title = "Wordcloud",
  status = "primary",
  column(
    width = 12,
    
    wordcloud2Output(
      "wordcloud"
    ),
    div(style = "height:10px"),
    splitLayout(
      sliderInput(
        "wordLengthSlider",
        label = NULL,
        min = 1,
        max = 20,
        value = c(5, 10)
      ),
      tags$div()
    )
  )
)


# word count bar plot panel (with add-your-own-word feature)

wordCountPanel <- tabPanel(
  title = "Your words",
  plotOutput(
    "wordCountPlot"
  ),
  tags$div(class = "col", textInput(
    "customWord",
    label = "Add word to plot"
  )),
  tags$div(class = "col",
           actionButton("addButton", "Add"))
)


dCounterCheckBox <- radioGroupButtons(
  inputId = "DRadio", label = NULL, 
  choices = c("xd", "xD", "XD", "all"),
  selected = "all",
  justified = TRUE, status = "primary",
  checkIcon = list(
    yes = icon("ok", lib = "glyphicon"),
    no = icon("remove", lib = "glyphicon"))
)

# different types of 'XD' plot panel

xdPanel <- tabPanel(
  title = "Your 'XD'",
  dCounterCheckBox,
  plotOutput(
    "Dcounter", height = "458px"
  )
)

# panel for dCounter and XD plots

toprightPanel <- tabBox(
  id = "plotset",
  title = "Minor plots",
  width = NULL,
  xdPanel,
  wordCountPanel
)

# series of icongraphics

mostPopularWordInfoBox <- valueBox(
  paste("\"", mkMostPopularWord(dfD1DP), "\"", sep = ""),
  "is a word that you used most", width = 12, color = "purple"
)

wordAvgInfoBox <- valueBox(
  paste(round(mkAvgWordsPercentage(dfD1DP), digits = 0)),
  "words in your average message", width = 12, color = "purple"
)

xDInfoBox <- valueBox(
  paste(round(mkTotalXDPercentage(dfD1DP) * 100), "%"),
  "of your messages contain 'XD'", width = 12, color = "purple"
)

messInfobox <- valueBox(
  paste(round(100 - mkTotalMessagesPercentage(dfD3JP, "someone") * 100), "%"),
  "of all messages were sent by you", width = 12, color = "purple"
)

# whole single user page definition

singleUserPage <- tabItem(tabName = "wordcloud",
                          tags$div( 
                            fluidRow(
                                     column( width = 2,
                                             fluidRow(
                                               mostPopularWordInfoBox
                                             ),
                                             fluidRow(
                                               wordAvgInfoBox
                                             ),
                                             fluidRow(
                                               xDInfoBox
                                             ),
                                             fluidRow(
                                               messInfobox
                                             )
                                     ),
                              column( width = 10,
                                      topleftPanel
                              )
                            ),
                            tags$div(
                              column(width = 6,
                                     tags$div(class = "m-5",
                                              bottomleftPanel)),
                              column(width = 6,
                                     tags$div(class = "m-5",
                                              toprightPanel)))
                          ))