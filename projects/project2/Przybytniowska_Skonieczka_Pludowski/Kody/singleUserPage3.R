source("./KodyDP/wordcloud_plot.R")
source("./KodyDP/minor_plots.R")
source("./KodyKS/CodesKS.R")
source("./KodyJP/SingleUserWithSomeone.R")

# plot1 - Kacper Skonieczka 
KSBox <- tabPanel(
  title = "When, with who we are most active",
  fluidPage(fluidRow(column(4,
                            selectInput('si31',
                                        shiny::HTML("<p>Select <span style='color: #1E90FF'>friend 1</span></p>"),
                                        choices = sort(unique(dfD3KS$FriendNick)),
                                        selected =  sort(unique(dfD3KS$FriendNick))[1])),
                     column(4, selectInput('si32',
                                           shiny::HTML("<p>Select <span style='color: 	#fa3c4c'>friend 2</span></p>"),
                                           choices = sort(unique(dfD3KS$FriendNick)),
                                           selected =  sort(unique(dfD3KS$FriendNick))[2])),
                     column(4, selectInput('si33',
                                           shiny::HTML("<p>Select <span style='color: #EE82EE'>friend 3</span></p>"),
                                           choices = sort(unique(dfD3KS$FriendNick)),
                                           selected =  sort(unique(dfD3KS$FriendNick))[3]))
  ),
  plotlyOutput("acitivityPlot3", height = "418px")
  )
)


# plot2 - Kacper Skonieczka
JPBox <- tabPanel(
  title = "Type of our messages",
  plotlyOutput("usWithSomeone3", height = "505px")
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
      "wordcloud3"
    ),
    div(style = "height:10px"),
    splitLayout(
      sliderInput(
        "wordLengthSlider3",
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
    "wordCountPlot3"
  ),
  tags$div(class = "col", textInput(
    "customWord3",
    label = "Add word to plot"
  )),
  tags$div(class = "col",
           actionButton("addButton3", "Add"))
)


dCounterCheckBox3 <- radioGroupButtons(
  inputId = "DRadio3", label = NULL, 
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
  dCounterCheckBox3,
  plotOutput(
    "Dcounter3", height = "458px"
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
  paste("\"", mkMostPopularWord(dfD3DP), "\"", sep = ""),
  "is a word that you used most", width = 12, color = "purple"
)

wordAvgInfoBox <- valueBox(
  paste(round(mkAvgWordsPercentage(dfD3DP), digits = 0)),
  "words in your average message", width = 12, color = "purple"
)

xDInfoBox <- valueBox(
  paste(round(mkTotalXDPercentage(dfD3DP) * 100), "%"),
  "of your messages contain 'XD'", width = 12, color = "purple"
)

messInfobox <- valueBox(
  paste(round(100 - mkTotalMessagesPercentage(dfD3JP, "someone") * 100), "%"),
  "of all messages were sent by you", width = 12, color = "purple"
)

# whole single user page definition

singleUserPage3 <- tabItem(
  tags$head(tags$style(HTML('
  .myClass2:after { 
            font-size: 20px;
            line-height: 50px;
            text-align: left;
            font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
            padding: 0 15px;
            overflow: hidden;
            color: black;
            content: "This text replaces the original.";
  }'))),

  tabName = "wordcloud3",
                           tags$div( 
                             fluidRow(
                               tags$div(class = "d-flex align-items-center",
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
                                        )),
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