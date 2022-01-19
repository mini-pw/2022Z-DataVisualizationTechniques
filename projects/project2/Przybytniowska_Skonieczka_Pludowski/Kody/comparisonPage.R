source("./KodyKS/CodesKS.R")

JPBox1 <-  tabPanel(
  title = "Our most sent emoji",
  plotlyOutput("emoji")
)
JPBox2 <-  tabPanel(
  title = "Our most used reactions",
  plotlyOutput("reactions")
)

bottomPanel <- tabBox(
  id = "bottomPanel",
  width = NULL,
  JPBox1,
  JPBox2
)






# plot1 - Kacper Skonieczka 
KSBox <- tabPanel(
  title = "Our activity on messeger",
  fluidPage(column(3,
                     selectInput('OneofUs',
                                 "Select autor:",
                                 choices = c("Kacper",
                                             "Dawid",
                                             "Julia"),
                                 selected = c("Kacper"))),
            fluidRow(style='height:50px'),
  plotlyOutput("acitivityPlot2D1", width = "auto"))
  )

box1 <-  valueBox(1, 
                  "Project", 
                  width = 12,
                  color = "purple")
box2 <-  valueBox(3, "Authors: KS,DP,JP", width = 12, color = "purple")
box3 <-  valueBox(textOutput("messagesNumber"), "Project messages", width = 12, color = "purple")
box4 <-  valueBox(textOutput("wordsNumber"), "Words", width = 12, color = "purple")
box5 <-  valueBox(textOutput("photosNumber"), "Sent photos", width = 12, color = "purple")
box6 <-  valueBox(4, "Voice chats", width = 12, color = "purple")
box7 <-  valueBox(160.6, "Minutes of voice chats", width = 12, color = "purple")
box8 <-  valueBox('100%', "Fun", width = 12, color = "purple")

KSBox3 <-  tabPanel(
  title = "When authors are most active",
  fluidPage(column(3,
                   selectInput('timeGroup',
                               "Select time grouping",
                               choices = c("Hour",
                                           "Day",
                                           "Month"),
                               selected = c("Hour"))),
            fluidRow(style='height:50px'),
            plotlyOutput("hourDayMonthActivity"))
)

KSBox2 <-  tabPanel(
  title = "Dinesity of activity",
  select = TRUE,
  plotlyOutput("dinesity", height = "450px", width = "auto"))




# panel
topPanel <- tabBox(
  id = "topPanel",
  title = "Activity",
  width = NULL,
  KSBox,
  KSBox2,
  KSBox3
)



comparisonPage <- tabItem(tabName = "comparison",
                          splitLayout(
                            fluidPage(fluidRow(column(box1,box2,box3,box4,width = 3),
                                               column(width = 9, topPanel)),
                                      fluidRow(column(width = 9, bottomPanel),
                                               column(width = 3, box5,box6,box7,box8)))
                            
                          )
)

