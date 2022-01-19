library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(wordcloud2)

source("source.R")
source("singleUserPage.R")
source("singleUserPage3.R")
source("singleUserPage2.R")
source("comparisonPage.R")

library(fresh)
myTheme <- create_theme(
  adminlte_color(
    light_blue = "#8A2BE2",
    blue = "#8A2BE2",
    navy = "#8A2BE2",
    aqua = "#8A2BE2"),
  adminlte_sidebar(
    dark_bg = "#FFFFFF",
    dark_hover_bg = "#E6E6E6",
    dark_color = "#000000"
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Comparison & sumarry", icon = icon("balance-scale-right"),
             tabName = "comparison",selected = T),
    menuItem("Kacper", icon = icon("user-tie"), tabName = "wordcloud"),
    menuItem("Dawid", icon = icon("user-tie"), tabName = "wordcloud2"),
    menuItem("Julia", icon = icon("user-tie"), tabName = "wordcloud3"),
    sliderInput(
      "yearsSlider",
      label = NULL,
      min = 2014,
      max = 2021,
      sep = "",
      value = c(2015, 2021)
    )
  )
)




body <- dashboardBody(
  use_theme(myTheme),

  tags$head(tags$style(HTML('
         /* logo */
        .skin-blue .main-header .logo {
                              background-color: #FFFFFF;
                              color: #8A2BE2;
                              border-right: 0.5px solid #8A2BE2;
        }
         /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
          border-bottom:0.5px solid #8A2BE2;
          background-color: #FFFFFF;
        } 
        
        
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #FFFFFF;
        }
        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              color: #000000;
  
         }
         /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              color: #000000;
                              border-right:3.5px solid #8A2BE2;
                           
        }
          /* toggle button */               
         .skin-blue .main-header .navbar .sidebar-toggle {
                              background-color: #FFFFFF;
                              color: #8A2BE2;
                              content: "\\f0c7";
         }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              color: #8A2BE2;
                              background-color: #FFFFFF;
         }
       /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar-text{
                              color: #8A2BE2;
                              background-color: #FFFFFF;
                              }
        
        /* main sidebar */
        .skin-blue .main-sidebar {
                               border-right:0.5px solid #8A2BE2;
        }
           /* body */
          .content-wrapper, .right-side {
          background-color: #F6F6F6;
          }

                              
             
             #sliders                  
            .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  border-top: 0px solid #000039 ;
                                                  border-bottom: 0px solid #000039 ;}
            .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
            border-top: 0px solid #000039 ;
            border-bottom: 0px solid #000039 ;}
            .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {
            border-top: 0px solid #000039 ;
            border-bottom: 0px solid #000039 ;}
                        .js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {
            border-top: 0px solid #000039 ;
            border-bottom: 0px solid #000039 ;}
            
            /* active button in Your XD box - change bg-color */
            .btn-group>.btn:hover, .btn-group>.btn:focus, .btn-group>.btn:active,
            .btn-group>.btn.active, .btn-group-vertical>.btn:hover,
            .btn-group-vertical>.btn:focus, .btn-group-vertical>.btn:active,
            .btn-group-vertical>.btn.active {
            z-index: 2;
            background-color: #4B1180;
            border-color: #4B1180}
}}'))),
  
  chooseSliderSkin(color = '#8A2BE2'),
  tags$style(".small-box.bg-purple { background-color: #D5D5D5 !important;}"),
  tags$style(".small-box.bg-purple { color: #000000 !important;}"),
  tags$script(HTML("$('body').addClass('fixed');")),
  tabItems(
    comparisonPage
    ,singleUserPage
    ,singleUserPage2
    ,singleUserPage3

  )
)


shinyUI(
  dashboardPage(
  dashboardHeader(title = "Messenger summary"),
  sidebar,
  body
))