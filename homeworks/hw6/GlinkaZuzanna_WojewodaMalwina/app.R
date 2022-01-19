library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

nobel <- read.csv("complete.csv")
mixedToFloat <- function(x){
  x <- sub(' ', '+', x, fixed=TRUE)
  return(unlist(lapply(x, function(x) eval(parse(text=x)))))
}
nobel$portion <- mixedToFloat(nobel$portion)
nobel2 <- nobel %>%  pivot_longer(c(affiliation_1, affiliation_2, affiliation_3, affiliation_4),names_to = NULL, values_to = "university") %>% 
    select(awardYear, category, university) %>% filter(university != "")  %>% 
    group_by(university, awardYear) %>% count() %>% ungroup() %>% group_by(university) %>% mutate(all = n()) %>% filter(all >= 8) %>% 
    arrange(-desc(awardYear)) %>% pivot_wider(names_from = "university", values_from = "n") %>% replace(is.na(.), 0) %>% select(!all)
nobel2[2:14] <- cumsum(nobel2[2:14])

colnames(nobel2) <- c("awardYear", "University_of_Cambrige", 
                      "University_of_Chicago", "Harvard_University", "University_of_Oxford",
                      "California_Institute_of_Technology", "Columbia_University",
                      "University_of_California", "Princeton_University",
                      "Stanford_University", "MRC_Laboratory_of_Molecular_Biology", 
                      "Massachusetts_Institute_of_Technology",
                      "Rockefeller_University", "Howard_Hughes_Medical_Institute")


NobelPrize <- nobel %>% filter(ind_or_org == "Individual")
NobelPrize <- NobelPrize %>% mutate(
  Age = as.numeric(NobelPrize$awardYear) - as.numeric(substring(NobelPrize$birth_date, 1, 4)))



ui1 <- fluidPage(
    
    titlePanel("Where Nobel Prize winners worked?"),
    
    fluidRow(
        column(8,
               plotOutput("distPlot1"),
               sliderInput("range",
                           "Range of years:",
                           min = 1906,
                           max = 2019,
                           value = c(1906, 2019), sep = "", 
                           width = "100%")
        ),
        column(width = 4,
               checkboxInput(inputId = "University_of_Cambrige", label = HTML('<div style="color:violet">University of Cambrige</div>')),
               checkboxInput('University_of_Chicago', label = HTML('<div style="color:rgb(152, 51, 216)">University of Chicago</div>')),
               checkboxInput("Harvard_University", label = HTML('<div style="color:rgb(74, 41, 154)">Harvard University</div>')),
               checkboxInput("University_of_Oxford", label = HTML('<div style="color:blue">University of Oxford</div>')),
               checkboxInput("California_Institute_of_Technology", label = HTML('<div style="color:rgb(38, 175, 188)">California Institute of Technology</div>')),
               checkboxInput("Columbia_University", label = HTML('<div style="color:green">Columbia University</div>')),
               checkboxInput("University_of_California", label = HTML('<div style="color:yellowgreen">University of California</div>')),
               checkboxInput("Princeton_University", label = HTML('<div style="color:gold">Princeton University</div>')),
               checkboxInput("Stanford_University", label = HTML('<div style="color:#EDAF2B">Stanford University</div>')), 
               checkboxInput("MRC_Laboratory_of_Molecular_Biology", label = HTML('<div style="color:#ED772B">MRC Laboratory of Molecular Biology</div>')),
               checkboxInput("Massachusetts_Institute_of_Technology", label = HTML('<div style="color:#F3381A">Massachusetts Institute of Technology (MIT)</div>')),
               checkboxInput("Rockefeller_University", label = HTML('<div style="color:#A00629">Rockefeller University</div>')),
               checkboxInput("Howard_Hughes_Medical_Institute", label = HTML('<div style="color:#cc0066">Howard Hughes Medical Institute</div>'))
        )
    )
)

ui2 <- fluidPage(
    
    titlePanel("Age of Nobel Prize laureates at the time of winning the award"),
    
    fluidRow(
        column(2,
               selectInput("category", "Select category",
                           choices = c("All", unique(NobelPrize$category)))
        ),
        column(width = 10,
               plotOutput("distPlot2"), 
               sliderInput("range2",
                           "Range of age:",
                           min = min(NobelPrize$Age),
                           max = max(NobelPrize$Age),
                           value = c(min(NobelPrize$Age), max(NobelPrize$Age)), sep = "", 
                           width = "100%"))
    )
)


server <- function(input, output, session) {
    output$distPlot1 <- renderPlot({
        
        range <- seq(input$range)
        
        by2 = function(x) {
            seq(0, max(x), by = 2)
        }
        
        p <- ggplot(data = nobel2, aes(x = awardYear), range(range)) +
            theme_bw() + 
            scale_y_continuous(name = "Number of aworded", 
                               expand=c(0,0.3), 
                               breaks = by2)+
            scale_x_continuous(limits = c(input$range[1], input$range[2]), 
                               expand = c(0,0))+
            theme(
                panel.grid.major.y = element_line(colour = "gray"),
                panel.grid.minor.y = element_line(colour = "gray", linetype = "dotted"),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12)
            )
        if(input$University_of_Cambrige){
            p <- p + geom_line(aes(x = awardYear, y = University_of_Cambrige), color = "violet", size = 1.5) 
        }
        if(input$University_of_Chicago){
            p <- p + geom_line(aes(x = awardYear, y = University_of_Chicago), color = "#A043DB", size = 1.5)
        }
        if(input$Harvard_University){
          p <- p + geom_line(aes(x = awardYear, y = Harvard_University), color = "#4A299A", size = 1.5)
        }
        if(input$University_of_Oxford){
            p <- p + geom_line(aes(x = awardYear, y = University_of_Oxford), color = "blue", size = 1.5)
        }
        if(input$California_Institute_of_Technology){
            p <- p + geom_line(aes(x = awardYear, y = California_Institute_of_Technology), color = "turquoise2", size = 1.5)
        }
        if(input$Columbia_University){
            p <- p + geom_line(aes(x = awardYear, y = Columbia_University), color = "springgreen4", size = 1.5)
        }
        if(input$University_of_California){
            p <- p + geom_line(aes(x = awardYear, y = University_of_California), color = "yellowgreen", size = 1.5)
        }
        if(input$Princeton_University){
            p <- p + geom_line(aes(x = awardYear, y = Princeton_University), color = "gold", size = 1.5)
        }
        if(input$Stanford_University){
            p <- p + geom_line(aes(x = awardYear, y = Stanford_University), color = "#EDAF2B", size = 1.5)
        }
        if(input$MRC_Laboratory_of_Molecular_Biology){
            p <- p + geom_line(aes(x = awardYear, y = MRC_Laboratory_of_Molecular_Biology),color = "#ED772B", size = 1.5)
        }
        if(input$Massachusetts_Institute_of_Technology){
            p <- p + geom_line(aes(x = awardYear, y = Massachusetts_Institute_of_Technology), color = "#F3381A", size = 1.5)
        }
        if(input$Rockefeller_University){
            p <- p + geom_line(aes(x = awardYear, y = Rockefeller_University), color = "#A00629",size = 1.5)
        }
        if(input$Howard_Hughes_Medical_Institute){
            p <- p + geom_line(aes(x = awardYear, y = Howard_Hughes_Medical_Institute),color = "violetred", size = 1.5)
        }
        p
    }) 
    
    output$distPlot2 <- renderPlot({
        
        data <- if(input$category != "All"){
            NobelPrize %>% filter(category == input$category)
        } else {
            NobelPrize
        }
        
        byone = function(x) {
            seq(0, max(x), by = 1)
        }
        
        ggplot(data) +
            geom_histogram(aes(x=Age), binwidth = 1 , fill="#337DD8", color="#e9ecef", alpha=0.9)+
            theme_bw() +  
            scale_x_continuous(limits = c(input$range2[1]-1, input$range2[2]+1), 
                               breaks = seq(input$range2[1], input$range2[2], by = 1), 
                               expand = c(0,0))+
            ylab("Number of winners")+
            scale_y_continuous(expand=c(0,0.3), breaks = byone)+
            theme(
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12)
            )
    })
}
app_ui <- navbarPage(
    title = "Nobel Prize",
    tabPanel("Chart 1", ui1),
    tabPanel("Chart 2", ui2)
)

shinyApp(app_ui, server)