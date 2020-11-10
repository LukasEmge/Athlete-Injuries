#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)

combine_data <- read_rds("combine_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("superhero"),
    navbarPage(
        "NFL Draft Combine Results",
        tabPanel("Testing Correlation",
                 titlePanel("Do results in some tests \ncorrelate with other tests?"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                       h4(strong("Select Tests")),
                       selectInput("test1",
                                     "Test 1",
                                     c("40 Yard Dash" = "x40yd",
                                       "Broad Jump" = "broad_jump",
                                       "3 Cone Drill" = "x3cone",
                                       "Shuttle" = "shuttle",
                                       "Vertical" = "vertical")),
                       selectInput("test2",
                                   "Test 2",
                                   c("40 Yard Dash" = "x40yd",
                                     "Broad Jump" = "broad_jump",
                                     "3 Cone Drill" = "x3cone",
                                     "Shuttle" = "shuttle",
                                     "Vertical" = "vertical"))
                                       
                     ),
                     
                     mainPanel(
                       plotOutput("distPlot1"),
                     )
                     
                   
                 ) 
                 
                 ),
        tabPanel("Tests Affects",
                 titlePanel("How do test results affect draft position?"),
                 sidebarLayout(
                   sidebarPanel(
                     h4(strong("Select Position")),
                     selectInput("position",
                                 "Position",
                                 c("Defensive End" = "DE",
                                   "Defensive Tackle" = "DT",
                                   "Inside Linebacker" = "ILB",
                                   "Outside Linebacker" = "OLB",
                                   "Strong Safety" = "SS",
                                   "Free Safety" = "FS",
                                   "Cornerback" = "CB",
                                   "Quarterback" = "QB",
                                   "Offensive Tackle" = "OT",
                                   "Offensive Guard" = "OG",
                                   "Fullback" = "FB",
                                   "Tight End" = "TE",
                                   "Wide Reciever" = "WR",
                                   "Running Back" = "RB",
                                   "Punter" = "P")),
                     selectInput("test",
                                 "Test",
                                 c("40 Yard Dash" = "x40yd",
                                   "Broad Jump" = "broad_jump",
                                   "3 Cone Drill" = "x3cone",
                                   "Shuttle" = "shuttle",
                                   "Vertical" = "vertical",
                                   "Bench Press" = "bench_reps"))),
                   mainPanel(
                     plotOutput("distPlot2"),
                   )
                   )
                 
                 
                 
                 ),
        tabPanel("Offense"),
        tabPanel("About",
                 titlePanel("About"),
                 br(),
                 h4("About the Data"),
                 p("The dataset I am using was found on kaggle.com. The data 
                   contains all of the results from the 2017 NFL Combine, 
                   as well as draft position. The data set was originally 
                   organized seperately, with an offensive and defensive 
                   dataset. Therefore, I had to manually join the two
                   seperate data sets in one. I would like to eventualy analyze
                   what events have high correlation between increased
                   draft position and what events have a lower correlation
                   with earlier draft position. This is different than my
                   original idea, but I believe easier to analyze given
                   the data already available. This could also have 
                   implications on what aspects of training future NFL
                   prospects should focus on."),
                 br(),
                 h4("About Me"),
                 p("I am a junior at Harvard College. I play quartback for the
                   varsity football team, so this specific data is very 
                   intriguing."),
                 h5("The source code for this Shiny App can be found at my 
                    GitHub", 
                    a("HERE", 
                      href= "https://github.com/LukasEmge/Athlete-Injuries"))
                 
                 
                 )
    ))

  

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$distPlot1 <- renderPlot({
        combine_data %>% 
        select(choice = !!input$test1, choicey = !!input$test2, pos) %>% 
            ggplot(aes(x = choice, y = choicey, color = pos)) + 
            geom_point() + 
            labs(x = input$test1, 
                 y = input$test2,
                 title = paste("Relation of ", input$test1, " and ", input$test2))
        
    })
    
    output$distPlot2 <- renderPlot({
      combine_data %>%
        select(choice = !!input$position, pos, choicey = !!input$test, pick) %>% 
        filter(pos == choice) %>% 
        ggplot(aes(x = choicey, y = pick)) + 
        geom_point() + 
        labs(x = "input$test", 
             y = "Draft Position",
             title = paste("Relation of Draft Position to ", input$test))
      
    })
    
    output$distPlot3 <- renderPlot({
      combine_data %>%
        select(choice = !!input$position2, pos, choicey = !!input$testOff, pick) %>% 
        filter(pos %in% choice) %>% 
        ggplot(aes(x = choicey, y = pick)) + 
        geom_point() + 
        labs(x = "input$testOff", 
             y = "Draft Position",
             title = paste("Relation of Draft Position to ", input$testOff))
      
    })
    
    output$distPlot4 <- renderPlot({
      combine_data %>% 
        ggplot(aes(x = x40yd, y = shuttle, color = pos)) + 
        geom_point() + 
        labs(x = "40 Yard Dash Time (Sec.)", 
             y = "Shuttle (Sec.)",
             title = "Relation of Shuttle to 40 Yard Dash")
      
    })
    
    output$distPlot5 <- renderPlot({
      combine_data %>% 
        ggplot(aes(x = vertical, y = broad_jump, color = pos)) + 
        geom_point() + 
        labs(x = "Vertical (Inches)",
             y = "Broad Jump (Inches)",
             title = "Relation of Broad Jump to Vertical")
      
    })
    
    output$distPlot6 <- renderPlot({
      combine_data %>% 
        ggplot(aes(x = vertical, y = x3cone, color = pos)) + 
        geom_point() + 
        labs(x  = "Vertical (Inches)",
             y = "3 Cone Drill Time (Sec.)",
             title = "Relation of 3 Cone Drill to Vertical")
      
    })
    
    output$distPlot7 <- renderPlot({
      combine_data %>% 
        ggplot(aes(x = vertical, y = shuttle, color = pos)) + 
        geom_point() + 
        labs(x = "Vertical (Inches)",
             y = "Shuttle (Sec.)",
             title = "Relation of Shuttle to Vertical")
      
    })
    
    output$distPlot8 <- renderPlot({
      combine_data %>% 
        ggplot(aes(x = broad_jump, y = x3cone, color = pos)) + 
        geom_point() + 
        labs(x = "Broad Jump (Inches)", 
             y = "3 Cone Drill (Sec.)",
             title = "Relation of 3 Cone Drill to Broad Jump")
      
    })
    
    output$distPlot9 <- renderPlot({
      combine_data %>% 
        ggplot(aes(x = broad_jump, y = shuttle, color = pos)) + 
        geom_point() + 
        labs(x = "Broad Jump (Inches))", 
             y = "Shuttle (Sec.)",
             title = "Relation of Shuttle to Broad Jump")
      
    })
    
    output$distPlot10 <- renderPlot({
      combine_data %>% 
        ggplot(aes(x = x3cone, y = shuttle, color = pos)) + 
        geom_point() + 
        labs(x = "3 Cone Drill (Sec.)", 
             y = "Shuttle (Sec.)",
             title = "Relation of Shuttle to 3 Cone Drill")
      
    })
    
}
# Run the application
shinyApp(ui = ui, server = server)
