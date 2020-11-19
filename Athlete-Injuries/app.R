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
                       h4(strong("Select Position and Tests")),
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
                     h4(strong("Select Position and Test")),
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
        tabPanel("Team by Team",
                 titlePanel("How do different teams tend to pick players?"),
                 sidebarLayout(
                   sidebarPanel(
                     h4(strong("Select Position and Team")),
                     selectInput("team",
                                 "Team",
                                 c("Arizona Cardinals" = "Arizona Cardinals",
                                 "Atlanta Falcons" = "Atlanta Falcons",
                                 "Baltimore Ravens" = "Baltimore Ravens",
                                 "Buffalo Bills" = "Buffalo Bills",
                                 "Carolina Panthers" = "Carolina Panthers",
                                 "Chicago Bears" = "Chicago Bears",
                                 "Cincinnati Bengals" = "Cincinnati Bengals",
                                 "Cleveland Browns" = "Cleveland Browns",
                                 "Dallas Cowboys" = "Dallas Cowboys",
                                 "Denver Broncos" = "Denver Broncos",
                                 "Detroit Lions" = "Detroit Lions",
                                 "Green Bay Packers" = "Green Bay Packers",
                                 "Houston Texans" = "Houston Texans",
                                 "Indianapolis Colts" = "Indianapolis Colts",
                                 "Jacksonville Jaguars" = "Jacksonville Jaguars",
                                 "Kansas City Chiefs" = "Kansas City Chiefs",
                                 "Las Vegas Raiders" = "Oakland Raiders",
                                 "Los Angeles Chargers" = "Los Angeles Chargers",
                                 "Los Angeles Rams" = "Los Angeles Rams",
                                 "Miami Dolphins" = "Miami Dolphins",
                                 "Minnesota Vikings" = "Minnesota Vikings",
                                 "New England Patriots" = "New England Patriots",
                                 "New Orleans Saints" = "New Orleans Saints",
                                 "New York Giants" = "New York Giants",
                                 "New York Jets" = "New York Jets",
                                 "Philadelphia Eagles" = "Philadelphia Eagles",
                                 "Pittsburgh Steelers" = "Pittsburgh Steelers",
                                 "San Francisco 49ers" = "San Francisco 49ers",
                                 "Seattle Seahawks" = "Seattle Seahawks",
                                 "Tampa Bay Buccaneers" = "Tampa Bay Buccaneers",
                                 "Tennessee Titans" = "Tennessee Titans",
                                 "Washington Football Team" = "Washington Redskins")),
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
                       plotOutput("distPlot3"),
                       plotOutput("distPlot4"),
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
    ),
)
    

  

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$distPlot1 <- renderPlot({
        combine_data %>% 
        filter(pos == input$position) %>% 
        select(choice = !!input$test1, choicey = !!input$test2, pos) %>% 
            ggplot(aes(x = choice, y = choicey)) + 
            geom_point() + 
        geom_smooth(method = "lm", se = FALSE) +
            labs(x = input$test1, 
                 y = input$test2,
                 title = paste("Relation of ", input$test1, " and ", input$test2))
        
    })
    
    output$distPlot2 <- renderPlot({
      combine_data %>%
        filter(pos == input$position) %>% 
        select(choicey = !!input$test, pick) %>%
        ggplot(aes(x = choicey, y = pick)) + 
        geom_point() + 
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "input$test", 
             y = "Draft Position",
             title = paste("Relation of Draft Position to ", input$test))
      
    })
    
    output$distPlot3 <- renderPlot({
      combine_data %>%
        select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos) %>% 
        filter(team == input$team,
               pos == input$position) %>% 
        pivot_longer(cols = c(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle), 
                     names_to = "test", values_to = "result") %>% 
        group_by(test) %>% 
        summarise(average = mean(result, na.rm = TRUE), .groups = "drop") %>% 
        filter(test == input$test) %>% 
        ggplot(aes(x = test, y = average)) +
        geom_col()
       
     })
    
    output$distPlot4 <- renderPlot({
       combine_data %>% 
        pivot_longer(cols = c(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle), 
                     names_to = "test", values_to = "result") %>% 
        group_by(test) %>% 
        summarise(average = mean(result, na.rm = TRUE), .groups = "drop") %>% 
        filter(test == input$test) %>% 
        ggplot(aes(x = test, y = average)) +
        geom_col()
       
      })
    # 
    # output$distPlot5 <- renderPlot({
    #   combine_data %>% 
    #     ggplot(aes(x = vertical, y = broad_jump, color = pos)) + 
    #     geom_point() + 
    #     labs(x = "Vertical (Inches)",
    #          y = "Broad Jump (Inches)",
    #          title = "Relation of Broad Jump to Vertical")
    #   
    # })
    # 
    # output$distPlot6 <- renderPlot({
    #   combine_data %>% 
    #     ggplot(aes(x = vertical, y = x3cone, color = pos)) + 
    #     geom_point() + 
    #     labs(x  = "Vertical (Inches)",
    #          y = "3 Cone Drill Time (Sec.)",
    #          title = "Relation of 3 Cone Drill to Vertical")
    #   
    # })
    # 
    # output$distPlot7 <- renderPlot({
    #   combine_data %>% 
    #     ggplot(aes(x = vertical, y = shuttle, color = pos)) + 
    #     geom_point() + 
    #     labs(x = "Vertical (Inches)",
    #          y = "Shuttle (Sec.)",
    #          title = "Relation of Shuttle to Vertical")
    #   
    # })
    # 
    # output$distPlot8 <- renderPlot({
    #   combine_data %>% 
    #     ggplot(aes(x = broad_jump, y = x3cone, color = pos)) + 
    #     geom_point() + 
    #     labs(x = "Broad Jump (Inches)", 
    #          y = "3 Cone Drill (Sec.)",
    #          title = "Relation of 3 Cone Drill to Broad Jump")
    #   
    # })
    # 
    # output$distPlot9 <- renderPlot({
    #   combine_data %>% 
    #     ggplot(aes(x = broad_jump, y = shuttle, color = pos)) + 
    #     geom_point() + 
    #     labs(x = "Broad Jump (Inches))", 
    #          y = "Shuttle (Sec.)",
    #          title = "Relation of Shuttle to Broad Jump")
    #   
    # })
    # 
    # output$distPlot10 <- renderPlot({
    #   combine_data %>% 
    #     ggplot(aes(x = x3cone, y = shuttle, color = pos)) + 
    #     geom_point() + 
    #     labs(x = "3 Cone Drill (Sec.)", 
    #          y = "Shuttle (Sec.)",
    #          title = "Relation of Shuttle to 3 Cone Drill")
    #   
    # })
    
}
# Run the application
shinyApp(ui = ui, server = server)
