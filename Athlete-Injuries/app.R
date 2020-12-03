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
    theme = shinytheme("united"),
    navbarPage(
        "NFL Draft Combine Results",
        tabPanel("Testing Correlation",
                 titlePanel("Do results in some tests \ncorrelate with other tests?"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                       h4(strong("Select Position and Tests")),
                       selectInput("position1",
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
                       plotOutput("distPlot1")
                     )
                     
                   
                 ) 
                 
                 ),
        tabPanel("Tests Affects",
                 titlePanel("How do test results affect draft position?"),
                 sidebarLayout(
                   sidebarPanel(
                     h4(strong("Select Position and Test")),
                     selectInput("position2",
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
                     selectInput("test3",
                                 "Test",
                                 c("40 Yard Dash" = "x40yd",
                                   "Broad Jump" = "broad_jump",
                                   "3 Cone Drill" = "x3cone",
                                   "Shuttle" = "shuttle",
                                   "Vertical" = "vertical",
                                   "Bench Press" = "bench_reps"))),
                   mainPanel(
                     plotOutput("distPlot2")
                   )
                   )
                 
                 
                 
                 ),
        tabPanel("Team by Team",
                 titlePanel("How do different teams tend to pick players?"),
                 sidebarLayout(
                   sidebarPanel(
                     h4(strong("Select Position and Team")),
                     selectInput("team1",
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
                     selectInput("position3",
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
                     selectInput("test4",
                                 "Test",
                                 c("40 Yard Dash" = "x40yd",
                                   "Broad Jump" = "broad_jump",
                                   "3 Cone Drill" = "x3cone",
                                   "Shuttle" = "shuttle",
                                   "Vertical" = "vertical",
                                   "Bench Press" = "bench_reps"))),
                     mainPanel(
                       plotOutput("distPlot3"),
                       plotOutput("distPlot4")
                     )
                   )
                 ),
        
        tabPanel("Tests Affect - stan_glm",
                 titlePanel("How do test results affect draft position?"),
                 sidebarLayout(
                   sidebarPanel(
                     h4(strong("Select Position and Test")),
                     selectInput("position4",
                                 "Position",
                                 c("Defensive End" = "DE",
                                   "Defensive Tackle" = "DT",
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
                                   "Punter" = "P"))),
                   mainPanel(
                     plotOutput("distPlot5")
                   )
                   )
        ),
        
        
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
    )
)
    

  

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$distPlot1 <- renderPlot({
        combine_data %>% 
        filter(pos == input$position1) %>% 
        select(choice = !!input$test1, choicey = !!input$test2, pos) %>% 
            ggplot(aes(x = choice, y = choicey)) + 
            geom_point() + 
        geom_smooth(method = "lm", se = FALSE) +
            labs(x = input$test1, 
                 y = input$test2,
                 title = paste("Relation of ", input$test1, " and ", input$test2))
      
     # combine_data %>% 
      #  filter(pos == input$position1) -> correlationdata
      
      #combine_data %>% 
       # cor(.$x40yd, .$broad_jump, method = "spearman") -> corr
      
    #  print(corr)
      
  
        
    })
    
    output$distPlot2 <- renderPlot({
      combine_data %>%
        filter(pos == input$position2) %>% 
        select(choicey = !!input$test3, pick, pos) %>%
        ggplot(aes(x = choicey, y = pick)) + 
        geom_point() + 
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = input$test3, 
             y = "Draft Position",
             title = paste("Relation of Draft Position to ", input$test))
      
    })
    
    output$distPlot3 <- renderPlot({
      combine_data %>% 
        filter(pos == input$position3) %>% 
        pivot_longer(cols = c(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle), 
                     names_to = "test", values_to = "result") %>% 
        group_by(test) %>% 
        summarise(nfl_average = mean(result, na.rm = TRUE), .groups = "drop") %>% 
        filter(test == input$test4) -> nfl_average
      
      
      combine_data %>% 
        select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos) %>% 
        filter(team == input$team1,
               pos == input$position3) %>% 
        pivot_longer(cols = c(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle), 
                     names_to = "test", values_to = "result") %>% 
        group_by(test) %>% 
        summarise(average = mean(result, na.rm = TRUE), .groups = "drop") %>% 
        filter(test == input$test4) %>% 
        full_join(nfl_average) %>% 
        pivot_longer(cols = c(average, nfl_average),
                     names_to = "team_or_nfl",
                     values_to = "result") %>% 
        ggplot(aes(x = team_or_nfl, y = result)) +
        geom_col()
       
     })
    
   # output$distPlot4 <- renderPlot({
    #   combine_data %>% 
     #   pivot_longer(cols = c(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle), 
      #               names_to = "test", values_to = "result") %>% 
       # group_by(test) %>% 
        #summarise(average = mean(result, na.rm = TRUE), .groups = "drop") %>% 
        #filter(test == input$test4) %>% 
        #ggplot(aes(x = test, y = average)) +
        #geom_col()
       
      #})
     
     output$distPlot5 <- renderPlot({
       
      combine_data_stan <- combine_data %>% 
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos, pick) %>% 
         filter(pos == input$position4)
       
      fit <- stan_glm(data = combine_data_stan,
                        formula = pick ~ x40yd + vertical + bench_reps + broad_jump + x3cone + shuttle,
                        family = gaussian(),
                        refresh = 0)
      posterior <- as.array(fit)
      dim(posterior)
      dimnames(posterior)
      color_scheme_set("red")
      mcmc_areas(posterior, par = c("x40yd", "vertical", "bench_reps", "broad_jump", "x3cone", "shuttle"))
       
     })
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
