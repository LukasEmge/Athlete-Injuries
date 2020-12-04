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
library(gtsummary)
library(gt)
library(broom.mixed)

combine_data <- read_rds("combine_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage(
        "NFL Draft Combine Results",
        tabPanel("Testing Correlation",
                 titlePanel("Do results in some tests \ncorrelate with other tests?"),
                 
                 # In this section, you will be able to analyze the correlation
                 # between different events that take place at the NFL Combine.
                 # For example, does running a fast 40 yard dash mean that a
                 # player will also likely have a good broad jump or vertical jump?
                 # The more we can contextualize the results of the tests done 
                 # at the NFL Combine, the better we can understand their affects.
                 
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
                       
                       # This slider allows you to select a specific position
                       # to analyze.
                       
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
                       
                       
                       # These 2 sliders allow you to pick which events you 
                       # would like to see the correlation between.
                                       
                     ),
                     
                     mainPanel(
                       plotOutput("distPlot1")
                     )
                     
                   
                 ) 
                 
                 ),
        tabPanel("Tests Effects",
                 titlePanel("How do test results affect draft position?"),
                 
                 # This is probably the most informative graph I have so far.
                 # This graph allows you to filter by a specific position, and then
                 # look at how the results of different events at each position affects
                 # their draft position. Does a good 40 yard dash time mean more or
                 # less than a good broad jump? This graph helps us look at that.
                 
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
                     
                     # Again, this slider is allowing the ability to select the
                     # position that you would like to analyze.
                     
                     selectInput("test3",
                                 "Test",
                                 c("40 Yard Dash" = "x40yd",
                                   "Broad Jump" = "broad_jump",
                                   "3 Cone Drill" = "x3cone",
                                   "Shuttle" = "shuttle",
                                   "Vertical" = "vertical",
                                   "Bench Press" = "bench_reps"))),
                   
                   # This slider allows you to select which event you want to 
                   # look at and analyze.
                   
                   mainPanel(
                     plotOutput("distPlot2")
                   )
                   )
                 
                 
                 
                 ),
        tabPanel("Team by Team",
                 titlePanel("How do different teams tend to pick players?"),
                 
                 # This graph will plot NFL Team's tendencies to prioritize 
                 # different events for different positions by plotting the results
                 # for the players they draft at different positions with the 
                 # NFL Average at these positions.
                 
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
                     
                     # This slider allows you to select which team you would like
                     # to look at.
                     
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
                     
                     # This slider allows you to select the position you would
                     # like to analyze.
                     
                     selectInput("test4",
                                 "Test",
                                 c("40 Yard Dash" = "x40yd",
                                   "Broad Jump" = "broad_jump",
                                   "3 Cone Drill" = "x3cone",
                                   "Shuttle" = "shuttle",
                                   "Vertical" = "vertical",
                                   "Bench Press" = "bench_reps"))),
                   
                   # This slider allows you to select the event you want to
                   # analyze.
                   
                     mainPanel(
                       plotOutput("distPlot3"),
                       plotOutput("distPlot4")
                     )
                   )
                 ),
        
        tabPanel("Model",
                 titlePanel("How do test results affect draft position?"),
                 
                 # This will run the model to tell us how different events 
                 # affect draft position for the specific positions in the NFL.
                 
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
                     gt_output("table"),
                     gt_output("table2"),
                     gt_output("table3"),
                     gt_output("table4"),
                     gt_output("table5"),
                     gt_output("table6")
                   )
                   )
        ),
        
        
        tabPanel("About",
                 titlePanel("About"),
                 br(),
                 h1("NFL Combine Background"),
                 p("Every year in February, the country's top college football players attend the
                   NFL Combine, where players are tested in different events such as the
                   40 yard dash, broad jump, vertical jump, and more. NFL teams send scouts to
                   evaluate players, and in April the NFL Draft occurs, where NFL teams take
                   turns drafting players in 7 rounds. Players train for months and even
                   years, often paying trainers large amounts of money in hopes of maximizing
                   their performance at the combine, and in turn maximize their draft position. 
                   But the question that not many people ask is, do the results of the NFL 
                   Combine really affect a players draft position? Furthermore, what events
                   have the largest affect at each position? Do different teams tend to 
                   prioritize different events more than others? In reality, does the NFL
                   Combine really even matter all that much?"),
                 br(),
                 h1("About the Data"),
                    p("The dataset was found on kaggle.com. The datasets were originally
                      seperated by year, from 2013 to 2017, and by offense vs. defense. I therefore
                      had to combine each seperate combine year and the two sides of the 
                      ball. Since the data covers a 5 year span, the dataset contains
                      1027 observations."),
                 h1("About Me"),
                 p("I am a junior at Harvard College. I am a chemistry concentrator with
                 plans to enter the medical field. I play quartback for the,
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
        drop_na(x40yd) %>% 
        drop_na(vertical) %>% 
        drop_na(bench_reps) %>% 
        drop_na(broad_jump) %>% 
        drop_na(x3cone) %>% 
        drop_na(shuttle) %>% 
        select(x = !!input$test1, y = !!input$test2, pos)-> correlationdata
      
      # Here I am filtering my data by the specific position that the viewer selects.
      # I am also running drop_na on all columns, and selecting the two tests that
      # the viewer chooses, assigning them to x and y. This allows me to 
      # check the correlation.
 
      cor(correlationdata$x, correlationdata$y, method = "spearman") %>% 
        round(3) -> corr
      
      # It took a lot of time for me to figure out how to plot the correlation of
      # the different tests on my graph. I finally decided to fun the correlation between
      # the two tests that are selected and then place the correlation in the subtitle.
      
      combine_data %>% 
        filter(pos == input$position1) %>% 
        select(choice = !!input$test1, choicey = !!input$test2, pos) %>% 
            ggplot(aes(x = choice, y = choicey)) + 
            geom_point() + 
        geom_smooth(method = "lm", se = FALSE) +
            labs(x = input$test1, 
                 y = input$test2,
                 title = paste("Relation of ", input$test1, " and ", input$test2),
                 subtitle = paste("r = ", corr)) +
        theme_minimal() 
      
      # Here, I filtered the position to the position that the user selected. I then
      # selected the tests they selected, and assigned them to choice and choicey. I
      # also selected pos column. I graphed the selected tests against each other, 
      # using geom_point and geom_smooth. I had to use select to assign the 
      # chosen tests to different names so that I could use the slider to change the 
      # ggplot.
     
      
  
        
    })
    
    output$distPlot2 <- renderPlot({
     
      combine_data %>% 
        filter(pos == input$position2) %>% 
        drop_na(x40yd) %>% 
        drop_na(vertical) %>% 
        drop_na(bench_reps) %>% 
        drop_na(broad_jump) %>% 
        drop_na(x3cone) %>% 
        drop_na(shuttle) %>% 
        select(x = !!input$test3, pick, pos)-> correlationdata2
      
      cor(correlationdata2$x, correlationdata2$pick, method = "spearman") %>% 
        round(3) -> corr2
      
      # Above, I did the same steps to find the correlation so that I could place 
      # it in the subtitle again.
      
       combine_data %>%
        filter(pos == input$position2) %>% 
        select(choicey = !!input$test3, pick, pos) %>%
        ggplot(aes(x = choicey, y = pick)) + 
        geom_point() + 
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = input$test3, 
             y = "Draft Position",
             title = paste("Relation of Draft Position to ", input$test3),
             subtitle = paste("r = ", corr2)) +
        theme_minimal()
       
       # I first filtered by the position that was selected, and then followed 
       # basically the sames steps as the graph above.
      
    })
    
    output$distPlot3 <- renderPlot({
      combine_data %>% 
        drop_na(x40yd) %>% 
        drop_na(vertical) %>% 
        drop_na(bench_reps) %>% 
        drop_na(broad_jump) %>% 
        drop_na(x3cone) %>% 
        drop_na(shuttle) %>% 
        filter(team == input$team1,
               pos == input$position3) %>% 
        select(event = input$test4, pos, team) -> comparison_data
      
      comparison_data %>% 
        summarise(event = mean(event)) %>% 
        pull() %>% 
        round(4) -> teamaverage
      
      # The above code was done so that I could calculate the average value
      # for each test for specific teams and positions. I used drop_na to get
      # rid of any NAs, and I filtered by team and position selected in the slider. 
      # I used select to rename the test4 slider to event so that I could take 
      # the mean of it in the summarise. Using input$test4 in summarise gave problems.
      # I assigned this value to teamaverage. This was done so that I could include 
      # this value in the subtitle.
      
      combine_data %>% 
        drop_na(x40yd) %>% 
        drop_na(vertical) %>% 
        drop_na(bench_reps) %>% 
        drop_na(broad_jump) %>% 
        drop_na(x3cone) %>% 
        drop_na(shuttle) %>% 
        filter(pos == input$position3) %>% 
        select(event2 = input$test4, pos) -> comparison_data_NFL
      
      comparison_data_NFL %>% 
        summarise(event2 = mean(event2)) %>% 
        pull() %>% 
        round(4) -> nflaverage
      
      # The same steps as above were followed, but I just did not filter by team.
      # This allowed me to find the NFL Average for each test at each position.
      # I named this to nflaverage so that I could add this to my subtitle.
      
      combine_data %>% 
        filter(pos == input$position3) %>% 
        pivot_longer(cols = c(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle), 
                     names_to = "test", values_to = "result") %>% 
        group_by(test) %>% 
        summarise(`NFL Average` = mean(result, na.rm = TRUE), .groups = "drop") %>% 
        filter(test == input$test4) -> nfl_average
      
      # Above, I filtered by the position that was select. I then pivoted my data
      # so that I had a test column with the different tests and a result column
      # with the results of the test. I then grouped by test and summarised to find
      # the mean of each different test. I then filtered by the selected test. What this
      # allowed me to do was get a value in a tibble for the average result of a 
      # test for a specific position accross the NFL.
      
      
      combine_data %>% 
        select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos) %>% 
        filter(team == input$team1,
               pos == input$position3) %>% 
        pivot_longer(cols = c(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle), 
                     names_to = "test", values_to = "result") %>% 
        group_by(test) %>% 
        summarise(`Average for Team` = mean(result, na.rm = TRUE), .groups = "drop") %>% 
        filter(test == input$test4) %>% 
        full_join(nfl_average) %>% 
        pivot_longer(cols = c(`Average for Team`, `NFL Average`),
                     names_to = "Team vs. NFL",
                     values_to = "Result") %>% 
        ggplot(aes(x = `Team vs. NFL`, y = Result, fill = `Team vs. NFL`)) +
        geom_col() +
        labs(subtitle = paste("Team Average = ", teamaverage, " NFL Average = ", nflaverage)) +
        scale_fill_manual(values = c("blue", "red")) +
        theme_minimal()
      
      # I did the same steps as above first, but I preceeded the pivot_longer with
      # a filter for selected team. This allowed me to get a tibble with the average
      # result of a specific test for a specific position and team. I then joined this tibble
      # with the whole NFL one I just created. I pivoted this joined data. This gave me
      # a tibble with a column named Team vs. NFL that told me whether the result was for
      # a specific team or across the NFL, and a second column that gave me the average result
      # for a selected test. I then graphed these two values against each other so that
      # their difference could be visualized.
       
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
     
     output$table <- render_gt({
       
      combine_data_stan <- combine_data %>% 
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos, pick) %>% 
         filter(pos == input$position4)
      
      set.seed(5)
      fit_40 <- stan_glm(data = combine_data_stan,
               formula = pick ~ x40yd - 1,
               family = gaussian(),
               refresh = 0)
     
       
     tbl_regression(fit_40, intercept = FALSE) %>% 
       as_gt()
     
     # I ran a model with pick dependent on 40 yard dash to see the affect that
     # 40 yard dash had on draft position. I originally ran into a problem because I
     # tried to run the model with all of the events in the formula. However, the
     # different tests are correlated to eachother and therefore gave inaccurate values.
     # Instead, I ran the model on each event seperately and plotted the tables 
     # individually, which can be seen below.
      
      #fit <- stan_glm(data = combine_data_stan,
       #               formula = pick ~ x40yd + vertical + bench_reps + broad_jump + x3cone + shuttle - 1,
        #              family = gaussian(),
         #             refresh = 0)
      
       #tbl_regression(fit, intercept = FALSE) %>% 
        # as_gt()
     })
     
     output$table2 <- render_gt({
       
       combine_data_stan <- combine_data %>% 
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos, pick) %>% 
         filter(pos == input$position4)
       
       set.seed(6)
       fit_vert <- stan_glm(data = combine_data_stan,
                            formula = pick ~ vertical - 1,
                            family = gaussian(),
                            refresh = 0)
      
       tbl_regression(fit_vert, intercept = FALSE) %>% 
         as_gt() })
     
     output$table3 <- render_gt({
       
       combine_data_stan <- combine_data %>% 
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos, pick) %>% 
         filter(pos == input$position4)
       
       set.seed(4)
       fit_bench <- stan_glm(data = combine_data_stan,
                            formula = pick ~ bench_reps - 1,
                            family = gaussian(),
                            refresh = 0)
       
       tbl_regression(fit_bench, intercept = FALSE) %>% 
         as_gt() })
     
     output$table4 <- render_gt({
       
       combine_data_stan <- combine_data %>% 
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos, pick) %>% 
         filter(pos == input$position4)
       
       set.seed(5)
       fit_broad <- stan_glm(data = combine_data_stan,
                            formula = pick ~ broad_jump - 1,
                            family = gaussian(),
                            refresh = 0)
       
       tbl_regression(fit_broad, intercept = FALSE) %>% 
         as_gt() })
     
     output$table5 <- render_gt({
       
       combine_data_stan <- combine_data %>% 
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos, pick) %>% 
         filter(pos == input$position4)
       
       set.seed(11)
       fit_3cone <- stan_glm(data = combine_data_stan,
                            formula = pick ~ x3cone - 1,
                            family = gaussian(),
                            refresh = 0)
       
       tbl_regression(fit_3cone, intercept = FALSE) %>% 
         as_gt() })
     
     output$table6 <- render_gt({
       
       combine_data_stan <- combine_data %>% 
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, team, pos, pick) %>% 
         filter(pos == input$position4)
       
       set.seed(12)
       fit_shuttle <- stan_glm(data = combine_data_stan,
                            formula = pick ~ shuttle - 1,
                            family = gaussian(),
                            refresh = 0)
       
       tbl_regression(fit_shuttle, intercept = FALSE) %>% 
         as_gt() })
     

       
    
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
