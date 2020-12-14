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
                     ),
                    
                 ),
                 br(),
                 p("For the 40 yard dash, three cone drill, and shuttle,
                       lower times arw better. For the ertical and broad jump,
                       higher/farther jumps are better. Therefore, when you
                       plot a combination 40 yard dash, three cone drill, or 
                       shuttle together, a positive correlation means they have
                       a relationship in which doing better on one indicates doing 
                       better on another. When you plot broad jump and vertical 
                       together, a positive correlation again means they have a 
                       relationship in which doing better on one indicates doing 
                       better on another. However, when plotting one of 40 yard
                       dash, shuttle, or three cone drill with one of broad jump 
                       or vertical, a negative correlation will actually mean a 
                       relationship in which doing better on one indicates doing 
                       better on another."),
                 
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
                                   "Bench Reps" = "bench_reps"))),
                   
                   # This slider allows you to select which event you want to 
                   # look at and analyze.
                   
                   mainPanel(
                     plotOutput("distPlot2")
                   ),
                   ),
                 br(),
                 p("This graph shows how much a specific test infleunces draft
                   position. It is important to note that a lower draft position
                   indicated a player getting drafted earlier and is thus better.
                   For 40 yard dash, shuttle, and three cone drill, since
                   lower times are better as well, a positive correlation means 
                   that doing better on these tests improves draft position. For
                   broad jump and vertical jump, since a higher/farther jump
                   is better, a negative correlation indicates that doing
                   better on these tests improves draft position. For bench 
                   press, since more reps is better, a negative correlation 
                   indicated that performing better on this test improves draft
                   position."),
                 
                 
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
                                   "Bench Reps" = "bench_reps"))),
                   
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
        
        tabPanel("Understanding the Tests",
                 titlePanel("How to contextualize the results"),
                 br(),
                 h4("Understanding the Tests"),
                 p("On the plots that you find on the different tabs, you will
                 see analysis related to different tests taken at the NFL 
                 Combine. It is important to put these tests in proper context. 
                 Below you will find descriptions of the tests that are 
                 analyzed in this project."),
                 br(),
                 p("40 YARD DASH: This test evaluates the straight-line 
                   speed of an individual. Prospects are timed in a 40 yard
                   sprint, with what constitutes good and bad times varying by 
                   position. Overall, 40 yard dash times fall between the range
                   of about 4.20 seconds to about 5.3 seconds, with lower, and
                   thus faster, times being considered better."),
                 br(),
                 p("BROAD JUMP: This test evaluates the explosiveness of an
                   athlete by testing how far they can jump. Prospects start
                   standing behind a line and jump as far as possible, and the
                   distance they cover is reported in inches. The most explosive
                   athletes can cover over 132 inches (11 feet), while less 
                   explosive athletes may not reach 120 inches (10 feet). Again,
                   what constitutes a good broad jump and a bad broad jump 
                   varies by position"),
                 br(),
                 p("VERTICAL JUMP: This test evaluates the explosiveness of an
                   athlete by testing how high they can jump. Prospects start
                   standing in place and then jump as high as they can. The 
                   height they jump is measured and reported in inches, with 
                   very explosive athletes jumping in the upper 30 inch range.
                   Again, what constitutes a good vertical jump and a bad
                   vertical jump varies by position."),
                 br(),
                 p("SHUTTLE: This test evaluates an athletes ability to change
                   direction. Prospects start in a three point stance, with one
                   hand on the ground, and then sprint 5 yards to the right, touch 
                   a line, sprint 10 yards to the left, touch another 
                   line, and then sprint 5 yards to the right again, finsihing
                   through the line they started on. Athletes may also be tested
                   starting out sprinting to the left instead of the right. A 
                   very agile athlete may run a shuttle in the 4.0 to 4.1 second
                   range, while less agile athletes will be more towards the 4.5
                   to 4.6 and above range. Again, what constitutes a good shuttle
                   time and bad shuttle time varies by position."),
                 br(),
                 p("THREE CONE DRILL: This test evaluates an athletes ability to
                   change direction. Prospects start in a three point stance, with
                   one hand on the ground, and sprint around cones placed in an
                   L shape. A very agile athlete may run the three cone drill
                   in around 6.6 to 6.7 seconds, while less agile athletes will
                   be in the mid to upper 7 second range. Again, what constitutes
                   a good and bad three cone drill time varies by position."),
                 br(),
                 p("BENCH PRESS: This test evaluates the strength of an athlete.
                   The prospect performs a bench press, doing as many reps as
                   possible at 225 pounds. The strongest athletes may do over 30 
                   reps, while less strong athletes may not reach double digits.
                   Again, what constitutes a good bench press and a bad bench
                   press varies by position.")
        ),
        
        tabPanel("Understanding the Positions",
                 titlePanel("How to understand what each position means"),
                 br(),
                 h4("Understanding the Positions"),
                 p("In the different tabs, the data is usually broken down
                   by the different positions an athlete plays. This is due to
                   the fact that each position has different roles and, in turn,
                   has different expectation for performance levels in each of
                   the tests. To better understand what each position means and
                   the implication of the position on the test results, see the
                   descriptions below."),
                 br(),
                 p("QUARTERBACK: The quarterback position refers to the player
                   who recieves the ball at the start of each play, either 
                   handing it to a running back or throwing it to wide receivers.
                   Therefore, the results of the tests are usually less important
                   for quarterbacks, as their ability to throw a football is what
                   is important. Due to this, you find high variation of athleticism
                   and strength at the quarterback position, and usually these
                   players have average test results for all tests."),
                 br(),
                 p("OFFENSIVE TACKLE and OFFENSIVE GUARD: These 
                   positions make up the offensive line, responsible for blocking
                   opposing players. These players are larger, usually over 300 
                   pounds, therefore the speed and athleticism of these positions
                   are usually lower, but their strength is higher. Therefor, 
                   you can expect these players to make up some of the worst
                   speed and explosiveness test results (40 yard dash, etc.()),
                   but the best strength test results."),
                 br(),
                 p("WIDE RECEIVER: This position is responsible for running routes
                   and catching passses from the quarterback. Therefore, these 
                   players are usually very athletic and test at the top end of
                   almost all of the tests, except their strength may be 
                   average."),
                 p("TIGHT END and FULLBACK: These positions have both the roles of 
                 blocking andreceiving passes, depending on the situatioin. 
                 Therefore, these players are larger and contain the strength 
                 close to that of offensive linemen, but the speed, agility, and
                 explosiveness close to that of the wide receiver. Therefore, 
                   expect their test results across the board to be average."),
                 br(),
                 p("RUNNING BACK: This position is responsible for running the
                   ball. Therefore, the players must be able to withstand many
                   hits and are usually strong athletes, but also must have
                   good athleticism and explosiveness. There testing results 
                   are usually slightly worse than that of wide receivers, but
                   better than tight ends."),
                 br(),
                 p("DEFENSIVE END: This position is responsible for rushing the
                   quarterback and breaking through blocks of the offensive
                   linemen. These players are usually large and very strong,
                   but contain better athleticism than that of the offensive 
                   linemen. Expect their test results to be on the lower to 
                   average level."),
                 br(),
                 p("DEFENSIVE TACKLE: This position is resposnible for playing
                   on the interior of the defensivle line, and therefore must
                   be made up of very large players in order to stop the run. 
                   These players have the athleticism and strength similar to
                   that of an offensive linemen, so you can expect below average
                   test results in all except bench press."),
                 br(),
                 p("OUTSIDE LINEBACKER and INSIDE LINEBACKER: These players have
                   both the roles of stopping the run and the pass, and therefore
                   must be very strong and muscular, similar to a running back, 
                   but must also have the athleticism close to that of a wide
                   receiver. You can expect their test results to be average
                   to above average."),
                 br(),
                 p("SAFETY AND CORNERBACK: This position is responsible for
                   covering the wide receivers. Due to this, these players
                   must be the most athletic players on the field and therefor
                   usually make up the fastest and most explosive test
                   results."),
                 br(),
                 p("PUNTERS: This position is responsible for punting the ball, 
                   and therefore the test results have little impact on them
                   and are highly variable, but usually lower on athleticism
                   and strength.")
                 
                 
                 ),
        
        
        tabPanel("About",
                 titlePanel("About"),
                 br(),
                 h1("NFL Combine Background"),
                 p("Every year in February, the country's top college footballplayers attend the
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
                 plans to enter the medical field. I play quartback for the
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
         select(x40yd, vertical, bench_reps, broad_jump, x3cone, shuttle, Team, pos, pick) %>% 
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
