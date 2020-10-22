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

data <- read_rds("data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cyborg"),
    navbarPage(
        "NFL Draft Combine Results",
        tabPanel("Testing Data",
                 titlePanel("Distribution of Vertical Jumps \n40 Yard Dash Times"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 100,
                                     value = 50)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("distPlot1"), 
                         plotOutput("distPlot2"),
                         plotOutput("distPlot3")
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
    ))

  

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$distPlot1 <- renderPlot({
      data %>% 
          ggplot(aes(x = vertical, fill = pos)) + 
            geom_histogram(bins = input$bins) +
            labs(x = "Vertical Jump (Inches)", 
                 y = "Number of Players", 
                 title = "Distribution of Vertical Jumps")
        
    })

    output$distPlot2 <- renderPlot({
        data %>% 
         ggplot(aes(x = x40yd, fill = pos)) + 
            geom_histogram(bins = input$bins) + 
            labs(x = "40 Yard Dash Time (Sec.)", 
                 y = "Number of Players",
                 title = "Distribution of 40 Yard Dashes")
    
    })
    
    output$distPlot3 <- renderPlot({
        data %>% 
            ggplot(aes(x = x40yd, y = vertical, color = pos)) + 
            geom_point() + 
            labs(x = "40 Yard Dash Time (Sec.)", 
                 y = "Vertical (Inches)",
                 title = "Relation of Vertical to 40 Yard Dash")
        
    })
    
}
# Run the application
shinyApp(ui = ui, server = server)
