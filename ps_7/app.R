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

##Here I downloaded all necessary libraries. Below I read in the data and created "choices" for the variables I'm going to use for the app. 

all_data <- read_rds("ps_7_data.rds")

education_choices <- all_data %>% 
  group_by(educ) %>% 
  summarize()

location_choices <- all_data %>% 
  group_by(join) %>% 
  summarize()

ui <- fluidPage(  
                 
                 titlePanel("Effectiveness of Poll Predictions based on Voter Education"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(inputId = "join",
                                 label = "State & District:",
                                 choices = location_choices,
                                 selected = "AZ-06"),
                     selectInput(inputId = "education",
                                 label = "Education:",
                                 choices = education_choices,
                                 selected = "High school")
                   ),
                   
##Here I added a title and my desired inputs of District and Education which will show on my graphics. Below I made it to where 
## there would be two tabs, one for general information about my app and another with the actual graphics. I got this tabs idea from Ms. Lupion's app. 
                   
                   mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("About This App", 
                                          h1("Summary"),
                                          p("This app examines what the NYT Upshot polls got right and wrong about the vote based on voters' education. 
                                            Essentially, it measures the degree to which the polls were similar to the actual results via bar graphs."),
                                          p("You can examine the polled results vs. the actual results by district and level of education on the graphics tab"),
                                          p("To navigate, simply click on the drop down menu of each desired variable to select your desired observation.")),
                                 tabPanel("Graphic", plotOutput("plot")))
                                 )
                   )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    req(input$education, input$join)
    
##Here I am rendering the plot that I'd like to show by bringing in my desired outputs and manipulating the data so that I have education 
# and location. I then created a dataframe for my bar graph and used the ggplot function and new data frame to create the desired graphs. 
    
    data <- all_data %>% 
      filter(educ == input$education, join == input$join)
    
    bar_data <- data.frame(
      type = factor(c("Polled Result", "Polled Result", "Polled Result", "Actual Result", "Actual Result", "Actual Result")),
      party = factor(c("Republican", "Democratic", "Other", "Republican", "Democratic", "Other"),
                     levels=c("Republican", "Democratic", "Other")),
      percent = c(data$predicted_rep,
                  data$predicted_dem,
                  data$predicted_other,
                  data$real_rep, 
                  data$real_dem,
                  data$real_other)
    )
    
    ggplot(data = bar_data, aes(x = party, y = percent, fill = type)) +
      geom_bar(color = "black", stat = "identity", position = position_dodge()) +
      xlab("Party") + ylab("Percentage of Vote") +
      ggtitle("Percentage of Vote based on Education") +
      theme_classic()
  })
  
}
# Running the application here 
shinyApp(ui = ui, server = server)